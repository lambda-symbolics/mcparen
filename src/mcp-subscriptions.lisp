(in-package #:mcparen)

;;;; -- Stateless Subscriptions --

(defclass mcp-subscription ()
  ((identifier :initarg :identifier :reader mcp-subscription-id :writer (setf mcp-subscription--id)
               :documentation "The wire identifier of the current subscription exchange.")
   (requested :initarg :requested :reader mcp-subscription--requested
              :documentation "The normalized requested filter.")
   (accepted :initform nil :accessor mcp-subscription--accepted
             :documentation "The most recently acknowledged filter, or NIL before the first acknowledgment.")
   (acknowledged-p :initform nil :accessor mcp-subscription--acknowledged-p
                   :documentation "True when the current exchange is acknowledged.")
   (lock :initform (make-lock "mcparen subscription") :reader mcp-subscription--lock
         :documentation "Serializes acknowledgment, queue and termination.")
   (changed :initform (make-condition-variable) :reader mcp-subscription--changed
            :documentation "Wakes the single consumer when the stream changes.")
   (events :initform nil :accessor mcp-subscription--events
           :documentation "Bounded FIFO of messages awaiting delivery.")
   (status :initform nil :accessor mcp-subscription--status
           :documentation "The first terminal outcome, or NIL while active.")
   (failure :initform nil :accessor mcp-subscription--failure
            :documentation "The diagnostic condition for a failed or dropped subscription.")
   (stop :initform nil :accessor mcp-subscription--stop
         :documentation "The installed transport cancellation function.")
   (wire-cancellation-p :initform nil :accessor mcp-subscription--wire-cancellation-p
                        :documentation "Whether a pending cancellation may write to the transport."))
  (:documentation "One request-scoped MCP subscription with a single consumer."))

(defparameter *mcp-subscription-list-events*
  '(("toolsListChanged" . "notifications/tools/list_changed")
    ("promptsListChanged" . "notifications/prompts/list_changed")
    ("resourcesListChanged" . "notifications/resources/list_changed"))
  "Mapping between modern filter keys and list-changed methods.")

(-> mcp-subscription--protocol-error (string &optional t) mcp-protocol-error)
(defun mcp-subscription--protocol-error (message &optional payload)
  "Construct a protocol failure concerning a subscription."
  (make-condition 'mcp-protocol-error :message message :method "subscriptions/listen"
                                     :payload payload))

(-> mcp-subscription--filter (t) hash-table)
(defun mcp-subscription--filter (filter)
  "Validate and copy a JSON subscription FILTER, retaining recognized keys only."
  (unless (hash-table-p filter)
    (error (mcp-subscription--protocol-error "An MCP subscription filter must be an object.")))
  (let ((copy (json-object)))
    (dolist (entry *mcp-subscription-list-events*)
      (multiple-value-bind (value present-p) (gethash (first entry) filter)
        (when present-p
          (unless (json-boolean-p value)
            (error (mcp-subscription--protocol-error "A subscription list-change flag must be boolean.")))
          (setf (gethash (first entry) copy) value))))
    (multiple-value-bind (uris present-p) (gethash "resourceSubscriptions" filter)
      (when present-p
        (unless (and (vectorp uris) (not (stringp uris))
                     (every (lambda (uri) (and (stringp uri) (plusp (length uri)))) uris))
          (error (mcp-subscription--protocol-error "Resource subscriptions need a vector of nonempty URIs.")))
        (setf (gethash "resourceSubscriptions" copy) (map 'vector #'copy-seq uris))))
    copy))

(-> mcp-subscription--subset-p (hash-table hash-table) boolean)
(defun mcp-subscription--subset-p (accepted requested)
  "Return true if ACCEPTED grants no events beyond REQUESTED."
  (and (every (lambda (entry)
                (let ((key (first entry)))
                  (or (not (json-true-p (json-get accepted key)))
                      (json-true-p (json-get requested key)))))
              *mcp-subscription-list-events*)
       (every (lambda (uri)
                (find uri (json-get requested "resourceSubscriptions") :test #'string=))
              (json-get accepted "resourceSubscriptions"))))

(-> mcp-subscription-acknowledged-filter (mcp-subscription) hash-table)
(defun mcp-subscription-acknowledged-filter (subscription)
  "Return a copy of the most recently acknowledged filter, which may be narrower than requested."
  (with-lock-held ((mcp-subscription--lock subscription))
    (mcp-subscription--filter (mcp-subscription--accepted subscription))))

(-> mcp-subscription--terminate (mcp-subscription keyword &key (:failure t) (:stop-p boolean) (:wire-p boolean)) null)
(defun mcp-subscription--terminate (subscription status &key failure stop-p wire-p)
  "Record first terminal STATUS and optionally stop the transport exchange."
  (let ((stop nil))
    (with-lock-held ((mcp-subscription--lock subscription))
      (unless (mcp-subscription--status subscription)
        (setf (mcp-subscription--status subscription) status
              (mcp-subscription--failure subscription) failure
              (mcp-subscription--wire-cancellation-p subscription) wire-p
              stop (and stop-p (mcp-subscription--stop subscription))
              (mcp-subscription--stop subscription) nil)
        (condition-notify (mcp-subscription--changed subscription))))
    (when stop
      (funcall stop wire-p)))
  nil)

(-> mcp-subscription-cancel (mcp-subscription) null)
(defun mcp-subscription-cancel (subscription)
  "Cancel SUBSCRIPTION once, using its binding's cancellation mechanism."
  (mcp-subscription--terminate subscription ':cancelled :stop-p t :wire-p t))

(defmethod mcp-client--stop-subscription ((subscription mcp-subscription))
  "Wake the consumer and stop the exchange during client teardown."
  (mcp-subscription--terminate subscription ':dropped :stop-p t))

(-> mcp-subscription--install-cancel (mcp-subscription function) null)
(defun mcp-subscription--install-cancel (subscription stop)
  "Install the transport's STOP function, honoring cancellation during open."
  (multiple-value-bind (terminated-p wire-p)
      (with-lock-held ((mcp-subscription--lock subscription))
        (if (mcp-subscription--status subscription)
            (values t (mcp-subscription--wire-cancellation-p subscription))
            (progn
              (setf (mcp-subscription--stop subscription) stop)
              (values nil nil))))
    (when terminated-p
      (funcall stop wire-p)))
  nil)

(-> mcp-subscription--allowed-event-p (hash-table string hash-table) boolean)
(defun mcp-subscription--allowed-event-p (filter method params)
  "Check METHOD against accepted subscription FILTER."
  (cond
    ((string= method "notifications/resources/updated")
     (let ((uri (json-get params "uri")))
       (and (stringp uri)
            (not (null (find uri (json-get filter "resourceSubscriptions") :test #'string=))))))
    (t
     (let ((entry (rassoc method *mcp-subscription-list-events* :test #'string=)))
       (and entry (json-true-p (json-get filter (first entry))))))))

(-> mcp-subscription--receive (mcp-subscription hash-table) boolean)
(defun mcp-subscription--receive (subscription message)
  "Validate one correlated MESSAGE and queue it without running application code."
  (handler-case
      (with-lock-held ((mcp-subscription--lock subscription))
        (let* ((params     (json-get message "params"))
               (method     (json-get message "method"))
               (identifier (mcp-subscription-id subscription))
               (wire-id    (mcp-subscription-wire-id params))
               (accepted   (and (mcp-subscription--acknowledged-p subscription)
                                (mcp-subscription--accepted subscription)))
               (server-cancel-p
                 (and (string= method "notifications/cancelled")
                      (hash-table-p params)
                      (equal identifier (json-get params "requestId")))))
          (unless (mcp-subscription--status subscription)
            (cond
              ((not (or (equal identifier wire-id) (and server-cancel-p (null wire-id))))
               (error (mcp-subscription--protocol-error "Wrong or missing MCP subscription ID." message)))
              ((string= method "notifications/subscriptions/acknowledged")
               (let ((filter (mcp-subscription--filter (json-get params "notifications"))))
                 (when (or accepted (not (mcp-subscription--subset-p
                                          filter (mcp-subscription--requested subscription))))
                   (error (mcp-subscription--protocol-error "Invalid MCP subscription acknowledgment." message)))
                 (setf (mcp-subscription--accepted subscription) filter
                       (mcp-subscription--acknowledged-p subscription) t)))
              ((not accepted)
               (error (mcp-subscription--protocol-error "An event preceded its subscription acknowledgment." message)))
              (server-cancel-p
               (setf (mcp-subscription--status subscription) ':server-cancelled))
              ((not (mcp-subscription--allowed-event-p accepted method params))
               (error (mcp-subscription--protocol-error "An event is outside its acknowledged filter." message)))
              ((>= (length (mcp-subscription--events subscription)) 128)
               (error (mcp-subscription--protocol-error "The MCP subscription event queue is full.")))
              (t
               (setf (mcp-subscription--events subscription)
                     (nconc (mcp-subscription--events subscription) (list message)))))
            (condition-notify (mcp-subscription--changed subscription))
            (not server-cancel-p))))
    (mcp-protocol-error (condition)
      (mcp-subscription--terminate subscription ':failed :failure condition)
      nil)))

(-> mcp-subscription--exchange-acknowledged-p (mcp-subscription) boolean)
(defun mcp-subscription--exchange-acknowledged-p (subscription)
  "Return whether the current exchange has received its acknowledgment."
  (with-lock-held ((mcp-subscription--lock subscription))
    (mcp-subscription--acknowledged-p subscription)))

(-> mcp-subscription--complete (mcp-subscription (or null hash-table)) null)
(defun mcp-subscription--complete (subscription response)
  "Validate a normal end to the current exchange and retain its outcome."
  (if response
      (let* ((identifier (mcp-subscription-id subscription))
             (result     (mcp-client--validate-response response identifier "subscriptions/listen")))
        (unless (and (hash-table-p result)
                     (equal (json-get result "resultType") "complete")
                     (equal identifier (mcp-subscription-wire-id result))
                     (mcp-subscription--exchange-acknowledged-p subscription))
          (error (mcp-subscription--protocol-error "Invalid subscription completion." result)))
        (mcp-subscription--terminate subscription ':complete))
      (mcp-subscription--terminate subscription ':dropped)))

(-> mcp-subscription--request-object (mcp-client mcp-subscription) hash-table)
(defun mcp-subscription--request-object (client subscription)
  "Build the request for SUBSCRIPTION's current exchange."
  (mcp-client--request-object
   client "subscriptions/listen" (mcp-subscription-id subscription)
   :params (json-object "notifications" (mcp-subscription--requested subscription))))

(-> mcp-subscription--stdio-restore-unlocked (mcp-client mcp-subscription integer) boolean)
(defun mcp-subscription--stdio-restore-unlocked (client subscription generation)
  "Restart the lost stdio GENERATION using the client's lifecycle lock, or join its replacement."
  (unless (and (mcp-client-connected-p client)
               (equal (mcp-client-protocol-version client) *mcp-modern-protocol-version*)
               (with-lock-held ((mcp-subscription--lock subscription))
                 (and (mcp-subscription--acknowledged-p subscription)
                      (not (mcp-subscription--status subscription)))))
    (return-from mcp-subscription--stdio-restore-unlocked nil))
  (let ((transport (mcp-client-transport client)))
    (when (= generation (mcp-client--connection-generation client))
      (when (mcp-transport-open-p transport)
        (return-from mcp-subscription--stdio-restore-unlocked nil))
      (setf (mcp-client-connected-p client) nil)
      (mcp-client--connect-unlocked client))
    (and (equal (mcp-client-protocol-version client) *mcp-modern-protocol-version*)
         (mcp-transport-open-p transport)
         (with-lock-held ((mcp-subscription--lock subscription))
           (unless (mcp-subscription--status subscription)
             (setf (mcp-subscription--id subscription) (mcp-client--next-identifier-unlocked client)
                   (mcp-subscription--acknowledged-p subscription) nil
                   (mcp-subscription--stop subscription) nil)
             t)))))

(-> mcp-subscription--run-stdio (mcp-client mcp-subscription mcp-stdio-transport
                                 &key (:generation integer) (:timeout real)) null)
(defun mcp-subscription--run-stdio (client subscription transport &key generation timeout)
  "Reissue an acknowledged stdio exchange after loss; never replay an unacknowledged one."
  (let ((failure nil))
    (loop
      (let ((deadline            nil)
            (exchange-generation generation)
            (identifier          nil)
            (pending             nil))
        (unwind-protect
             (progn
               ;; Serialize the write, not the wait, with close and process replacement.
               (with-lock-held ((mcp-client-lock client))
                 (unless (if failure
                             (mcp-subscription--stdio-restore-unlocked client subscription generation)
                             (and (= generation (mcp-client--connection-generation client))
                                  (with-lock-held ((mcp-subscription--lock subscription))
                                    (not (mcp-subscription--status subscription)))))
                   (mcp-subscription--terminate subscription ':dropped :failure failure)
                   (return))
                 (setf generation (mcp-client--connection-generation client)
                       exchange-generation generation
                       deadline (mcp-stdio--deadline timeout))
                 (multiple-value-setq (identifier pending)
                   (mcp-stdio--start-request
                    transport (mcp-subscription--request-object client subscription)
                    :deadline deadline :timeout timeout
                    :receive (lambda (message) (mcp-subscription--receive subscription message)))))
               (mcp-subscription--install-cancel
                subscription
                (lambda (wire-p)
                  (if wire-p
                      (with-lock-held ((mcp-client-lock client))
                        (mcp-stdio--cancel-request
                         transport identifier pending
                         :timeout timeout
                         :wire-p  (and (= exchange-generation (mcp-client--connection-generation client))
                                       (mcp-transport-open-p transport))))
                      (mcp-stdio--cancel-request transport identifier pending :timeout timeout))))
               (handler-case
                   (let ((response
                           (mcp-stdio--take-response
                            transport identifier pending :deadline deadline :timeout timeout
                            :acknowledged-p (lambda () (mcp-subscription--exchange-acknowledged-p subscription)))))
                     (mcp-subscription--complete subscription response)
                     (return))
                 (mcp-timeout (condition)
                   (handler-case (mcp-subscription--terminate subscription ':dropped :failure condition
                                                              :stop-p t :wire-p t)
                     (mcp-transport-error () nil))
                   (return))
                 (mcp-transport-error (condition)
                   (setf failure condition))))
          (when pending
            (mcp-stdio--discard-request transport identifier pending))))))
  nil)

(-> mcp-subscription--run (mcp-client mcp-subscription &key (:generation integer) (:timeout real)) null)
(defun mcp-subscription--run (client subscription &key generation timeout)
  "Run the long-lived exchange and retain the first terminal outcome."
  (unwind-protect
       (handler-case
           (let ((transport (mcp-client-transport client)))
             (if (typep transport 'mcp-stdio-transport)
                 (mcp-subscription--run-stdio client subscription transport :generation generation :timeout timeout)
                 (mcp-subscription--complete
                  subscription
                  (mcp-transport-listen
                   transport (mcp-subscription--request-object client subscription) timeout
                   :receive        (lambda (message) (mcp-subscription--receive subscription message))
                   :install-cancel (lambda (stop) (mcp-subscription--install-cancel subscription stop))))))
         (mcp-transport-error (condition)
           (mcp-subscription--terminate subscription ':dropped :failure condition))
         (error (condition)
           (mcp-subscription--terminate subscription ':failed :failure condition)))
    (with-lock-held ((mcp-client-lock client))
      (remhash subscription (mcp-client--subscriptions client))))
  nil)

(-> mcp-subscription--wait (mcp-subscription (or null real) boolean) (values t keyword t))
(defun mcp-subscription--wait (subscription timeout acknowledgment-p)
  "Wait for an acknowledgment or the next event, with an optional deadline."
  (let ((deadline (when timeout (+ (get-internal-real-time)
                                   (ceiling (* timeout internal-time-units-per-second))))))
    (with-lock-held ((mcp-subscription--lock subscription))
      (loop
        (when (and acknowledgment-p (mcp-subscription--accepted subscription))
          (return (values nil ':acknowledged nil)))
        (when (and (not acknowledgment-p) (mcp-subscription--events subscription))
          (return (values (pop (mcp-subscription--events subscription)) ':notification nil)))
        (let ((status (mcp-subscription--status subscription)))
          (when status
            (return (values nil status (mcp-subscription--failure subscription)))))
        (let ((remaining (when deadline (/ (- deadline (get-internal-real-time))
                                           internal-time-units-per-second))))
          (when (and remaining (not (plusp remaining)))
            (return (values nil ':timeout nil)))
          (condition-wait (mcp-subscription--changed subscription)
                          (mcp-subscription--lock subscription)
                          :timeout remaining))))))

(-> mcp-subscription-next (mcp-subscription &key (:timeout (or null real))) (values t keyword t))
(defun mcp-subscription-next (subscription &key timeout)
  "Return a message, outcome keyword and optional failure; use one consumer per subscription."
  (mcp-subscription--wait subscription timeout nil))

(-> mcp-client-listen (mcp-client hash-table &key (:timeout real)) mcp-subscription)
(defun mcp-client-listen (client notifications &key (timeout (mcp-client-startup-timeout client)))
  "Open a 2026-07-28 subscription and wait up to TIMEOUT seconds for acknowledgment."
  (unless (plusp timeout)
    (error (mcp-subscription--protocol-error "The acknowledgment timeout must be positive.")))
  (let ((filter       (mcp-subscription--filter notifications))
        (subscription nil))
    (mcp-client-connect client)
    (with-lock-held ((mcp-client-lock client))
      (unless (equal (mcp-client-protocol-version client) *mcp-modern-protocol-version*)
        (error (mcp-subscription--protocol-error "subscriptions/listen requires MCP 2026-07-28.")))
      (let ((identifier (mcp-client--next-identifier-unlocked client))
            (generation (mcp-client--connection-generation client)))
        (setf subscription (make-instance 'mcp-subscription :identifier identifier :requested filter)
              (gethash subscription (mcp-client--subscriptions client)) t)
        (handler-case
            (make-thread (lambda () (mcp-subscription--run client subscription :generation generation :timeout timeout))
                         :name "mcparen subscription")
          (error (condition)
            (remhash subscription (mcp-client--subscriptions client))
            (error condition)))))
    (multiple-value-bind (message status failure) (mcp-subscription--wait subscription timeout t)
      (declare (ignore message))
      (case status
        (:acknowledged
         subscription)
        (:timeout
         (handler-case (mcp-subscription-cancel subscription)
           (mcp-transport-error () nil))
         (error 'mcp-timeout :message "The MCP subscription acknowledgment timed out."
                             :transport (mcp-client-transport client) :cause nil
                             :operation "subscriptions/listen" :seconds timeout))
        (otherwise
         (error (or failure (mcp-subscription--protocol-error
                             "The MCP subscription ended before acknowledgment."))))))))
