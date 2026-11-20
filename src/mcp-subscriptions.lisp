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
                        :documentation "Whether a pending cancellation may write to the transport.")
   (watch-signal :initarg :watch-signal :initform nil :reader mcp-subscription--watch-signal
                 :documentation "Internal tool watcher callback for metadata-only signals.")
   (watch-restore :initarg :watch-restore :initform nil :reader mcp-subscription--watch-restore
                  :documentation "Internal scope and capability check for stdio replacement.")
   (watch-generation :initform nil :accessor mcp-subscription--watch-generation
                     :documentation "Connection generation of the current watch exchange."))
  (:documentation "One request-scoped MCP subscription with a single consumer."))

(defparameter *mcp-subscription-list-events*
  '(("toolsListChanged" . "notifications/tools/list_changed")
    ("promptsListChanged" . "notifications/prompts/list_changed")
    ("resourcesListChanged" . "notifications/resources/list_changed"))
  "Mapping between modern filter keys and list-changed methods.")

(-> mcp-tool-watch-project-notification (t) (or null hash-table))
(defun mcp-tool-watch-project-notification (message)
  "Copy only tool watch metadata; return NIL for other notifications.
Use before enqueue or demultiplexing. The receiver checks the actual request ID."
  (when (and (hash-table-p message)
             (equal (json-get message "jsonrpc") "2.0")
             (not (nth-value 1 (gethash "id" message)))
             (not (nth-value 1 (gethash "result" message)))
             (not (nth-value 1 (gethash "error" message))))
    (let ((params (json-get message "params"))
          (method (json-get message "method")))
      (when (hash-table-p params)
        (let* ((subscription-id (mcp-subscription-wire-id params))
               (request-id      (json-get params "requestId"))
               (safe-params     (json-object)))
          (when (integerp subscription-id)
            (setf (gethash "_meta" safe-params)
                  (json-object "io.modelcontextprotocol/subscriptionId" subscription-id)))
          (cond
            ((and (integerp subscription-id) (equal method "notifications/tools/list_changed"))
             (mcp-client--notification "notifications/tools/list_changed" safe-params))
            ((and (integerp subscription-id) (equal method "notifications/subscriptions/acknowledged"))
             (let* ((filter (json-get params "notifications"))
                    (flag   (and (hash-table-p filter) (json-get filter "toolsListChanged"))))
               (setf (gethash "notifications" safe-params)
                     (if (json-boolean-p flag)
                         (json-object "toolsListChanged" flag)
                         (json-object)))
               (mcp-client--notification "notifications/subscriptions/acknowledged" safe-params)))
            ((and (integerp request-id) (equal method "notifications/cancelled")
                  (or (integerp subscription-id) (null subscription-id)))
             (setf (gethash "requestId" safe-params) request-id)
             (mcp-client--notification "notifications/cancelled" safe-params))))))))

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
        (let ((watch (mcp-subscription--watch-signal subscription)))
          (setf (mcp-subscription--status subscription) status
                (mcp-subscription--failure subscription) (unless watch failure)
                (mcp-subscription--wire-cancellation-p subscription) wire-p
                stop (and stop-p (mcp-subscription--stop subscription))
                (mcp-subscription--stop subscription) nil)
          (when watch (funcall watch ':terminal (mcp-subscription--watch-generation subscription)))
          (condition-notify (mcp-subscription--changed subscription)))))
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

(-> mcp-subscription--receive-watch (mcp-subscription hash-table function) boolean)
(defun mcp-subscription--receive-watch (subscription message signal-watch)
  "Consume safe correlated watch metadata without queuing messages or conditions."
  (let ((safe-message (mcp-tool-watch-project-notification message)))
    (when safe-message
      (let* ((method     (json-get safe-message "method"))
             (params     (json-get safe-message "params"))
             (wire-id    (mcp-subscription-wire-id params))
             (generation nil)
             (terminal   nil))
        (with-lock-held ((mcp-subscription--lock subscription))
          (when (mcp-subscription--status subscription)
            (return-from mcp-subscription--receive-watch nil))
          (let ((identifier (mcp-subscription-id subscription))
                (active-p   (mcp-subscription--acknowledged-p subscription)))
            (setf generation (mcp-subscription--watch-generation subscription))
            (cond
              ((equal method "notifications/cancelled")
               (when (and (eql identifier (json-get params "requestId"))
                          (or (null wire-id) (eql identifier wire-id)))
                 (setf terminal ':server-cancelled)))
              ((not (eql identifier wire-id))
               nil)
              ((equal method "notifications/subscriptions/acknowledged")
               (let ((filter (json-get params "notifications")))
                 (if (or active-p (not (json-true-p (json-get filter "toolsListChanged"))))
                     (setf terminal ':failed)
                     (progn
                       (setf (mcp-subscription--accepted subscription) filter
                             (mcp-subscription--acknowledged-p subscription) t)
                       (funcall signal-watch ':acknowledged generation)
                       (condition-notify (mcp-subscription--changed subscription))))))
              (active-p
               (funcall signal-watch ':changed generation))
              (t
               (setf terminal ':failed)))))
        (when terminal
          (mcp-subscription--terminate subscription terminal)
          (return-from mcp-subscription--receive-watch nil)))))
  t)

(-> mcp-subscription--receive (mcp-subscription hash-table) boolean)
(defun mcp-subscription--receive (subscription message)
  "Validate one correlated MESSAGE and queue it without running application code."
  (let ((watch (mcp-subscription--watch-signal subscription)))
    (when watch
      (return-from mcp-subscription--receive (mcp-subscription--receive-watch subscription message watch))))
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

(-> mcp-subscription--watch-capability-p (mcp-client) boolean)
(defun mcp-subscription--watch-capability-p (client)
  "Check the modern tools.listChanged grant before application projection."
  (let* ((capabilities (mcp-client-server-capabilities client))
         (tools        (and (hash-table-p capabilities) (json-get capabilities "tools"))))
    (and (equal (mcp-client-protocol-version client) *mcp-modern-protocol-version*)
         (hash-table-p tools)
         (json-true-p (json-get tools "listChanged")))))

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
      (mcp-client--connect-unlocked client)
      ;; Project replacement metadata before releasing the credential scope.
      (when (mcp-subscription--watch-signal subscription)
        (unless (funcall (mcp-subscription--watch-restore subscription) ':prepare
                         (mcp-subscription--watch-capability-p client))
          (return-from mcp-subscription--stdio-restore-unlocked nil))))
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
                 (flet ((start ()
                          (with-lock-held ((mcp-client-lock client))
                            (when (if failure
                                      (mcp-subscription--stdio-restore-unlocked client subscription generation)
                                      (and (= generation (mcp-client--connection-generation client))
                                           (with-lock-held ((mcp-subscription--lock subscription))
                                             (not (mcp-subscription--status subscription)))))
                              (setf generation (mcp-client--connection-generation client)
                                    exchange-generation generation
                                    deadline (mcp-stdio--deadline timeout))
                              (let ((watch (mcp-subscription--watch-signal subscription)))
                                (when watch
                                  (with-lock-held ((mcp-subscription--lock subscription))
                                    (setf (mcp-subscription--watch-generation subscription) generation
                                          (mcp-subscription--accepted subscription) nil)
                                    (funcall watch ':opening generation)))
                                (multiple-value-setq (identifier pending)
                                  (mcp-stdio--start-request
                                   transport (mcp-subscription--request-object client subscription)
                                   :deadline deadline :timeout timeout
                                   :watch-p (not (null watch))
                                   :receive (lambda (message) (mcp-subscription--receive subscription message)))))))))
                   (let ((restore (mcp-subscription--watch-restore subscription)))
                     (if (and failure restore)
                         (funcall restore ':restart #'start)
                         (start)))
                   (unless pending
                     (mcp-subscription--terminate subscription ':dropped :failure failure)
                     (return)))
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
                                                               :stop-p t :wire-p (null (mcp-subscription--watch-signal subscription)))
                     (mcp-transport-error () nil))
                   (return))
                 (mcp-transport-error (condition)
                    (setf failure (if (mcp-subscription--watch-signal subscription) t condition)))))
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
                 (flet ((receive (message) (mcp-subscription--receive subscription message))
                        (install (stop) (mcp-subscription--install-cancel subscription stop)))
                   (mcp-subscription--complete
                    subscription
                    (if (and (mcp-subscription--watch-signal subscription)
                             (typep transport 'mcp-streamable-http-transport))
                        (mcp-http--listen transport (mcp-subscription--request-object client subscription)
                                         timeout :receive #'receive :install-cancel #'install :watch-p t)
                        (mcp-transport-listen
                         transport (mcp-subscription--request-object client subscription) timeout
                         :receive #'receive :install-cancel #'install))))))
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

(-> mcp-subscription--start
    (mcp-client hash-table real &key (:watch-signal (or null function)) (:watch-restore (or null function)))
    mcp-subscription)
(defun mcp-subscription--start (client filter timeout &key watch-signal watch-restore)
  "Register an exchange on the connected CLIENT and start its worker."
  (let ((subscription nil))
    (with-lock-held ((mcp-client-lock client))
      (unless (and (mcp-client-connected-p client)
                   (equal (mcp-client-protocol-version client) *mcp-modern-protocol-version*))
        (error (mcp-subscription--protocol-error "subscriptions/listen requires MCP 2026-07-28.")))
      (let ((identifier (mcp-client--next-identifier-unlocked client))
            (generation (mcp-client--connection-generation client)))
        (setf subscription (make-instance 'mcp-subscription :identifier identifier :requested filter
                                          :watch-signal watch-signal :watch-restore watch-restore)
              (mcp-subscription--watch-generation subscription) (and watch-signal generation)
              (gethash subscription (mcp-client--subscriptions client)) t)
        (handler-case
            (make-thread (lambda () (mcp-subscription--run client subscription :generation generation :timeout timeout))
                         :name "mcparen subscription")
          (error (condition)
            (remhash subscription (mcp-client--subscriptions client))
            (error condition)))))
    subscription))

(-> mcp-client-listen (mcp-client hash-table &key (:timeout real)) mcp-subscription)
(defun mcp-client-listen (client notifications &key (timeout (mcp-client-startup-timeout client)))
  "Open a 2026-07-28 subscription and wait up to TIMEOUT seconds for acknowledgment."
  (unless (plusp timeout)
    (error (mcp-subscription--protocol-error "The acknowledgment timeout must be positive.")))
  (let* ((filter       (mcp-subscription--filter notifications))
         (subscription (progn
                         (mcp-client-connect client)
                         (mcp-subscription--start client filter timeout))))
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
