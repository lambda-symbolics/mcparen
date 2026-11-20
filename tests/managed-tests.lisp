(in-package #:mcparen)

;;;; -- Managed Connection Fixtures --

(-> test-managed-server
    (string &key (:required-p boolean) (:tools-function (or null function))
                 (:server-class symbol) (:initargs list))
    (values mcp-managed-server hash-table))
(defun test-managed-server (name &key required-p tools-function
                                     (server-class 'mcp-managed-server) initargs)
  "Return a managed scripted server and its request counters as two values."
  (let ((counts (make-hash-table :test #'equal)))
    (values
     (apply
      #'make-instance server-class :name name :required-p required-p
      :client-factory
      (lambda (server notification-handler)
        (declare (ignore server notification-handler))
        (make-mcp-client
         (make-test-scripted-transport
          (lambda (transport request)
            (declare (ignore transport))
            (let ((method (json-get request "method")))
              (incf (gethash method counts 0))
              (cond
                ((string= method "initialize")
                 (test-rpc-result request (test-initialize-result)))
                ((string= method "tools/list")
                 (test-rpc-result
                  request
                  (json-object "tools"
                               (map 'vector #'mcp-tool-raw
                                    (if tools-function (funcall tools-function)
                                        (list (test-tool name)))))))
                ((string= method "ping")
                 (test-rpc-result request (json-object)))
                (t
                 (error "Unexpected managed fixture method ~S." method))))))))
      initargs)
     counts)))

(defclass test-changing-managed-server (mcp-managed-server)
  ((changes :initarg :changes :initform 0 :accessor test-managed-changes
            :documentation "Remaining transparent reconnections during discovery."))
  (:documentation "A server that changes sessions after returning a tool page."))

(defmethod mcp-managed-prepare-tools :around
    ((server test-changing-managed-server) tools &key allocated-schema-bytes)
  "Replace the session after parsing a page, before managed publication."
  (declare (ignore tools allocated-schema-bytes))
  (multiple-value-prog1 (call-next-method)
    (when (plusp (test-managed-changes server))
      (decf (test-managed-changes server))
      (mcp-client-close (mcp-server-runtime-client server))
      (mcp-client-connect (mcp-server-runtime-client server)))))

(defclass test-cleanup-managed-server (mcp-managed-server)
  ((scope-failure :initarg :scope-failure :initform nil :accessor test-managed-scope-failure
                  :documentation "Whether credential scope entry must fail.")
   (local-cleanup-function :initform nil :accessor test-managed-local-cleanup-function
                           :documentation "Optional replacement for the local cleanup scope."))
  (:documentation "A connection whose injected cleanup credentials may be unavailable."))

(define-condition test-managed-cleanup-abort (serious-condition) ()
  (:documentation "An injected non-error cleanup scope exit."))

(defmethod mcp-managed-call-with-cleanup ((server test-cleanup-managed-server) function)
  "Model credential scope errors and nonlocal exits before local cleanup."
  (block nil
    (case (test-managed-scope-failure server)
      (:throw
       (throw 'test-managed-cleanup :aborted))
      (:serious
       (error 'test-managed-cleanup-abort))
      (:skip
       (return nil))
      ((nil)
       nil)
      (otherwise
       (error "Credential resolver unavailable.")))
    (funcall function)))

(defmethod mcp-managed-call-with-local-cleanup
    ((server test-cleanup-managed-server) function cause)
  "Inject local scope-entry failure independently of the cleanup callback."
  (if (test-managed-local-cleanup-function server)
      (funcall (test-managed-local-cleanup-function server) function cause)
      (call-next-method)))

(defclass test-observed-cleanup-server (test-cleanup-managed-server)
  ((observer :initarg :observer :reader test-managed-cleanup-observer
             :documentation "The observer called before entering the cleanup scope."))
  (:documentation "A managed connection with observable teardown ordering."))

(defmethod mcp-managed-call-with-cleanup :before
    ((server test-observed-cleanup-server) function)
  "Observe scope entry before the cleanup callback or its local fallback."
  (declare (ignore function))
  (funcall (test-managed-cleanup-observer server) server))


;;;; -- Migrated Lifecycle and Discovery Cases --

(define-test managed-stable-generation-and-bounded-churn
  (dolist (changes '(1 8))
    (multiple-value-bind (server counts)
        (test-managed-server "generation" :server-class 'test-changing-managed-server
                                           :initargs    (list :changes changes))
      (unwind-protect
           (let ((*mcp-tool-discovery-restart-limit* 3))
             (if (= changes 1)
                 (progn
                   (mcp-server-runtime-connect server)
                   (test-equal 2 (gethash "tools/list" counts))
                   (let ((snapshot (mcp-server-runtime-snapshot server)))
                     (test-equal :ready (mcp-discovery-snapshot-state snapshot))
                     (test-equal (mcp-client-connection-generation (mcp-server-runtime-client server))
                                 (mcp-discovery-snapshot-generation snapshot))))
                 (progn
                   (test-signals mcp-managed-server-error (mcp-server-runtime-connect server))
                   (test-equal 3 (gethash "tools/list" counts))
                   (test-equal :failed (mcp-server-runtime-state server))
                   (test-assert (null (mcp-server-runtime-tools server)))
                   (test-assert (not (mcp-client-connected-p (mcp-server-runtime-client server)))))))
        (mcp-server-runtime-close server)))))

(define-test managed-notifications-during-discovery
  (let ((server nil) (notify-p t))
    (setf server
          (test-managed-server
           "notify" :tools-function
           (lambda ()
             (when notify-p
               (setf notify-p nil)
               (mcp-server-runtime-request-tool-refresh server))
             (list (test-tool "notified")))))
    (unwind-protect
         (progn
           (mcp-server-runtime-connect server)
           (test-assert (mcp-server-runtime-tools-stale-p server))
           (let ((revision (mcp-server-runtime-tools-revision server)))
             (mcp-server-runtime-connect server)
             (test-assert (> (mcp-server-runtime-tools-revision server) revision))
             (test-assert (not (mcp-server-runtime-tools-stale-p server)))))
      (mcp-server-runtime-close server))))

(define-test managed-transparent-reconnect-and-detached-snapshot
  (multiple-value-bind (server counts) (test-managed-server "snapshot")
    (unwind-protect
         (progn
           (mcp-server-runtime-connect server)
           (let* ((snapshot   (mcp-server-runtime-snapshot server))
                  (schema     (mcp-tool-input-schema (first (mcp-discovery-snapshot-tools snapshot))))
                  (generation (mcp-discovery-snapshot-generation snapshot)))
             (setf (gethash "type" schema) "mutated")
             (test-equal "object"
                         (gethash "type" (mcp-tool-input-schema (first (mcp-server-runtime-tools server)))))
             (mcp-client-close (mcp-server-runtime-client server))
             (mcp-client-connect (mcp-server-runtime-client server))
             (test-assert (mcp-server-runtime-tools-stale-p server))
             (mcp-server-runtime-connect server)
             (test-equal 2 (gethash "tools/list" counts))
             (test-assert (< generation (mcp-server-runtime-observed-connection-generation server)))
             (test-equal generation (mcp-discovery-snapshot-generation snapshot))
             (test-equal :ready (mcp-discovery-snapshot-state snapshot))))
      (mcp-server-runtime-close server))))

(define-test managed-required-failure-isolation-and-retry
  (let* ((fail-p   nil)
         (required (test-managed-server
                    "required"
                    :required-p     t
                    :tools-function (lambda ()
                                      (when fail-p (error "Injected discovery failure."))
                                      (list (test-tool "required")))))
         (healthy  (test-managed-server "healthy"))
         (manager  (make-instance 'mcp-connection-manager :runtimes (list required healthy))))
    (unwind-protect
         (progn
           (mcp-manager-start manager)
           (setf fail-p t)
           (mcp-server-runtime-request-tool-refresh required)
           (let ((first (test-signals mcp-managed-server-error (mcp-server-runtime-connect required))))
             (mcp-server-runtime-connect healthy)
             (let ((cached (test-signals mcp-managed-server-error (mcp-server-runtime-connect required))))
               (test-equal (mcp-error-message first) (mcp-error-message cached))))
           (test-equal :ready (mcp-server-runtime-state healthy))
           (test-equal :failed (mcp-server-runtime-state required))
           (setf fail-p nil)
           (test-signals mcp-managed-server-error (mcp-server-runtime-connect required))
           (mcp-manager-refresh manager :server-name "required")
           (test-equal :ready (mcp-server-runtime-state required)))
      (mcp-manager-close manager))))

(define-test managed-required-first-schema-budget
  (let* ((optional (test-managed-server "optional"))
         (required (test-managed-server "required" :required-p t))
         (bytes    (length (babel:string-to-octets (json-encode (mcp-tool-input-schema (test-tool "x")))
                                                  :encoding :utf-8)))
         (manager  (make-instance 'mcp-connection-manager
                                  :runtimes             (list optional required)
                                  :maximum-schema-bytes bytes)))
    (unwind-protect
         (progn
           (mcp-manager-start manager)
           (test-equal (list optional required) (mcp-manager-runtimes manager))
           (test-equal :ready (mcp-server-runtime-state required))
           (test-equal :failed (mcp-server-runtime-state optional))
           (test-equal bytes (mcp-server-runtime-tool-schema-bytes required))
           (test-equal 0 (mcp-server-runtime-tool-schema-bytes optional)))
      (mcp-manager-close manager))))

(define-test managed-budget-growth-preempts-cached-optional-server
  (let* ((grow-p   nil)
         (required (test-managed-server
                    "required"
                    :required-p     t
                    :tools-function (lambda () (if grow-p (list (test-tool "one") (test-tool "two"))
                                                   (list (test-tool "one"))))))
         (optional (test-managed-server "optional"))
         (bytes    (length (babel:string-to-octets (json-encode (mcp-tool-input-schema (test-tool "x")))
                                                  :encoding :utf-8)))
         (manager  (make-instance 'mcp-connection-manager
                                  :runtimes             (list optional required)
                                  :maximum-schema-bytes (* 2 bytes))))
    (unwind-protect
         (progn
           (mcp-manager-start manager)
           (test-equal :ready (mcp-server-runtime-state optional))
           (setf grow-p t)
           (mcp-manager-refresh manager :server-name "required")
           (test-equal (* 2 bytes) (mcp-server-runtime-tool-schema-bytes required))
           (test-equal :failed (mcp-server-runtime-state optional))
           (test-assert (not (mcp-client-connected-p (mcp-server-runtime-client optional)))))
      (mcp-manager-close manager))))

(define-test managed-duplicate-tools-and-qualified-aggregation
  (let* ((duplicate (test-managed-server "duplicate"
                       :tools-function (lambda () (list (test-tool "same") (test-tool "same")))))
         (first     (test-managed-server "first"))
         (second    (test-managed-server "second"))
         (manager   (make-instance 'mcp-connection-manager :runtimes (list duplicate first second))))
    (unwind-protect
         (progn
           (mcp-manager-start manager)
           (test-equal :failed (mcp-server-runtime-state duplicate))
           (multiple-value-bind (results failures)
               (mcp-manager-collect manager
                                    :list-function (lambda (client) (declare (ignore client))
                                                     (list (json-object "name" "shared")))
                                    :item-key      (lambda (item) (gethash "name" item)))
             (test-equal (list first second) (mapcar #'first results))
             (test-equal (list duplicate) (mapcar #'first failures)))
           (multiple-value-bind (results failures)
               (mcp-manager-collect manager
                                    :server-name   "first"
                                    :list-function (lambda (client) (declare (ignore client))
                                                     '("duplicate" "duplicate"))
                                    :item-key      #'identity)
             (test-assert (null results))
             (test-equal (list first) (mapcar #'first failures))))
      (mcp-manager-close manager))))

(define-test managed-single-owner-and-atomic-identity-validation
  (let* ((first  (test-managed-server "same"))
         (second (test-managed-server "same")))
    (test-signals mcp-managed-server-error
      (make-instance 'mcp-connection-manager :runtimes (list first second)))
    (test-assert (null (mcp-server-runtime-manager first)))
    (let ((owner (make-instance 'mcp-connection-manager :runtimes (list first))))
      (unwind-protect
           (progn
             (test-signals mcp-managed-server-error
               (make-instance 'mcp-connection-manager :runtimes (list first)))
             (test-assert (eq owner (mcp-server-runtime-manager first))))
        (mcp-manager-close owner)))
    (mcp-server-runtime-close second)))

(define-test managed-partial-startup-and-factory-failure-cleanup
  (dolist (failure-kind '(:factory :startup))
    (let ((ready (test-managed-server "ready" :required-p t)))
      (test-signals error
        (mcp-manager-build
         (list (lambda () ready)
               (lambda ()
                 (if (eq failure-kind :factory) (error "Factory failed.")
                     (test-managed-server "failed"
                                          :required-p     t
                                          :tools-function (lambda () (error "Startup failed."))))))))
      (test-assert (not (mcp-client-connected-p (mcp-server-runtime-client ready))))
      (test-equal :disconnected (mcp-server-runtime-state ready)))))

(define-test managed-cleanup-with-unavailable-credentials
  (dolist (operation (list #'mcp-server-runtime-close #'mcp-server-runtime-detach))
    (let* ((server    (test-managed-server "cleanup" :server-class 'test-cleanup-managed-server))
           (transport (mcp-client-transport (mcp-server-runtime-client server))))
      (mcp-server-runtime-connect server)
      (setf (test-managed-scope-failure server) t)
      (funcall operation server)
      (test-assert (not (mcp-transport-open-p transport)))
      (test-assert (null (mcp-server-runtime-tools server)))
      (test-assert (null (mcp-server-runtime-observed-connection-generation server)))
      (setf (test-managed-scope-failure server) nil)
      (mcp-server-runtime-connect server)
      (test-equal :ready (mcp-server-runtime-state server))
      (mcp-server-runtime-close server))))

(define-test managed-cancellation-and-reconnect
  (let* ((transport (make-test-stdio-transport))
         (server    (make-instance 'mcp-managed-server
                                   :name   "cancel"
                                   :client (make-mcp-client transport :startup-timeout 3)))
         (request   nil))
    (unwind-protect
         (progn
           (mcp-server-runtime-connect server)
           (setf request
                 (test-start-thread
                  (lambda ()
                    (mcp-server-runtime-call
                     server
                     (lambda (client)
                       (mcp-client-call-tool client (test-tool "never") (json-object)))))
                  "managed cancellation"))
           (test-assert
            (test-wait-until
             (lambda ()
               (search "received:never" (mcp-stdio-transport-stderr-text transport))) 1))
           (mcp-server-runtime-cancel server)
           (test-assert (test-wait-until
                         (lambda () (test-thread-result-finished-p request)) 2))
           (test-assert (test-thread-result-condition request))
           (join-thread (test-thread-result-thread request))
           (test-equal :disconnected (mcp-server-runtime-state server))
           (mcp-server-runtime-connect server)
           (test-equal :ready (mcp-server-runtime-state server)))
      (mcp-server-runtime-close server))))

(define-test managed-concurrent-close
 "Test parallel MCP shutdown and deterministic close-order failure reporting."
 (let ((lock     (make-lock "MCP close test"))
       (entered  0)
       (observed (make-hash-table :test #'eq)))
   (mcp-manager--close-runtimes '(:first :second)
    (lambda (runtime)
      (with-lock-held (lock)
        (incf entered))
      (let ((peer-seen-p
             (loop repeat 100
                   thereis (with-lock-held (lock)
                             (= entered 2))
                   do (sleep 0.005))))
        (with-lock-held (lock)
          (setf (gethash runtime observed) peer-seen-p)))))
   (test-assert (and (gethash :first observed) (gethash :second observed))
    "MCP manager closes independent server runtimes concurrently"))
 (let ((failure
        (handler-case
         (progn
          (mcp-manager--close-runtimes '(:second :first)
           (lambda (runtime) (error "close ~A" runtime)))
          nil)
         (simple-error (condition) (princ-to-string condition)))))
   (test-assert (and failure (search "SECOND" failure))
    "concurrent MCP close reports the first failure in close order"))
 nil)

(define-test managed-close-owns-workers-during-fallback-exit
  (dolist (later-exit-p '(nil t))
    (let* ((original-make-thread (symbol-function 'make-thread))
           (release              (sb-thread:make-semaphore))
           (lock                 (make-lock "Managed close observations"))
           (counts               (make-hash-table :test #'equal))
           (ownership-observed   nil)
           (workers              nil)
           (launches             0)
           (manager              nil)
           (servers
             (loop for name in '("worker-one" "worker-two" "abort" "remaining")
                   collect
                   (test-managed-server
                    name :server-class 'test-observed-cleanup-server
                    :initargs
                    (list :scope-failure (when (string= name "abort") :throw)
                          :observer
                          (lambda (server)
                            (let ((name (mcp-server-runtime-name server)))
                              (with-lock-held (lock)
                                (incf (gethash name counts 0)))
                              (cond
                                ((search "worker-" name)
                                 (let* ((released-p
                                          (sb-thread:wait-on-semaphore release :timeout 3))
                                        (acquired-p
                                          (bordeaux-threads:acquire-lock
                                           (mcp-manager-lock manager) nil)))
                                   (when acquired-p
                                     (bordeaux-threads:release-lock (mcp-manager-lock manager)))
                                   (with-lock-held (lock)
                                     (push (and released-p (not acquired-p)) ownership-observed))))
                                ((string= name "remaining")
                                 (sb-thread:signal-semaphore release 2)
                                 (when later-exit-p
                                   (throw 'test-managed-cleanup :later-abort)))))))))))
      (setf manager (make-instance 'mcp-connection-manager :runtimes (reverse servers)))
      (unwind-protect
           (progn
             (setf (symbol-function 'make-thread)
                   (lambda (function &key name)
                     (when (= (incf launches) 3)
                       (error "Injected close worker creation failure."))
                     (let ((worker (funcall original-make-thread function :name name)))
                       (push worker workers)
                       worker)))
             (test-equal :aborted
                         (catch 'test-managed-cleanup (mcp-manager-close manager)))
             (test-assert (every (lambda (worker) (not (thread-alive-p worker))) workers))
             (with-lock-held (lock)
               (test-equal '(t t) ownership-observed)
               (dolist (server servers)
                 (test-equal 1 (gethash (mcp-server-runtime-name server) counts)))))
        (setf (symbol-function 'make-thread) original-make-thread)
        (sb-thread:signal-semaphore release 2)
        (dolist (worker workers) (join-thread worker))))))

(define-test managed-close-joins-workers-after-join-failure
  (let ((original-join-thread (symbol-function 'join-thread))
        (failure              (make-condition 'simple-error :format-control "Injected join failure."))
        (joined               0))
    (unwind-protect
         (progn
           (setf (symbol-function 'join-thread)
                 (lambda (thread)
                   (funcall original-join-thread thread)
                   (when (= (incf joined) 1)
                     (error failure))))
           (test-assert
            (eq failure
                (handler-case
                    (mcp-manager--close-runtimes '(:first :second :third)
                                                (lambda (runtime) (declare (ignore runtime))))
                  (error (condition) condition))))
           (test-equal 3 joined))
      (setf (symbol-function 'join-thread) original-join-thread))))

(defclass test-rotating-managed-server (mcp-managed-server)
  ((identity :initform "first" :accessor test-managed-identity
             :documentation "The non-secret identity of the current credential snapshot.")
   (missing-p :initform nil :accessor test-managed-missing-p
              :documentation "Whether the injected credential snapshot is unavailable."))
  (:documentation "A persistent connection with replaceable credential identity."))

(defmethod mcp-managed-credential-check-p ((server test-rotating-managed-server))
  "Require persistent credential validation on every use boundary."
  t)

(defmethod mcp-managed-credential-key ((server test-rotating-managed-server))
  "Return a fake identity without retaining credential values."
  (values (test-managed-identity server)
          (when (test-managed-missing-p server)
            (make-condition 'simple-error :format-control "Missing credential snapshot."))))

(define-test managed-credential-rotation-and-missing-snapshot
  (multiple-value-bind (server counts)
      (test-managed-server "credentials" :server-class 'test-rotating-managed-server)
    (unwind-protect
         (progn
           (mcp-server-runtime-connect server)
           (mcp-server-runtime-connect server)
           (test-equal 1 (gethash "initialize" counts))
           (setf (test-managed-identity server) "second")
           (mcp-server-runtime-connect server)
           (test-equal 2 (gethash "initialize" counts))
           (test-equal "second" (mcp-server-runtime-launch-environment-fingerprint server))
           (setf (test-managed-missing-p server) t)
           (test-signals mcp-managed-server-error (mcp-server-runtime-connect server))
           (test-assert (not (mcp-transport-open-p (mcp-client-transport (mcp-server-runtime-client server)))))
           (test-assert (null (mcp-server-runtime-launch-environment-fingerprint server)))
           (setf (test-managed-missing-p server) nil)
           (mcp-server-runtime-request-tool-refresh server)
           (mcp-server-runtime-connect server)
           (test-equal 3 (gethash "initialize" counts)))
      (mcp-server-runtime-close server))))

(define-test managed-teardown-serializes-with-discovery
  (dolist (operation (list #'mcp-manager-close #'mcp-manager-detach))
    (let ((entered-p nil) (release-p nil) (lock (make-lock "managed teardown race")))
      (let* ((server
               (test-managed-server
                "closing" :tools-function
                (lambda ()
                  (with-lock-held (lock) (setf entered-p t))
                  (unless (test-wait-until (lambda () (with-lock-held (lock) release-p)) 3)
                    (error "Discovery barrier timed out."))
                  (list (test-tool "closing")))))
             (manager   (make-instance 'mcp-connection-manager :runtimes (list server)))
             (discovery (test-start-thread (lambda () (mcp-manager-start manager))
                                           "managed discovery"))
             (closing   nil))
        (unwind-protect
             (progn
               (test-assert (test-wait-until (lambda () (with-lock-held (lock) entered-p)) 2))
               (setf closing (test-start-thread (lambda () (funcall operation manager))
                                                "managed teardown"))
               (with-lock-held (lock) (setf release-p t))
               (test-await-thread discovery 3)
               (test-await-thread closing 3)
               (test-equal (if (eq operation #'mcp-manager-close) :disconnected :detached)
                           (mcp-server-runtime-state server))
               (test-assert (not (mcp-client-connected-p (mcp-server-runtime-client server)))))
          (with-lock-held (lock) (setf release-p t))
          (mcp-manager-close manager))))))

(define-test managed-cleanup-exactly-once-on-scope-exits
  (dolist (failure '(nil :error :serious :throw))
    (let ((server (test-managed-server "scope-exit"
                                      :server-class 'test-cleanup-managed-server))
          (calls  0))
      (setf (test-managed-scope-failure server) failure)
      (flet ((cleanup () (incf calls)))
        (case failure
          (:serious
           (test-signals test-managed-cleanup-abort
             (mcp-managed-call-with-cleanup server #'cleanup)))
          (:throw
           (test-equal :aborted
                       (catch 'test-managed-cleanup
                         (mcp-managed-call-with-cleanup server #'cleanup))))
          (otherwise
           (mcp-managed-call-with-cleanup server #'cleanup))))
      (test-equal 1 calls)))
  (let ((server (test-managed-server "callback-failure")) (calls 0))
    (test-signals simple-error
      (mcp-managed-call-with-cleanup server
       (lambda () (incf calls) (error "Local cleanup failed."))))
    (test-equal 1 calls)))

(define-test managed-cleanup-preserves-scope-exits
  (dolist (scope-failure '(:throw :serious))
    (dolist (cleanup-failure '(:error :serious :throw))
      (let ((server (test-managed-server "scope-exit"
                                        :server-class 'test-cleanup-managed-server))
            (calls  0))
        (setf (test-managed-scope-failure server) scope-failure)
        (let ((outcome
                (handler-case
                    (catch 'test-managed-cleanup
                      (mcp-managed-call-with-cleanup
                       server
                       (lambda ()
                         (incf calls)
                         (ecase cleanup-failure
                           (:error
                            (error "Local cleanup failed."))
                           (:serious
                            (error 'serious-condition))
                           (:throw
                            (throw 'test-managed-cleanup :cleanup-aborted))))))
                  (serious-condition (condition) condition))))
          (if (eq scope-failure :throw)
              (test-equal :aborted outcome)
              (test-assert (typep outcome 'test-managed-cleanup-abort)))
          (test-equal 1 calls))))))

(define-test managed-cleanup-local-scope-failure
  (dolist (scope-failure '(:skip t :throw :serious))
    (let ((server   (test-managed-server "local-scope"
                                        :server-class 'test-cleanup-managed-server))
          (failure  (make-condition 'simple-error :format-control "Local scope failed."))
          (attempts 0)
          (calls    0))
      (setf (test-managed-scope-failure server) scope-failure
            (test-managed-local-cleanup-function server)
            (lambda (function cause)
              (declare (ignore function cause))
              (incf attempts)
              (error failure)))
      (let ((outcome
              (handler-case
                  (catch 'test-managed-cleanup
                    (mcp-managed-call-with-cleanup server (lambda () (incf calls))))
                (serious-condition (condition) condition))))
        (case scope-failure
          (:throw
           (test-equal :aborted outcome))
          (:serious
           (test-assert (typep outcome 'test-managed-cleanup-abort)))
          (otherwise
           (test-assert (eq failure outcome))))
        (test-equal 1 attempts)
        (test-equal 0 calls)))))

(define-test managed-stdio-cleanup-with-unavailable-credentials
  (let* ((transport (make-test-stdio-transport))
         (server    (make-instance 'test-cleanup-managed-server
                                   :name   "stdio-cleanup"
                                   :client (make-mcp-client transport)))
         (process   nil))
    (unwind-protect
         (progn
           (mcp-server-runtime-connect server)
           (setf process (mcp-stdio-transport-process transport)
                 (test-managed-scope-failure server) t)
           (test-assert (uiop:process-alive-p process))
           (mcp-server-runtime-close server)
           (test-assert (not (mcp-transport-open-p transport)))
           (test-assert (null (mcp-stdio-transport-process transport)))
           (test-assert (not (ignore-errors (uiop:process-alive-p process)))))
      (mcp-server-runtime-close server))))

(define-test managed-http-listener-cleanup-with-unavailable-credentials
  (let* ((transport (make-mcp-streamable-http-transport
                     "http://127.0.0.1:9/mcp"
                     :headers-function (lambda () (error "Credentials unavailable."))))
         (server    (make-instance 'test-cleanup-managed-server
                                   :name   "http-cleanup"
                                   :client (make-mcp-client transport)))
         (listener  nil))
    (unwind-protect
         (progn
           (mcp-transport-open transport)
           (setf (mcp-http-transport-session-identifier transport) "session"
                 listener (make-thread
                           (lambda ()
                             (loop until (mcp-http-transport-listener-stopping-p transport)
                                   do (sleep 0.01)))
                           :name "managed HTTP cleanup")
                 (mcp-http-transport-listener-thread transport) listener
                 (test-managed-scope-failure server) t)
           (mcp-server-runtime-close server)
           (test-assert (not (mcp-transport-open-p transport)))
           (test-assert (null (mcp-http-transport-listener-thread transport)))
           (test-assert (not (thread-alive-p listener))))
      (mcp-server-runtime-close server))))

(define-test managed-nonlocal-startup-cleanup
  (dolist (failure-kind '(:factory :discovery))
    (let ((ready (test-managed-server "ready")))
      (mcp-server-runtime-connect ready)
      (test-equal :aborted
        (catch 'test-managed-startup
          (mcp-manager-build
           (list (lambda () ready)
                 (lambda ()
                   (if (eq failure-kind :factory)
                       (throw 'test-managed-startup :aborted)
                       (test-managed-server
                        "aborted" :tools-function
                        (lambda () (throw 'test-managed-startup :aborted)))))))))
      (test-assert (not (mcp-client-connected-p (mcp-server-runtime-client ready))))
      (test-equal :disconnected (mcp-server-runtime-state ready)))))

(defclass test-tool-watch-server (mcp-managed-server) ()
  (:documentation "Managed fixture that erases capabilities after watch sampling."))

(defvar *test-tool-watch-scope-p* nil
  "True only inside the current thread's fresh managed scope.")

(defmethod mcp-managed-call-with-scope ((server test-tool-watch-server) function)
  "Bind a transient launch marker, including during background replacement."
  (let ((*test-tool-watch-scope-p* t))
    (call-next-method server function)))

(defmethod mcp-managed-prepare-client ((server test-tool-watch-server))
  "Erase capability details, as a secret-capable consumer would."
  (test-assert *test-tool-watch-scope-p*)
  (let ((capabilities (mcp-client-server-capabilities (mcp-server-runtime-client server))))
    (when (hash-table-p (json-get capabilities "tools"))
      (setf (gethash "tools" capabilities) (json-object)))))

(-> test-tool-watch-stdio (string &key (:opt-in-p boolean) (:timeout real))
    (values test-tool-watch-server mcp-client))
(defun test-tool-watch-stdio (mode &key (opt-in-p t) (timeout 3))
  "Make a real credential-scoped stdio fixture with the notification projection seam."
  (let* ((transport (make-mcp-stdio-transport
                     (namestring sb-ext:*runtime-pathname*)
                     :arguments            (append (test-fixture-script-arguments "stdio-server.lisp")
                                                   (list "persistent" mode))
                     :environment-function (lambda ()
                                             (test-assert *test-tool-watch-scope-p*)
                                             nil)
                     :ingress-projector    (lambda (kind value)
                                             (if (eq kind :notification)
                                                 (mcp-tool-watch-project-notification value)
                                                 value))))
         (client    (make-mcp-client transport :protocol-mode ':auto :startup-timeout timeout)))
    (values (apply #'make-instance 'test-tool-watch-server :name "watch" :client client
                   (when opt-in-p (list :watch-tool-changes-p t)))
            client)))

(-> test-tool-watch-command (mcp-client string) hash-table)
(defun test-tool-watch-command (client method)
  "Run one local fixture command, without resubmitting on replacement."
  (mcp-client--request client method :timeout 3))

(define-test managed-tool-watch-stdio-opt-in-and-capability
  (dolist (case '(("true" nil 0) ("true" t 1) ("false" t 0) ("missing" t 0) ("invalid" t 0)))
    (destructuring-bind (mode opt-in-p expected) case
      (multiple-value-bind (runtime client) (test-tool-watch-stdio mode :opt-in-p opt-in-p)
        (declare (ignore client))
        (unwind-protect
             (progn
               (mcp-server-runtime-connect runtime)
               (test-equal (format nil "listens-~D" expected)
                           (mcp-tool-name (first (mcp-server-runtime-tools runtime))))
               (test-assert (not (mcp-server-runtime-tools-stale-p runtime))))
          (mcp-server-runtime-close runtime))))))

(define-test managed-tool-watch-stdio-denial-and-bounded-ack
  (dolist (mode '("denied" "malformed" "silent"))
    (multiple-value-bind (runtime client) (test-tool-watch-stdio mode :timeout 2)
      (unwind-protect
           (ls-compat:with-timeout 8
             (mcp-server-runtime-connect runtime)
             (test-equal "listens-1" (mcp-tool-name (first (mcp-server-runtime-tools runtime))))
             (dotimes (index 3)
               (mcp-server-runtime-request-tool-refresh runtime)
               (mcp-server-runtime-connect runtime))
             (test-equal 1 (json-get (test-tool-watch-command client "fixture/subscription-state") "listens"))
             (test-assert (not (mcp-server-runtime-tools-stale-p runtime))))
        (mcp-server-runtime-close runtime)))))

(define-test managed-tool-watch-stdio-replacement-change-and-rearm
  (multiple-value-bind (runtime client) (test-tool-watch-stdio "true")
    (unwind-protect
         (progn
           (mcp-server-runtime-connect runtime)
           (let* ((initial      (mcp-managed--tool-watch runtime))
                  (subscription (mcp-managed-tool-watch-subscription initial))
                  (id           (mcp-subscription-id subscription)))
             (test-tool-watch-command client "fixture/subscription-event")
             (test-assert (mcp-server-runtime-tools-stale-p runtime))
             (mcp-server-runtime-connect runtime)
             (test-signals mcp-transport-error
               (test-tool-watch-command client "fixture/exit-unexpectedly"))
             (test-assert (test-wait-until
                           (lambda ()
                             (with-lock-held ((mcp-subscription--lock subscription))
                               (and (/= id (mcp-subscription-id subscription))
                                    (mcp-subscription--acknowledged-p subscription)))) 5))
             (mcp-server-runtime-connect runtime)
             (test-assert (eq initial (mcp-managed--tool-watch runtime)))
             (test-equal 1 (json-get (test-tool-watch-command client "fixture/subscription-state") "listens"))
             (test-tool-watch-command client "fixture/cancel-subscription")
             (test-assert (mcp-server-runtime-tools-stale-p runtime))
             (test-equal ':server-cancelled (nth-value 1 (mcp-subscription-next subscription :timeout 1)))
             (test-assert (null (mcp-subscription--failure subscription)))
             (test-equal 1 (json-get (test-tool-watch-command client "fixture/subscription-state") "listens"))
             (mcp-server-runtime-connect runtime)
             (test-equal "listens-2" (mcp-tool-name (first (mcp-server-runtime-tools runtime))))
             (test-assert (not (mcp-server-runtime-tools-stale-p runtime)))
             (mcp-managed--signal-tool-watch runtime initial ':changed :generation (mcp-client-connection-generation client))
             (test-assert (not (mcp-server-runtime-tools-stale-p runtime)))
             (mcp-server-runtime-detach runtime)
             (test-assert (null (mcp-managed--tool-watch runtime)))))
      (mcp-server-runtime-close runtime))))

(define-test managed-tool-watch-http-scope-and-rearm
  (let ((lock     (make-lock "tool watch HTTP fixture"))
        (scope    (make-instance 'test-http-exchange-scope))
        (listens  0)
        (commands nil))
    (labels ((enqueue (command)
               "Ask the first open stream to send a fixture event."
               (with-lock-held (lock) (push command commands)))

             (take-command ()
               "Read the next fixture command, if any."
               (with-lock-held (lock) (pop commands)))

             (send-event (stream identifier method params)
               "Write one SSE event with extra data that must not survive receipt."
               (setf (gethash "_meta" params) (test-subscription-meta identifier)
                     (gethash "extra" params) "fixture-only")
               (write-string (test-sse-body (mcp-client--notification method params)) stream)
               (finish-output stream))

             (respond (server exchange)
               "Serve discovery, tools and the long-lived watch on separate local connections."
               (declare (ignore server))
               (let* ((request    (json-decode (test-http-request-body exchange)))
                      (method     (json-get request "method"))
                      (identifier (json-get request "id"))
                      (headers    (list (cons "Content-Type" "application/json"))))
                 (test-equal "fixture-auth" (rest (assoc "authorization" (test-http-request-headers exchange)
                                                        :test #'string=)))
                 (cond
                   ((equal method "server/discover")
                    (values 200 headers (test-json-response-body
                                         request (json-object "resultType" "complete"
                                                              "supportedVersions" #("2026-07-28")
                                                              "capabilities" (json-object "tools" (json-object "listChanged" t))))))
                   ((equal method "tools/list")
                    (values 200 headers (test-json-response-body
                                         request (json-object "resultType" "complete" "tools"
                                                              (vector (json-object
                                                                       "name" (format nil "listens-~D" (with-lock-held (lock) listens))
                                                                       "inputSchema" (json-object "type" "object")))))))
                   ((equal method "subscriptions/listen")
                    (let ((sequence (with-lock-held (lock) (incf listens))))
                      (values 200 (list (cons "Content-Type" "text/event-stream"))
                              (make-test-http-streaming-body
                               (lambda (stream)
                                 (send-event stream (1+ identifier) "notifications/subscriptions/acknowledged"
                                             (json-object "notifications" (json-object "toolsListChanged" t)))
                                 (send-event stream identifier "notifications/subscriptions/acknowledged"
                                             (json-object "notifications"
                                                          (json-object "toolsListChanged" (if (= sequence 1) t (json-decode "false")))))
                                 (when (= sequence 1)
                                   (loop repeat 2
                                         do (let ((command nil))
                                              (test-assert (test-wait-until (lambda () (setf command (take-command))) 5))
                                              (ecase command
                                                (:change
                                                 (send-event stream (1+ identifier) "notifications/tools/list_changed" (json-object))
                                                 (send-event stream identifier "notifications/tools/list_changed" (json-object)))
                                                (:cancel
                                                 (send-event stream identifier "notifications/cancelled"
                                                             (json-object "requestId" identifier "reason" "fixture-only"))))))))))))
                   (t
                    (error "Unexpected fixture HTTP method ~S" method))))))
      (with-test-http-server (server #'respond)
        (let* ((transport (make-mcp-streamable-http-transport
                           (test-http-server-url server)
                           :exchange-scope-function (test-http-exchange-scope-function scope)
                           :headers-function        (lambda ()
                                                      (test-assert (test-http-exchange-scope-active-p scope))
                                                      (list (cons "Authorization" "fixture-auth")))))
               (client    (make-mcp-client transport :protocol-mode ':auto :startup-timeout 2))
               (runtime   (make-instance 'test-tool-watch-server :name "http watch" :client client :watch-tool-changes-p t)))
          (unwind-protect
               (progn
                 (mcp-server-runtime-connect runtime)
                 (test-equal "listens-1" (mcp-tool-name (first (mcp-server-runtime-tools runtime))))
                 (test-assert (not (test-http-exchange-scope-active-p scope)))
                 (let* ((subscription (mcp-managed-tool-watch-subscription (mcp-managed--tool-watch runtime)))
                        (version      (mcp-server-runtime-tools-change-version runtime)))
                   (enqueue ':change)
                   (test-assert (test-wait-until (lambda () (mcp-server-runtime-tools-stale-p runtime)) 2))
                   (test-equal (1+ version) (mcp-server-runtime-tools-change-version runtime))
                   (test-assert (null (mcp-subscription--events subscription)))
                   (mcp-server-runtime-connect runtime)
                   (enqueue ':cancel)
                   (test-equal ':server-cancelled (nth-value 1 (mcp-subscription-next subscription :timeout 2)))
                   (test-assert (null (mcp-subscription--failure subscription)))
                   (test-equal 1 (with-lock-held (lock) listens))
                   (mcp-server-runtime-connect runtime)
                   (test-equal "listens-2" (mcp-tool-name (first (mcp-server-runtime-tools runtime))))
                   (dotimes (index 2)
                     (mcp-server-runtime-request-tool-refresh runtime)
                     (mcp-server-runtime-connect runtime))
                   (test-equal 2 (with-lock-held (lock) listens))
                   (test-assert (not (test-http-exchange-scope-active-p scope)))))
            (mcp-server-runtime-close runtime)))))))
