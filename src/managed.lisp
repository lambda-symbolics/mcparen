(in-package #:mcparen)

;;;; -- Managed Connection Protocol --

(defvar *mcp-managed-ownership-lock* (make-lock "MCP lifecycle ownership")
  "Serializes validation and attachment of servers to lifecycle owners.")

(define-condition mcp-managed-server-error (mcp-error)
  ((server-name :initarg :server-name :reader mcp-managed-error-server-name
                :documentation "The stable server identity.")
   (required-p :initarg :required-p :reader mcp-managed-error-required-p
               :documentation "Whether startup requires this server.")
   (cause :initarg :cause :initform nil :reader mcp-managed-error-cause
          :documentation "The underlying failure, when safe to retain."))
  (:documentation "A managed server failed to connect or publish discovery."))

(define-condition mcp-managed-budget-exceeded (mcp-managed-server-error)
  ((resource :initarg :resource :reader mcp-managed-budget-resource
             :documentation "The exhausted discovery resource.")
   (allocated :initarg :allocated :reader mcp-managed-budget-allocated
              :documentation "The allocation to earlier servers.")
   (requested :initarg :requested :reader mcp-managed-budget-requested
              :documentation "The allocation requested by this server.")
   (limit :initarg :limit :reader mcp-managed-budget-limit
          :documentation "The manager-wide bound."))
  (:documentation "Discovery exceeded a shared manager budget."))

(defparameter *mcp-maximum-retained-input-schema-bytes* (* 8 1024 1024)
  "Default aggregate encoded input schema budget for managed discovery.")
(defparameter *mcp-tool-discovery-restart-limit* 8
  "Maximum attempts to discover tools within one connection generation.")

(defgeneric mcp-managed-call-with-scope (server function)
  (:documentation "Invoke FUNCTION synchronously inside SERVER's credential scope."))
(defgeneric mcp-managed-call-with-cleanup (server function)
  (:documentation "Invoke cleanup FUNCTION even when credential resolution fails."))
(defgeneric mcp-managed-prepare-client (server)
  (:documentation "Project retained server-controlled metadata inside the active scope."))
(defgeneric mcp-managed-reset-client (server)
  (:documentation "Forget retained connection metadata and credential identity."))
(defgeneric mcp-managed-check-credentials (server)
  (:documentation "Invalidate a connection after credential rotation, under its lock."))
(defgeneric mcp-managed-credential-check-p (server)
  (:documentation "Return true when a ready connection needs credential checks."))
(defgeneric mcp-managed-prepare-tools (server tools &key allocated-schema-bytes)
  (:documentation "Return validated TOOLS and their encoded input schema byte count."))
(defgeneric mcp-managed-error (server cause &optional message)
  (:documentation "Signal a structured discovery failure with application-safe details."))
(defgeneric mcp-managed-budget-error (server &key resource allocated requested limit)
  (:documentation "Signal an aggregate discovery budget failure."))
(defgeneric mcp-managed-failure (server cause)
  (:documentation "Return a bounded diagnostic safe to retain outside a secret scope."))
(defgeneric mcp-managed-cached-error (server)
  (:documentation "Reconstruct a structured failure from SERVER's retained diagnostic."))

(defun mcp-managed--bounded-string (value &key (limit 1000))
  "Return at most LIMIT characters from VALUE."
  (subseq value 0 (min limit (length value))))


(defclass mcp-managed-server ()
  ((name :initarg :name :reader mcp-server-runtime-name :type string
         :documentation "The stable, case-sensitive server identity.")
   (required-p :initarg :required-p :initform nil :reader mcp-managed-required-p
               :type boolean :documentation "Whether startup requires this server.")
   (client :initarg :client :reader mcp-server-runtime-client :type mcp-client
           :documentation "The shared thread-safe client.")
   (client-factory :initarg :client-factory :initform nil
                   :reader mcp-managed-client-factory
                   :documentation "Optional function of server and notification handler.")
   (lock :initform (make-lock "MCP managed server") :reader mcp-server-runtime-lock
         :documentation "Serializes discovery and status publication.")
   (state :initform ':disconnected :accessor mcp-server-runtime-state :type keyword
          :documentation "The disconnected, connecting, ready, failed or detached state.")
   (failure :initform nil :accessor mcp-server-runtime-failure :type (or null string)
            :documentation "The last bounded, credential-safe diagnostic.")
   (capabilities :initform nil :accessor mcp-server-runtime-capabilities
                 :documentation "Capabilities covered by the published generation.")
   (tools :initform nil :accessor mcp-server-runtime-tools :type list
          :documentation "The last complete tool discovery result.")
   (tool-schema-bytes :initform 0 :accessor mcp-server-runtime-tool-schema-bytes
                      :type (integer 0) :documentation "Retained input schema bytes.")
   (manager :initform nil :accessor mcp-server-runtime-manager
            :documentation "The sole lifecycle owner, shared by application registries.")
   (launch-environment-fingerprint
    :initform nil :accessor mcp-server-runtime-launch-environment-fingerprint
    :documentation "Optional non-secret credential identity.")
   (observed-connection-generation
    :initform nil :accessor mcp-server-runtime-observed-connection-generation
    :documentation "The connection generation of published discovery.")
   (tools-change-version :initform 0 :accessor mcp-server-runtime-tools-change-version
                         :type (integer 0) :documentation "Latest requested version.")
   (tools-change-lock :initform (make-lock "MCP discovery notifications")
                      :reader mcp-server-runtime-tools-change-lock
                      :documentation "Independent lock for reentrant notifications.")
   (tools-discovered-version
    :initform 0 :accessor mcp-server-runtime-tools-discovered-version
    :type (integer 0) :documentation "Version covered by the last attempt.")
   (tools-revision :initform 0 :accessor mcp-server-runtime-tools-revision
                   :type (integer 0) :documentation "Monotonic discovery revision."))
  (:documentation "A restartable client with synchronized discovery and cached failures."))

(defclass mcp-connection-manager ()
  ((runtimes :initarg :runtimes :initform nil :reader mcp-manager-runtimes :type list
             :documentation "Owned servers in stable presentation order.")
   (maximum-schema-bytes :initarg :maximum-schema-bytes :initform nil
                         :reader mcp-manager-maximum-schema-bytes
                         :documentation "Aggregate schema bound, or NIL for the dynamic default.")
   (lock :initform (make-lock "MCP connection manager") :reader mcp-manager-lock
         :documentation "Serializes discovery and application reconciliation."))
  (:documentation "One lifecycle owner and discovery budget for a set of shared clients."))


(-> mcp-server-runtime--connection-current-p (mcp-managed-server) boolean)


(-> mcp-server-runtime-tools-stale-p (mcp-managed-server) boolean)


(-> mcp-server-runtime-request-tool-refresh (mcp-managed-server) (integer 1))


(-> mcp-server-runtime--discover-tools-stably
    (mcp-managed-server &key (:allocated-schema-bytes (integer 0)))
    (values list (integer 0) (integer 0)))


(-> mcp-server-runtime--connect
    (mcp-managed-server &key (:allocated-schema-bytes (integer 0)))
    (values mcp-managed-server boolean))


(-> mcp-server-runtime-close (mcp-managed-server) null)


(-> mcp-server-runtime-detach (mcp-managed-server) null)


(-> mcp-manager--close-runtimes (list function) null)


(-> mcp-manager-close (mcp-connection-manager) null)


(-> mcp-manager-detach (mcp-connection-manager) null)


(-> mcp-manager-runtime (mcp-connection-manager string) (or null mcp-managed-server))


(-> mcp-manager--ordered-runtimes (mcp-connection-manager) list)


(-> mcp-server-runtime--mark-failed (mcp-managed-server mcp-managed-server-error)
    boolean)


(-> mcp-server-runtime--budget-usage (mcp-managed-server) (integer 0))


(-> mcp-manager--runtime-failure-barrier-p
    (mcp-managed-server (or null mcp-managed-server) boolean) boolean)


(-> mcp-server-runtime--manager-connect-p
    (mcp-managed-server (or null mcp-managed-server)) boolean)


(-> mcp-manager--connect-runtimes
    (mcp-connection-manager &key (:target-runtime (or null mcp-managed-server))
     (:signal-target-failure-p boolean))
    boolean)


(-> mcp-server-runtime-connect (mcp-managed-server)
    (values mcp-managed-server boolean))


(-> mcp-server-runtime--discard-connection (mcp-managed-server keyword) null)


(-> mcp-server-runtime--capability-p (mcp-managed-server string) boolean)


(defun mcp-server-runtime--connection-current-p (runtime)
  "Return true when RUNTIME's tool snapshot covers its live connection."
  (let* ((client (mcp-server-runtime-client runtime))
         (observed (mcp-server-runtime-observed-connection-generation runtime)))
    (and observed (mcp-client-connected-p client)
         (mcp-transport-open-p (mcp-client-transport client))
         (= observed (mcp-client-connection-generation client)) t)))


(defun mcp-server-runtime-tools-stale-p (runtime)
  "Return true when RUNTIME needs tool rediscovery or reconciliation."
  (with-lock-held ((mcp-server-runtime-lock runtime))
    (or
     (< (mcp-server-runtime-tools-discovered-version runtime)
        (with-lock-held ((mcp-server-runtime-tools-change-lock runtime))
          (mcp-server-runtime-tools-change-version runtime)))
     (and (eq (mcp-server-runtime-state runtime) :ready)
          (not (mcp-server-runtime--connection-current-p runtime))))))


(defun mcp-server-runtime-request-tool-refresh (runtime)
  "Advance RUNTIME's requested tool discovery version."
  (with-lock-held ((mcp-server-runtime-tools-change-lock runtime))
    (incf (mcp-server-runtime-tools-change-version runtime))))


(defun mcp-server-runtime--discover-tools-stably
    (runtime &key (allocated-schema-bytes 0))
  "Discover RUNTIME's tools within one stable client connection generation."
  (let ((client (mcp-server-runtime-client runtime)))
    (loop repeat *mcp-tool-discovery-restart-limit*
          do (mcp-client-connect client)
             (mcp-managed-prepare-client runtime)
             (let ((initial-generation (mcp-client-connection-generation client)))
               (multiple-value-bind (tools schema-bytes)
                   (if (mcp-server-runtime--capability-p runtime "tools")
                       (mcp-managed-prepare-tools
                        runtime (mcp-client-list-tools client)
                        :allocated-schema-bytes allocated-schema-bytes)
                       (values nil 0))
                 (let ((final-generation (mcp-client-connection-generation client)))
                   (when (= initial-generation final-generation)
                     (return (values tools final-generation schema-bytes))))))
          finally
             (mcp-managed-error
              runtime nil
              (format nil "MCP server ~A changed connections during ~D consecutive tool discovery attempts."
                      (mcp-server-runtime-name runtime)
                      *mcp-tool-discovery-restart-limit*)))))


(defun mcp-server-runtime--connect (runtime &key (allocated-schema-bytes 0))
  "Initialize RUNTIME within aggregate budgets and report snapshot publication."
  (let ((discovery-p nil))
    (with-lock-held ((mcp-server-runtime-lock runtime))
      (let ((target-version
             (with-lock-held ((mcp-server-runtime-tools-change-lock runtime))
               (mcp-server-runtime-tools-change-version runtime))))
        (handler-case
         (mcp-managed-call-with-scope runtime
          (lambda ()
            (mcp-managed-check-credentials runtime)
            (unless
                (and (eq (mcp-server-runtime-state runtime) :ready)
                     (mcp-server-runtime--connection-current-p runtime)
                     (>= (mcp-server-runtime-tools-discovered-version runtime)
                         target-version))
              (setf (mcp-server-runtime-state runtime) :connecting
                    (mcp-server-runtime-failure runtime) nil)
              (multiple-value-bind (tools generation schema-bytes)
                  (mcp-server-runtime--discover-tools-stably runtime
                   :allocated-schema-bytes allocated-schema-bytes)
                (setf (mcp-server-runtime-capabilities runtime)
                        (mcp-managed--copy-value
                         (mcp-client-server-capabilities
                          (mcp-server-runtime-client runtime)))
                      (mcp-server-runtime-tools runtime) tools
                      (mcp-server-runtime-tool-schema-bytes runtime) schema-bytes
                      (mcp-server-runtime-observed-connection-generation runtime)
                        generation
                      (mcp-server-runtime-tools-discovered-version runtime)
                        target-version
                      (mcp-server-runtime-state runtime) :ready
                      discovery-p t)
                (incf (mcp-server-runtime-tools-revision runtime))))))
         (mcp-managed-server-error (cause)
          (handler-case (mcp-server-runtime--discard-connection runtime :failed)
                        (error nil nil))
          (setf (mcp-server-runtime-tools-discovered-version runtime) target-version
                (mcp-server-runtime-state runtime) :failed
                (mcp-server-runtime-failure runtime)
                  (mcp-managed--bounded-string (mcp-managed-failure runtime cause)
                   :limit 1000)
                discovery-p t)
          (mcp-managed-reset-client runtime)
          (setf (mcp-server-runtime-launch-environment-fingerprint runtime) nil)
          (incf (mcp-server-runtime-tools-revision runtime)) (error cause)))))
    (values runtime discovery-p)))


(defun mcp-server-runtime-close (runtime)
  "Close RUNTIME's client and leave it restartable."
  (with-lock-held ((mcp-server-runtime-lock runtime))
    (unwind-protect
        (mcp-managed-call-with-cleanup runtime
         (lambda () (mcp-client-close (mcp-server-runtime-client runtime))))
      (setf (mcp-server-runtime-capabilities runtime) nil
            (mcp-server-runtime-tools runtime) nil
            (mcp-server-runtime-tool-schema-bytes runtime) 0
            (mcp-server-runtime-observed-connection-generation runtime) nil
            (mcp-server-runtime-launch-environment-fingerprint runtime) nil
            (mcp-server-runtime-failure runtime) nil
            (mcp-server-runtime-state runtime) :disconnected)
      (mcp-managed-reset-client runtime)))
  nil)


(defun mcp-server-runtime-detach (runtime)
  "Detach RUNTIME's inherited resources without signaling their owner."
  (with-lock-held ((mcp-server-runtime-lock runtime))
    (unwind-protect
        (mcp-managed-call-with-cleanup runtime
         (lambda () (mcp-client-detach (mcp-server-runtime-client runtime))))
      (setf (mcp-server-runtime-capabilities runtime) nil
            (mcp-server-runtime-tools runtime) nil
            (mcp-server-runtime-tool-schema-bytes runtime) 0
            (mcp-server-runtime-observed-connection-generation runtime) nil
            (mcp-server-runtime-launch-environment-fingerprint runtime) nil
            (mcp-server-runtime-failure runtime) nil
            (mcp-server-runtime-state runtime) :detached)
      (mcp-managed-reset-client runtime)))
  nil)


(defun mcp-manager--close-runtimes (runtimes close-function)
  "Close RUNTIMES concurrently with CLOSE-FUNCTION and signal the first failure.
Finish pending teardown and join owned workers before propagating a nonlocal exit."
  (let* ((runtime-vector (coerce runtimes 'simple-vector))
         (failures (make-array (length runtime-vector) :initial-element nil))
         (next-position 0)
         (threads nil))
    (labels ((close-at (index)
               (handler-case (funcall close-function (aref runtime-vector index))
                 (serious-condition (condition)
                   (setf (aref failures index) condition))))

             (close-next ()
               (close-at (prog1 next-position (incf next-position))))

             (join-next ()
               (join-thread (pop threads)))

             (preserve-exit (function)
               (block nil
                 (unwind-protect
                      (handler-case (funcall function)
                        (serious-condition () nil))
                   (return nil)))))
      (unwind-protect
           (progn
             (loop while (< next-position (length runtime-vector))
                   do (let ((position next-position))
                        (handler-case
                            (push (make-thread (lambda () (close-at position))
                                               :name "MCP managed server close")
                                  threads)
                          (serious-condition () (return)))
                        (incf next-position)))
             (loop while (< next-position (length runtime-vector)) do (close-next))
             (loop while threads do (join-next)))
        (loop while (< next-position (length runtime-vector))
              do (preserve-exit #'close-next))
        (loop while threads do (preserve-exit #'join-next)))
      (loop for failure across failures
            when failure
            do (error failure))))
  nil)


(defun mcp-manager-close (manager)
  "Close every server in MANAGER concurrently, preserving close-order failures."
  (with-lock-held ((mcp-manager-lock manager))
    (mcp-manager--close-runtimes (reverse (copy-list (mcp-manager-runtimes manager)))
     #'mcp-server-runtime-close)
    nil))


(defun mcp-manager-detach (manager)
  "Detach all inherited servers concurrently, serialized against discovery."
  (with-lock-held ((mcp-manager-lock manager))
    (mcp-manager--close-runtimes (mcp-manager-runtimes manager)
     #'mcp-server-runtime-detach))
  nil)


(defun mcp-manager-runtime (manager name)
  "Return MANAGER's case-sensitive raw server NAME, or NIL."
  (find name (mcp-manager-runtimes manager) :test #'string= :key
        #'mcp-server-runtime-name))


(defun mcp-manager--ordered-runtimes (manager)
  "Return MANAGER runtimes required-first with stable configuration order."
  (let ((runtimes (mcp-manager-runtimes manager)))
    (append
     (remove-if-not (lambda (runtime) (mcp-managed-required-p runtime)) runtimes)
     (remove-if (lambda (runtime) (mcp-managed-required-p runtime)) runtimes))))


(defun mcp-server-runtime--mark-failed (runtime condition)
  "Clear RUNTIME tools and publish CONDITION as its visible failure."
  (with-lock-held ((mcp-server-runtime-lock runtime))
    (let* ((previous-state (mcp-server-runtime-state runtime))
           (previous-tools-p (not (null (mcp-server-runtime-tools runtime))))
           (previous-schema-bytes (mcp-server-runtime-tool-schema-bytes runtime))
           (previous-failure (mcp-server-runtime-failure runtime))
           (failure
            (mcp-managed--bounded-string (mcp-managed-failure runtime condition)
             :limit 1000))
           (changed-p
            (or (not (eq previous-state :failed)) previous-tools-p
                (plusp previous-schema-bytes)
                (not (equal previous-failure failure)))))
      (handler-case (mcp-server-runtime--discard-connection runtime :failed)
                    (error nil (mcp-managed-reset-client runtime)
                           (setf (mcp-server-runtime-launch-environment-fingerprint
                                  runtime)
                                   nil)))
      (setf (mcp-server-runtime-tools runtime) nil
            (mcp-server-runtime-tool-schema-bytes runtime) 0
            (mcp-server-runtime-state runtime) :failed
            (mcp-server-runtime-failure runtime) failure)
      (when changed-p (incf (mcp-server-runtime-tools-revision runtime)))
      (and changed-p t))))


(defun mcp-server-runtime--budget-usage (runtime)
  "Return retained encoded input schema bytes for ready RUNTIME."
  (with-lock-held ((mcp-server-runtime-lock runtime))
    (if (eq (mcp-server-runtime-state runtime) :ready)
        (mcp-server-runtime-tool-schema-bytes runtime)
        0)))


(defun mcp-manager--runtime-failure-barrier-p
       (runtime target-runtime signal-target-failure-p)
  "Return true when RUNTIME's failure must cross the current manager boundary."
  (or (and (null target-runtime) (mcp-managed-required-p runtime))
      (and signal-target-failure-p (eq runtime target-runtime))))


(defun mcp-server-runtime--manager-connect-p (runtime target-runtime)
  "Return true when RUNTIME needs discovery during manager reconciliation."
  (or (mcp-server-runtime-tools-stale-p runtime)
      (with-lock-held ((mcp-server-runtime-lock runtime))
        (let ((state (mcp-server-runtime-state runtime)))
          (and (not (eq state :failed))
               (or (eq runtime target-runtime)
                   (and (eq state :ready) (mcp-managed-credential-check-p runtime))
                   (and
                    (member state '(:disconnected :connecting :detached) :test #'eq)
                    t)))))))


(defun mcp-manager--connect-runtimes
    (manager &key target-runtime signal-target-failure-p)
  "Reconcile MANAGER under its held lock and return true after any revision."
  (let ((allocated-schema-bytes 0)
        (changed-p nil))
    (dolist (runtime (mcp-manager--ordered-runtimes manager))
      (let* ((before (with-lock-held ((mcp-server-runtime-lock runtime))
                       (mcp-server-runtime-tools-revision runtime)))
             (connect-p (mcp-server-runtime--manager-connect-p runtime target-runtime))
             (cached-failure
               (and (not connect-p) (mcp-server-runtime--cached-failure runtime))))
        (when (and cached-failure
                   (mcp-manager--runtime-failure-barrier-p
                    runtime target-runtime signal-target-failure-p))
          (error cached-failure))
        (handler-case
            (progn
              (when connect-p
                (mcp-server-runtime--connect
                 runtime :allocated-schema-bytes allocated-schema-bytes))
              (let ((schema-bytes (mcp-server-runtime--budget-usage runtime))
                    (limit (mcp-managed-schema-limit runtime)))
                (when (> (+ allocated-schema-bytes schema-bytes) limit)
                  (mcp-managed-budget-error
                   runtime :resource ':input-schema-bytes
                   :allocated allocated-schema-bytes :requested schema-bytes
                   :limit limit))
                (incf allocated-schema-bytes schema-bytes)))
          (mcp-managed-server-error (condition)
            (when (mcp-server-runtime--mark-failed runtime condition)
              (setf changed-p t))
            (when (mcp-manager--runtime-failure-barrier-p
                   runtime target-runtime signal-target-failure-p)
              (error condition))))
        (unless (= before (with-lock-held ((mcp-server-runtime-lock runtime))
                            (mcp-server-runtime-tools-revision runtime)))
          (setf changed-p t))))
    (and changed-p t)))


(defun mcp-server-runtime-connect (runtime)
  "Initialize RUNTIME under its manager-wide aggregate discovery budgets."
  (let ((manager (mcp-server-runtime-manager runtime)))
    (if manager
        (with-lock-held ((mcp-manager-lock manager))
          (values runtime
                  (mcp-manager--connect-runtimes manager :target-runtime runtime
                   :signal-target-failure-p t)))
        (let ((cached-failure (mcp-server-runtime--cached-failure runtime)))
          (if (and cached-failure (not (mcp-server-runtime-tools-stale-p runtime)))
              (error cached-failure)
              (mcp-server-runtime--connect runtime))))))


(defun mcp-server-runtime--discard-connection (runtime state)
  "Close RUNTIME's client and clear retained connection state to STATE.

The caller must hold RUNTIME's lock."
  (unwind-protect
      (mcp-managed-call-with-cleanup runtime
       (lambda () (mcp-client-close (mcp-server-runtime-client runtime))))
    (mcp-managed-reset-client runtime)
    (setf (mcp-server-runtime-capabilities runtime) nil
          (mcp-server-runtime-tools runtime) nil
          (mcp-server-runtime-tool-schema-bytes runtime) 0
          (mcp-server-runtime-observed-connection-generation runtime) nil
          (mcp-server-runtime-launch-environment-fingerprint runtime) nil
          (mcp-server-runtime-failure runtime) nil
          (mcp-server-runtime-state runtime) state))
  nil)


(defun mcp-server-runtime--capability-p (runtime name)
  "Return true when RUNTIME's connected server advertises capability NAME."
  (let ((capabilities
         (mcp-client-server-capabilities (mcp-server-runtime-client runtime))))
    (multiple-value-bind (capability present-p)
        (gethash name capabilities)
      (cond ((not present-p) nil) ((hash-table-p capability) t)
            (t
             (mcp-managed-error runtime capability
              (format nil
                      "MCP server ~A advertised malformed ~A capability metadata."
                      (mcp-server-runtime-name runtime) name)))))))


;;;; -- Construction and Default Boundaries --

(defmethod initialize-instance :after ((server mcp-managed-server) &key)
  "Materialize a lazy client factory with managed notification tracking."
  (unless (slot-boundp server 'client)
    (setf (slot-value server 'client)
          (funcall (mcp-managed-client-factory server) server
                   (lambda (method params)
                     (declare (ignore params))
                     (when (string= method "notifications/tools/list_changed")
                       (mcp-server-runtime-request-tool-refresh server)))))))


(defmethod initialize-instance :after ((manager mcp-connection-manager) &key)
  "Validate all identities and ownership before attaching any server."
  (with-lock-held (*mcp-managed-ownership-lock*)
    (let ((seen (make-hash-table :test #'equal)))
      (dolist (server (mcp-manager-runtimes manager))
        (let ((owner (mcp-server-runtime-manager server))
              (name (mcp-server-runtime-name server)))
          (when (or (gethash name seen) (and owner (not (eq owner manager))))
            (mcp-managed-error server nil
             "Duplicate server identity or existing lifecycle owner."))
          (setf (gethash name seen) t)))
      (dolist (server (mcp-manager-runtimes manager))
        (setf (mcp-server-runtime-manager server) manager)))))

(defmethod mcp-managed-call-with-scope ((server mcp-managed-server) function)
  "Translate ordinary boundary failures into managed connection conditions."
  (handler-case (funcall function)
    (mcp-managed-server-error (condition) (error condition))
    (error (condition) (mcp-managed-error server condition))))

(defmethod mcp-managed-call-with-cleanup ((server mcp-managed-server) function)
  "Run local cleanup without requiring credentials."
  (funcall function))

(defmethod mcp-managed-prepare-client ((server mcp-managed-server))
  "Accept the protocol client's validated metadata."
  nil)

(defmethod mcp-managed-reset-client ((server mcp-managed-server))
  "Forget connection metadata on local teardown."
  (let ((client (mcp-server-runtime-client server)))
    (setf (mcp-client-server-capabilities client) nil
          (mcp-client-server-info client) nil
          (mcp-client-instructions client) nil))
  nil)


(defmethod mcp-managed-check-credentials ((server mcp-managed-server))
  "Check persistent credential identity within the active scope."
  (mcp-managed-check-credential-identity server))

(defmethod mcp-managed-credential-check-p ((server mcp-managed-server))
  "Return NIL for clients without persistent credential identity."
  nil)

(defmethod mcp-managed-error ((server mcp-managed-server) cause &optional message)
  "Signal a bounded diagnostic for SERVER."
  (error 'mcp-managed-server-error
         :server-name (mcp-server-runtime-name server)
         :required-p (mcp-managed-required-p server)
         :cause nil
         :message (or message (mcp-managed-failure server cause))))

(defmethod mcp-managed-failure ((server mcp-managed-server) cause)
  "Bound the protocol diagnostic; override this boundary for secret redaction."
  (mcp-managed--bounded-string
   (if (typep cause 'mcp-error) (mcp-error-message cause) (princ-to-string cause))))

(defmethod mcp-managed-budget-error
    ((server mcp-managed-server) &key resource allocated requested limit)
  "Signal the exact requested and available aggregate schema allocation."
  (error 'mcp-managed-budget-exceeded
         :server-name (mcp-server-runtime-name server)
         :required-p (mcp-managed-required-p server)
         :resource resource :allocated allocated :requested requested :limit limit
         :message (format nil "MCP server ~A exceeds ~A budget ~D: ~D allocated, ~D requested."
                          (mcp-server-runtime-name server) resource limit allocated requested)))

(defmethod mcp-managed-cached-error ((server mcp-managed-server))
  "Reconstruct a managed error without retaining a transient condition."
  (make-condition 'mcp-managed-server-error
                  :server-name (mcp-server-runtime-name server)
                  :required-p (mcp-managed-required-p server)
                  :message (or (mcp-server-runtime-failure server) "MCP server unavailable.")))

(defun mcp-server-runtime--cached-failure (runtime)
  "Return the last failure without repeating network or credential work."
  (with-lock-held ((mcp-server-runtime-lock runtime))
    (when (eq (mcp-server-runtime-state runtime) :failed)
      (mcp-managed-cached-error runtime))))

(defun mcp-managed-schema-limit (server)
  "Return SERVER's owning manager budget, or the dynamic default."
  (let ((manager (mcp-server-runtime-manager server)))
    (or (and manager (mcp-manager-maximum-schema-bytes manager))
        *mcp-maximum-retained-input-schema-bytes*)))

(defmethod mcp-managed-prepare-tools
    ((server mcp-managed-server) tools &key (allocated-schema-bytes 0))
  "Reject duplicate identities and bound the retained input schemas."
  (let ((seen (make-hash-table :test #'equal)) (bytes 0))
    (dolist (tool tools)
      (unless (and (typep tool 'mcp-tool) (plusp (length (mcp-tool-name tool))))
        (mcp-managed-error server nil "Invalid tool identity."))
      (when (gethash (mcp-tool-name tool) seen)
        (mcp-managed-error server nil (format nil "Duplicate tool ~S." (mcp-tool-name tool))))
      (setf (gethash (mcp-tool-name tool) seen) t)
      (incf bytes (length (babel:string-to-octets (json-encode (mcp-tool-input-schema tool))
                                               :encoding :utf-8)))
      (when (> (+ allocated-schema-bytes bytes) (mcp-managed-schema-limit server))
        (mcp-managed-budget-error server :resource ':input-schema-bytes
                                 :allocated allocated-schema-bytes :requested bytes
                                 :limit (mcp-managed-schema-limit server))))
    (values tools bytes)))

(defun mcp-manager-start (manager)
  "Discover MANAGER; close every allocated client on incomplete startup."
  (let ((complete-p nil))
    (unwind-protect
         (progn
           (with-lock-held ((mcp-manager-lock manager))
             (mcp-manager--connect-runtimes manager))
           (setf complete-p t)
           manager)
      (unless complete-p
        (handler-case (mcp-manager-close manager)
          (serious-condition () nil))))))

(defun mcp-manager-refresh (manager &key server-name)
  "Request rediscovery of every server or SERVER-NAME, then publish complete results."
  (let ((target (and server-name (mcp-manager-runtime manager server-name))))
    (when (and server-name (null target))
      (error 'mcp-error :message (format nil "Unknown MCP server ~S." server-name)))
    (with-lock-held ((mcp-manager-lock manager))
      (dolist (server (if target (list target) (mcp-manager-runtimes manager)))
        (mcp-server-runtime-request-tool-refresh server))
      (mcp-manager--connect-runtimes manager :target-runtime target
                                   :signal-target-failure-p (not (null target))))))

(defun mcp-manager-tool-revisions (manager)
  "Return synchronized discovery revisions in presentation order."
  (mapcar (lambda (server)
            (with-lock-held ((mcp-server-runtime-lock server))
              (mcp-server-runtime-tools-revision server)))
          (mcp-manager-runtimes manager)))


(defgeneric mcp-managed-credential-key (server)
  (:documentation "Return an opaque non-secret identity and an optional resolution failure.
Callers supply the identity of the exact credential snapshot active in the use scope."))

(defmethod mcp-managed-credential-key ((server mcp-managed-server))
  "Return no persistent credential identity by default."
  (values nil nil))

(defun mcp-managed-check-credential-identity (server)
  "Discard a live connection after credential rotation or failed resolution.
The caller holds the server lock inside its credential use scope."
  (when (mcp-managed-credential-check-p server)
    (multiple-value-bind (identity failure)
        (handler-case (mcp-managed-credential-key server)
          (error (condition) (values nil condition)))
      (when failure
        (mcp-server-runtime--discard-connection server :disconnected)
        (error failure))
      (when (and (mcp-client-connected-p (mcp-server-runtime-client server))
                 (mcp-transport-open-p (mcp-client-transport (mcp-server-runtime-client server)))
                 (not (equal identity (mcp-server-runtime-launch-environment-fingerprint server))))
        (mcp-server-runtime--discard-connection server :disconnected))
      (setf (mcp-server-runtime-launch-environment-fingerprint server) identity)))
  nil)
