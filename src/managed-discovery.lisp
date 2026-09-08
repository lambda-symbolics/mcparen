(in-package #:mcparen)

;;;; -- Detached Discovery Observations --

(defstruct (mcp-discovery-snapshot (:constructor mcp-discovery-snapshot--create))
  "One detached observation of a managed connection and its discovery revision."
  (name "" :read-only t)
  (state ':disconnected :read-only t)
  (generation nil :read-only t)
  (revision 0 :read-only t)
  (requested-version 0 :read-only t)
  (discovered-version 0 :read-only t)
  (capabilities nil :read-only t)
  (tools nil :read-only t)
  (schema-bytes 0 :read-only t)
  (diagnostic nil :read-only t))

(defun mcp-managed--copy-value (value)
  "Detach protocol metadata so snapshot consumers cannot mutate retained discovery."
  (typecase value
    (string (copy-seq value))
    (hash-table
     (let ((copy (make-hash-table :test #'equal)))
       (maphash (lambda (key child)
                  (setf (gethash (mcp-managed--copy-value key) copy)
                        (mcp-managed--copy-value child))) value)
       copy))
    (vector (map 'vector #'mcp-managed--copy-value value))
    (cons (cons (mcp-managed--copy-value (first value))
                (mcp-managed--copy-value (rest value))))
    (mcp-tool
     (let ((copy
             (make-instance 'mcp-tool
                            :name (mcp-managed--copy-value (mcp-tool-name value))
                            :title (mcp-managed--copy-value (mcp-tool-title value))
                            :description (mcp-managed--copy-value (mcp-tool-description value))
                            :input-schema (mcp-managed--copy-value (mcp-tool-input-schema value))
                            :output-schema (mcp-managed--copy-value (mcp-tool-output-schema value))
                            :annotations (mcp-managed--copy-value (mcp-tool-annotations value))
                            :execution (mcp-managed--copy-value (mcp-tool-execution value))
                            :task-support (mcp-managed--copy-value (mcp-tool-task-support value)))))
       (when (slot-boundp value 'raw)
         (setf (slot-value copy 'raw) (mcp-managed--copy-value (mcp-tool-raw value))))
       copy))
    (t value)))


(defun mcp-server-runtime-snapshot (server)
  "Capture a detached, synchronized discovery observation without network work."
  (with-lock-held ((mcp-server-runtime-lock server))
    (mcp-discovery-snapshot--create :name (copy-seq (mcp-server-runtime-name server))
     :state (mcp-server-runtime-state server) :generation
     (mcp-server-runtime-observed-connection-generation server) :revision
     (mcp-server-runtime-tools-revision server) :requested-version
     (with-lock-held ((mcp-server-runtime-tools-change-lock server))
       (mcp-server-runtime-tools-change-version server))
     :discovered-version (mcp-server-runtime-tools-discovered-version server)
     :capabilities (mcp-managed--copy-value (mcp-server-runtime-capabilities server))
     :tools (mcp-managed--copy-value (mcp-server-runtime-tools server)) :schema-bytes
     (mcp-server-runtime-tool-schema-bytes server) :diagnostic
     (mcp-managed--copy-value (mcp-server-runtime-failure server)))))

(defun mcp-manager-snapshot (manager)
  "Capture every server in presentation order under the manager discovery lock."
  (with-lock-held ((mcp-manager-lock manager))
    (mapcar #'mcp-server-runtime-snapshot (mcp-manager-runtimes manager))))

(defgeneric mcp-managed-project-result (server items)
  (:documentation "Detach and optionally sanitize list results inside the credential scope."))

(defmethod mcp-managed-project-result ((server mcp-managed-server) items)
  "Return detached protocol items."
  (mcp-managed--copy-value items))

(defun mcp-manager-collect (manager &key server-name list-function item-key)
  "Return successes and failures as (SERVER . VALUE) lists in presentation order.
LIST-FUNCTION receives a connected client. ITEM-KEY optionally identifies items;
duplicate identities within a server fail that server. Equal identities from
different servers retain their separate server qualification. Optional failures
never suppress another server's results. Returned diagnostics are scope-projected."
  (let ((selected (if server-name
                      (let ((server (mcp-manager-runtime manager server-name)))
                        (unless server
                          (error 'mcp-error :message (format nil "Unknown MCP server ~S." server-name)))
                        (list server))
                      (mcp-manager-runtimes manager)))
        (successes nil) (failures nil))
    (dolist (server selected)
      (handler-case
          (mcp-server-runtime-call
           server
           (lambda (client)
             (let ((items (mcp-managed-project-result
                           server (funcall list-function client))))
               (when item-key
                 (let ((seen (make-hash-table :test #'equal)))
                   (dolist (item items)
                     (let ((key (funcall item-key item)))
                       (when (gethash key seen)
                         (mcp-managed-error server nil "Duplicate discovery item identity."))
                       (setf (gethash key seen) t)))))
               (push (cons server items) successes))))
        (mcp-managed-server-error (condition)
          (push (cons server (mcp-managed-failure server condition)) failures))))
    (values (nreverse successes) (nreverse failures))))

(defun mcp-server-runtime-cancel (server)
  "Cancel in-flight transport requests, then leave SERVER disconnected and restartable."
  (unwind-protect
       (mcp-managed-call-with-cleanup
        server (lambda ()
                 (mcp-transport-close (mcp-client-transport (mcp-server-runtime-client server)))))
    (mcp-server-runtime-close server)))

(defun mcp-manager-build (server-factories &key (manager-class 'mcp-connection-manager)
                                             manager-initargs)
  "Build and start a manager from zero-argument SERVER-FACTORIES.
Close all previously returned servers if a later factory or initialization fails."
  (let ((servers nil) (manager nil))
    (unwind-protect
         (progn
           (dolist (factory server-factories)
             (push (funcall factory) servers))
           (setf manager (apply #'make-instance manager-class :runtimes (reverse servers)
                                manager-initargs))
           (mcp-manager-start manager))
      (unless manager
        (ignore-errors
          (mcp-manager--close-runtimes servers #'mcp-server-runtime-close))))))
