(in-package #:mcparen)

;;;; -- Transport Protocol --

(defparameter *mcp-modern-protocol-version* "2026-07-28"
  "The implemented MCP revision with stateless per-request negotiation.")

(-> mcp-modern-rpc-error-code-p (t) boolean)
(defun mcp-modern-rpc-error-code-p (code)
  "Recognize modern-only MCP errors without confusing legacy RPC errors with them."
  (case code
    ((-32020 -32021 -32022)
     t)
    (otherwise
     nil)))

(define-condition mcp-legacy-http-probe-response (mcp-transport-error) ()
  (:documentation "An unrecognized HTTP 400 from the modern discovery probe."))

(defclass mcp-transport ()
  ()
  (:documentation "The lifecycle and request boundary of one MCP connection."))

(defgeneric mcp-transport-open (transport)
  (:documentation "Open TRANSPORT if necessary and return it."))

(defgeneric mcp-transport-open-p (transport)
  (:documentation "Return true when TRANSPORT can exchange messages."))

(defgeneric mcp-transport-request (transport request timeout)
  (:documentation
   "Send JSON-RPC REQUEST and return its matching response within TIMEOUT seconds."))

(defgeneric mcp-transport-notify (transport notification timeout)
  (:documentation "Send JSON-RPC NOTIFICATION within TIMEOUT seconds."))

(defgeneric mcp-transport-set-protocol-version (transport version)
  (:documentation "Record the negotiated MCP protocol VERSION on TRANSPORT."))

(defmethod mcp-transport-set-protocol-version
    ((transport mcp-transport) version)
  "Leave transports without version-specific headers unchanged."
  (declare (ignore version))
  transport)

(defgeneric mcp-transport-legacy-probe-error-p (transport condition)
  (:documentation "Whether a discovery failure indicates legacy MCP on TRANSPORT."))

(defmethod mcp-transport-legacy-probe-error-p ((transport mcp-transport) condition)
  "Do not infer support for legacy MCP from an unknown transport error."
  (declare (ignore condition))
  nil)

(-> mcp-subscription-wire-id (t) t)
(defun mcp-subscription-wire-id (params)
  "Extract the modern subscription correlation ID from PARAMS or RESULT."
  (let ((meta (and (hash-table-p params) (json-get params "_meta"))))
    (when (hash-table-p meta)
      (json-get meta "io.modelcontextprotocol/subscriptionId"))))

(defgeneric mcp-transport-listen (transport request timeout &key receive install-cancel)
  (:documentation "Read one long-lived REQUEST; deliver notifications through RECEIVE and install a cancellation function. Return the final JSON-RPC response or NIL for EOF."))

(defmethod mcp-transport-listen
    ((transport mcp-transport) request timeout &key receive install-cancel)
  "Reject subscriptions on transports without a long-lived exchange."
  (declare (ignore timeout receive install-cancel))
  (error 'mcp-protocol-error
         :message "This MCP transport does not support subscriptions/listen."
         :method  "subscriptions/listen"
         :payload request))

(defgeneric mcp-transport-commit-initialize-session (transport)
  (:documentation
   "Commit session state staged by a validated initialization exchange."))

(defmethod mcp-transport-commit-initialize-session ((transport mcp-transport))
  "Leave transports without initialization-bound session state unchanged."
  transport)

(defgeneric mcp-transport-session-ready (transport)
  (:documentation
   "Start transport work that requires a fully initialized MCP session."))

(defmethod mcp-transport-session-ready ((transport mcp-transport))
  "Leave transports without initialized-session work unchanged."
  transport)

(defgeneric mcp-transport-close (transport)
  (:documentation "Close TRANSPORT and release all external resources."))

(defgeneric mcp-transport-detach (transport)
  (:documentation
   "Forget inherited resources after a fork without signaling their owner."))

(defmethod mcp-transport-detach ((transport mcp-transport))
  "Leave transports without inherited resources unchanged."
  (declare (ignore transport))
  nil)

(defmacro with-open-mcp-transport ((variable transport) &body body)
  "Open TRANSPORT as VARIABLE for BODY, then close it during unwinding."
  `(let ((,variable (mcp-transport-open ,transport)))
     (unwind-protect
          (progn ,@body)
       (mcp-transport-close ,variable))))
