(in-package #:mcparen)

;;;; -- Scoped Operations and Local Teardown --

(defgeneric mcp-managed-call-with-local-cleanup (server function cause)
  (:documentation "Invoke cleanup FUNCTION without resolving credentials after scope failure CAUSE."))

(defmethod mcp-managed-call-with-local-cleanup
    ((server mcp-managed-server) function cause)
  "Perform local teardown without an external credential dependency."
  (declare (ignore cause))
  (funcall function))

(defmethod mcp-managed-call-with-cleanup :around
    ((server mcp-managed-server) function)
  "Guarantee exactly one local teardown attempt when scope entry fails or unwinds.
Ordinary scope-entry errors use local cleanup. Other nonlocal exits keep their
original outcome after the local attempt. Never repeat a failed cleanup callback."
  (let ((started-p nil)
        (completed-p nil))
    (labels ((cleanup ()
               (setf started-p t)
               (funcall function))

             (fallback (cause)
               (setf started-p t)
               (mcp-managed-call-with-local-cleanup server #'cleanup cause))

             (preserve-exit (cause)
               (block nil
                 (unwind-protect
                      (handler-case (fallback cause)
                        (serious-condition () nil))
                   (return nil)))))
      (unwind-protect
           (multiple-value-prog1
               (handler-case
                   (call-next-method server #'cleanup)
                 (error (cause)
                   (if started-p
                       (error cause)
                       (fallback cause)))
                 (serious-condition (cause)
                   (unless started-p
                     (preserve-exit cause))
                   (error cause)))
             (setf completed-p t))
        (unless started-p
          (if completed-p
              (fallback nil)
              (preserve-exit nil)))))))

(-> mcp-server-runtime-call (mcp-managed-server function) t)
(defun mcp-server-runtime-call (server function)
  "Connect SERVER and invoke FUNCTION with its client inside the injected scope.
Project or consume secret-bearing results before FUNCTION returns. Requests may
run concurrently; client transport synchronization governs close/request races."
  (mcp-managed-call-with-scope
   server
   (lambda ()
     (mcp-server-runtime-connect server)
     (funcall function (mcp-server-runtime-client server)))))
