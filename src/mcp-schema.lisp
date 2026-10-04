(in-package #:mcparen)

;;;; -- Bounded Input Schemas --

(define-condition mcp-input-schema-error (mcp-error)
  ((reason :initarg :reason :reader mcp-input-schema-error-reason
           :documentation "The invalid schema member or exceeded JSON constraint.")
   (value :initarg :value :initform nil :reader mcp-input-schema-error-value
          :documentation "The offending member, when useful."))
  (:documentation "An MCP tool input schema cannot be projected within its bounds."))

(-> mcp-schema--error (keyword &optional t) null)
(defun mcp-schema--error (reason &optional value)
  "Signal a typed input schema failure for REASON and VALUE."
  (error 'mcp-input-schema-error
         :message "Invalid bounded MCP input schema." :reason reason :value value))

(-> mcp-schema--encode (hash-table json-limits integer) string)
(defun mcp-schema--encode (schema limits maximum-bytes)
  "Encode SCHEMA within structural LIMITS and a UTF-8 byte bound."
  (let ((encoded (argo:json-encode schema :limits limits)))
    (when (> (length (string-to-octets encoded :encoding ':utf-8)) maximum-bytes)
      (mcp-schema--error ':bytes))
    encoded))

(-> mcp-validate-input-schema
    (t &key (:maximum-bytes integer) (:maximum-depth integer)
            (:maximum-nodes integer) (:maximum-string-characters integer)
            (:maximum-key-characters integer))
    (values hash-table integer))
(defun mcp-validate-input-schema
    (schema &key (maximum-bytes (* 1024 1024)) (maximum-depth 64)
                 (maximum-nodes 32768) (maximum-string-characters 65536)
                 (maximum-key-characters 256))
  "Return a detached object input schema and its compact UTF-8 byte count.

Apply bounds to both the original and normalized JSON. Default missing TYPE
and PROPERTIES to an object schema. Preserve object-containing type unions,
JSON false and null, extensions, and nested schemas. Node counts include keys,
as in argo. JSON arrays supplied as proper nonempty lists become vectors."
  (unless (hash-table-p schema)
    (mcp-schema--error ':root-object))
  (let ((limits (make-json-limits
                 :maximum-characters maximum-bytes
                 :maximum-depth maximum-depth :maximum-nodes maximum-nodes
                 :maximum-string-characters maximum-string-characters
                 :maximum-object-key-characters maximum-key-characters)))
    (handler-case
        (let ((copy (argo:json-decode
                     (mcp-schema--encode schema limits maximum-bytes) :limits limits)))
          (multiple-value-bind (type present-p) (gethash "type" copy)
            (if present-p
                (unless (or (equal type "object")
                            (and (vectorp type) (not (stringp type))
                                 (every #'stringp type)
                                 (find "object" type :test #'equal)))
                  (mcp-schema--error ':type type))
                (setf (gethash "type" copy) "object")))
          (multiple-value-bind (properties present-p) (gethash "properties" copy)
            (if present-p
                (unless (hash-table-p properties)
                  (mcp-schema--error ':properties properties))
                (setf (gethash "properties" copy) (json-object))))
          (multiple-value-bind (required present-p) (gethash "required" copy)
            (when (and present-p
                       (not (and (vectorp required) (not (stringp required))
                                 (every (lambda (name)
                                          (and (stringp name) (plusp (length name))))
                                        required))))
              (mcp-schema--error ':required required)))
          (let ((encoded (mcp-schema--encode copy limits maximum-bytes)))
            (values copy (length (string-to-octets encoded :encoding ':utf-8)))))
      (json-limit-exceeded (condition)
        (mcp-schema--error (json-limit-exceeded-constraint condition)))
      (json-error (condition)
        (declare (ignore condition))
        (mcp-schema--error ':value)))))
