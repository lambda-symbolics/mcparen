(in-package #:mcparen)

;;;; -- Input Schema Normalization --

(define-test input-schema-normalizes-default-object
  (multiple-value-bind (schema bytes)
      (mcp-validate-input-schema (json-object))
    (test-assert (hash-table-p schema))
    (test-equal "object" (json-get schema "type") :test #'string=)
    (test-assert (hash-table-p (json-get schema "properties")))
    (test-assert (integerp bytes))
    (test-equal bytes (length (string-to-octets (argo:json-encode schema)
                                               :encoding ':utf-8)))))

(define-test input-schema-preserves-json-values-and-detaches
  (let* ((annotations (json-object "description" "A tool" "default" (json-false)))
         (properties (json-object "enabled" (json-object "type" "boolean")))
         (original (json-object "type" (vector "object" "null")
                                "properties" properties
                                "required" (vector "enabled")
                                "x-annotation" annotations)))
    (multiple-value-bind (normalized bytes)
        (mcp-validate-input-schema original)
      (test-assert (not (eq original normalized)))
      (test-assert (not (eq properties (json-get normalized "properties"))))
      (test-equal (vector "object" "null") (json-get normalized "type") :test #'equalp)
      (test-assert (json-false-p (gethash "default" (json-get normalized "x-annotation"))))
      (test-assert (plusp bytes))
      (setf (gethash "description" annotations) "changed")
      (test-equal "A tool"
                  (json-get (json-get normalized "x-annotation") "description")
                  :test #'string=))))

(define-test input-schema-accepts-object-containing-type-unions
  (dolist (type (list (vector "object" "null")
                      (vector "null" "object")
                      "object"))
    (multiple-value-bind (schema bytes)
        (mcp-validate-input-schema
         (json-object "type" type "properties" (json-object)))
      (test-assert (hash-table-p schema))
      (test-assert (plusp bytes)))))

(define-test input-schema-rejects-malformed-presence-and-required
  (dolist (schema (list (json-object "properties" nil)
                        (json-object "properties" "wrong")
                        (json-object "required" nil)
                        (json-object "required" "name")
                        (json-object "required" (vector ""))
                        (json-object "type" nil)))
    (test-signals mcp-input-schema-error
      (mcp-validate-input-schema schema))))

(define-test input-schema-rejects-bounds-and-unrepresentable-values
  (test-signals mcp-input-schema-error
    (mcp-validate-input-schema
     (json-object "description" "12345")
     :maximum-string-characters 4))
  (let ((circular (make-hash-table :test #'equal)))
    (setf (gethash "self" circular) circular)
    (test-signals mcp-input-schema-error
      (mcp-validate-input-schema circular))))

(define-test input-schema-reports-reason-and-value
  (let ((condition
          (test-signals mcp-input-schema-error
            (mcp-validate-input-schema (json-object "properties" nil)))))
    (test-equal ':properties (mcp-input-schema-error-reason condition))
    (test-assert (null (mcp-input-schema-error-value condition)))))

(define-test input-schema-checks-original-and-projected-bounds
  (dolist (options '((:maximum-bytes 2) (:maximum-nodes 1) (:maximum-depth 0)))
    (test-signals mcp-input-schema-error
      (apply #'mcp-validate-input-schema (json-object) options)))
  (let* ((schema (json-object "type" "object" "properties" (json-object)
                              "description" (make-string 40 :initial-element #\ř)))
         (characters (length (argo:json-encode schema))))
    (test-signals mcp-input-schema-error
      (mcp-validate-input-schema schema :maximum-bytes characters)))
  (test-signals mcp-input-schema-error
    (mcp-validate-input-schema (json-object "long-key" nil) :maximum-key-characters 3))
  (test-signals mcp-input-schema-error
    (mcp-validate-input-schema (json-object "extension" #'identity)))
  (let ((array (vector nil)))
    (setf (aref array 0) array)
    (test-signals mcp-input-schema-error
      (mcp-validate-input-schema (json-object "extension" array))))
  (test-assert (hash-table-p
                (mcp-validate-input-schema (json-object "required" #())))))
