(in-package #:mcparen)

;;;; -- Tool Routing Headers --

(define-condition mcp-invalid-tool-header (mcp-protocol-error) ()
  (:documentation "An invalid x-mcp-header annotation in a tool input schema."))

(-> mcp-http--routing-token-p (t) boolean)
(defun mcp-http--routing-token-p (value)
  "Return true when VALUE is a nonempty RFC 9110 field-name token."
  (and (stringp value) (plusp (length value))
       (every (lambda (character)
                (let ((code (char-code character)))
                  (or (<= 48 code 57) (<= 65 code 90) (<= 97 code 122)
                      (not (null (find character "!#$%&'*+-.^_`|~"))))))
              value)))

(-> mcp-http--tool-header-paths (hash-table) list)
(defun mcp-http--tool-header-paths (schema)
  "Return valid header names, primitive types and their exact instance paths."
  (let ((seen  (make-hash-table :test 'equalp))
        (paths nil))
    (labels ((walk (node path reachable-p)
               "Visit annotations, extending PATH only through properties."
               (cond
                 ((hash-table-p node)
                  (multiple-value-bind (name present-p) (gethash "x-mcp-header" node)
                    (when present-p
                      (let ((type (json-get node "type")))
                        (unless (and reachable-p path
                                     (mcp-http--routing-token-p name)
                                     (member type '("integer" "string" "boolean") :test #'equal)
                                     (not (gethash name seen)))
                          (error 'mcp-invalid-tool-header
                                 :message "Invalid, duplicate or unreachable x-mcp-header annotation."
                                 :method  "tools/list"
                                 :payload path))
                        (setf (gethash name seen) t)
                        (push (list name type (reverse path)) paths))))
                  (maphash (lambda (key value)
                             (if (and reachable-p (equal key "properties") (hash-table-p value))
                                 (maphash (lambda (property child)
                                            (walk child (cons property path) t)) value)
                                 (walk value nil nil)))
                           node))
                 ((and (vectorp node) (not (stringp node)))
                  (map nil (lambda (value) (walk value nil nil)) node)))))
      (walk schema nil t))
    paths))

(-> mcp-http--argument-at-path (hash-table list) (values t boolean))
(defun mcp-http--argument-at-path (arguments path)
  "Return the value and presence at the exact PATH in ARGUMENTS."
  (let ((value arguments))
    (dolist (key path (values value t))
      (unless (hash-table-p value)
        (return-from mcp-http--argument-at-path (values nil nil)))
      (multiple-value-bind (next present-p) (gethash key value)
        (unless present-p
          (return-from mcp-http--argument-at-path (values nil nil)))
        (setf value next)))))

(-> mcp-http--tool-routing-headers (hash-table hash-table) list)
(defun mcp-http--tool-routing-headers (schema arguments)
  "Mirror present, typed, statically reachable tool arguments into headers."
  (loop for (name type path) in (mcp-http--tool-header-paths schema)
        append
        (multiple-value-bind (value present-p) (mcp-http--argument-at-path arguments path)
          (when present-p
            (let ((text
                    (cond
                      ((and (equal type "string") (stringp value))
                       value)
                      ((and (equal type "boolean") (json-boolean-p value))
                       (if (json-true-p value) "true" "false"))
                      ((and (equal type "integer") (typep value '(integer -9007199254740991 9007199254740991)))
                       (format nil "~D" value))
                      (t
                       (error 'mcp-protocol-error
                              :message "An annotated tool argument does not match its header type."
                              :method  "tools/call"
                              :payload path)))))
              (list (cons (concatenate 'string "Mcp-Param-" name)
                          (mcp-http--encode-routing-value text))))))))
