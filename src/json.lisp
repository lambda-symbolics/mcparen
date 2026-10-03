(in-package #:mcparen)

;;;; -- JSON Values --

;;; Mcparen uses argo's value model: objects are EQUAL hash tables with string
;;; keys, arrays are vectors, true is T, false is argo's JSON-FALSE marker,
;;; which JSON-GET reads as NIL, and null is NIL. Use GETHASH or
;;; JSON-GET-PRESENT where a present null must differ from an absent member.

(defparameter *json-diagnostic-limit* 4096
  "The maximum characters retained from invalid JSON in diagnostics.")

(defparameter *mcp-maximum-message-characters* (* 16 1024 1024)
  "The default maximum characters accepted in one inbound MCP document.")

(defparameter *mcp-json-limits* *json-strict-limits*
  "The structural bounds applied to every MCP JSON document.

Each call replaces only the character bound, with its own message limit.")


;;;; -- JSON Boundary --

(-> mcp-message-too-large-error (string integer) null)
(defun mcp-message-too-large-error (source limit)
  "Signal that SOURCE exceeded LIMIT characters."
  (error 'mcp-message-too-large
         :message
         (format nil "The ~A exceeded the ~D-character safety limit."
                 source limit)
         :method nil
         :payload nil
         :source source
         :limit limit))

(-> json--limits (integer) json-limits)
(defun json--limits (limit)
  "Return *MCP-JSON-LIMITS* with its character bound replaced by LIMIT."
  (let ((limits *mcp-json-limits*))
    (make-json-limits
     :maximum-characters limit
     :maximum-depth (json-limits-maximum-depth limits)
     :maximum-nodes (json-limits-maximum-nodes limits)
     :maximum-string-characters (json-limits-maximum-string-characters limits)
     :maximum-aggregate-string-characters
     (json-limits-maximum-aggregate-string-characters limits)
     :maximum-object-key-characters
     (json-limits-maximum-object-key-characters limits)
     :maximum-object-members (json-limits-maximum-object-members limits)
     :maximum-array-elements (json-limits-maximum-array-elements limits)
     :maximum-number-characters (json-limits-maximum-number-characters limits))))

(-> json--call-at-boundary (function string integer t) t)
(defun json--call-at-boundary (function source-name limit payload)
  "Call FUNCTION, reporting argo failures as MCP conditions about SOURCE-NAME.

An exceeded character bound becomes MCP-MESSAGE-TOO-LARGE for LIMIT; every
other JSON failure becomes MCP-PROTOCOL-ERROR carrying PAYLOAD."
  (handler-case
      (funcall function)
    (json-limit-exceeded (condition)
      (if (eq (json-limit-exceeded-constraint condition) ':characters)
          (mcp-message-too-large-error source-name limit)
          (error 'mcp-protocol-error
                 :message
                 (format nil "The ~A exceeded a JSON safety limit: ~A"
                         source-name (json-error-message condition))
                 :method nil
                 :payload payload)))
    (json-syntax-error (condition)
      (error 'mcp-protocol-error
             :message
             (format nil "Could not decode MCP JSON: ~A"
                     (json-error-message condition))
             :method nil
             :payload payload))
    (json-error (condition)
      (error 'mcp-protocol-error
             :message
             (format nil "The ~A is not representable as JSON: ~A"
                     source-name (json-error-message condition))
             :method nil
             :payload payload))))

(-> json-encode
    (t &key (:limit integer) (:source-name string))
    string)
(defun json-encode
    (value
     &key
       (limit *mcp-maximum-message-characters*)
       (source-name "outbound MCP JSON document"))
  "Encode VALUE as one compact JSON document within structural and size bounds."
  (json--call-at-boundary
   (lambda ()
     (argo:json-encode value :limits (json--limits limit)))
   source-name limit nil))

(-> json-encode-octets
    (t &key (:limit integer) (:source-name string))
    (vector (unsigned-byte 8)))
(defun json-encode-octets
    (value
     &key
       (limit *mcp-maximum-message-characters*)
       (source-name "MCP JSON value"))
  "Encode VALUE as compact UTF-8 JSON octets within structural and size bounds."
  (json--call-at-boundary
   (lambda ()
     (json-encode-utf8 value :limits (json--limits limit)))
   source-name limit nil))

(-> json-decode
    (string &key (:limit integer) (:source-name string))
    t)
(defun json-decode
    (source
     &key
       (limit *mcp-maximum-message-characters*)
       (source-name "MCP JSON document"))
  "Decode one complete JSON document from SOURCE within LIMIT characters."
  (json--call-at-boundary
   (lambda ()
     (argo:json-decode source :limits (json--limits limit)))
   source-name
   limit
   (subseq source 0 (min (length source) *json-diagnostic-limit*))))

(-> json-boolean-p (t) boolean)
(defun json-boolean-p (value)
  "Return true when VALUE represents JSON true or JSON false."
  (or (json-true-p value)
      (json-false-p value)))

(-> json-sequence->list (t) list)
(defun json-sequence->list (value)
  "Return JSON array VALUE as a fresh list."
  (unless (vectorp value)
    (error 'mcp-protocol-error
           :message "The MCP server returned a value where an array was required."
           :payload value))
  (coerce value 'list))

(-> bounded-diagnostic (t &key (:limit integer)) string)
(defun bounded-diagnostic (value &key (limit *json-diagnostic-limit*))
  "Return a bounded printed representation of VALUE for a diagnostic."
  (let ((text (with-output-to-string (stream)
                (let ((*print-length* 20)
                      (*print-level* 8))
                  (prin1 value stream)))))
    (if (<= (length text) limit)
        text
        (concatenate 'string (subseq text 0 (max 0 (- limit 3))) "..."))))


(-> stream-read-bounded-line
    (stream integer string)
    (values t boolean))
(defun stream-read-bounded-line (stream limit source)
  "Read one line from STREAM without retaining more than LIMIT characters.

Return the line or NIL, followed by true when end of input was encountered."
  (let ((characters
          (make-array (max 1 (min limit 4096))
                      :element-type 'character
                      :adjustable t
                      :fill-pointer 0)))
    (loop for character = (read-char stream nil nil)
          do
             (cond
               ((null character)
                (return
                  (values
                   (and (plusp (length characters))
                        (coerce characters 'string))
                   t)))
               ((char= character #\Newline)
                (return (values (coerce characters 'string) nil)))
               ((>= (length characters) limit)
                (mcp-message-too-large-error source limit))
               (t
                (vector-push-extend character characters 4096))))))

(-> stream-read-bounded-string
    (stream integer string)
    string)
(defun stream-read-bounded-string (stream limit source)
  "Read STREAM to a string while rejecting more than LIMIT characters."
  (let ((characters
          (make-array (max 1 (min limit 4096))
                      :element-type 'character
                      :adjustable t
                      :fill-pointer 0))
        (buffer (make-string 8192)))
    (loop for count = (read-sequence buffer stream)
          while (plusp count)
          do
             (when (> (+ (length characters) count) limit)
               (mcp-message-too-large-error source limit))
             (loop for index below count
                   do (vector-push-extend
                       (char buffer index) characters 8192)))
    (coerce characters 'string)))


;;;; -- JSON-RPC Validation --

(-> json-rpc-identifier-p (t) boolean)
(defun json-rpc-identifier-p (value)
  "Return true when VALUE is an MCP request identifier."
  (or (stringp value)
      (realp value)))

(-> json-rpc-message-validate (t) keyword)
(defun json-rpc-message-validate (message)
  "Validate MESSAGE as one MCP JSON-RPC object and return its message kind."
  (unless (hash-table-p message)
    (error 'mcp-protocol-error
           :message "The MCP peer emitted a non-object JSON-RPC message."
           :method nil
           :payload message))
  (unless (equal (json-get message "jsonrpc") "2.0")
    (error 'mcp-protocol-error
           :message "The MCP peer emitted invalid JSON-RPC version metadata."
           :method nil
           :payload message))
  (multiple-value-bind (method method-present-p)
      (gethash "method" message)
    (multiple-value-bind (identifier identifier-present-p)
        (gethash "id" message)
      (multiple-value-bind (result result-present-p)
          (gethash "result" message)
        (declare (ignore result))
        (multiple-value-bind (error-value error-present-p)
            (gethash "error" message)
          (cond
            (method-present-p
             (unless (stringp method)
               (error 'mcp-protocol-error
                      :message "The MCP JSON-RPC method is not a string."
                      :method nil
                      :payload message))
             (when (or result-present-p error-present-p)
               (error 'mcp-protocol-error
                      :message
                      "An MCP JSON-RPC request or notification contains response fields."
                      :method method
                      :payload message))
             (multiple-value-bind (params params-present-p)
                 (gethash "params" message)
               (when (and params-present-p
                          (not (hash-table-p params)))
                 (error 'mcp-protocol-error
                        :message "The MCP JSON-RPC params value is not an object."
                        :method method
                        :payload message)))
             (if identifier-present-p
                 (progn
                   (unless (json-rpc-identifier-p identifier)
                     (error 'mcp-protocol-error
                            :message
                            "The MCP JSON-RPC request identifier is invalid."
                            :method method
                            :payload message))
                   ':request)
                 ':notification))
            ((or result-present-p error-present-p)
             (unless (and identifier-present-p
                          (json-rpc-identifier-p identifier))
               (error 'mcp-protocol-error
                      :message
                      "The MCP JSON-RPC response identifier is absent or invalid."
                      :method nil
                      :payload message))
             (when (eq result-present-p error-present-p)
               (error 'mcp-protocol-error
                      :message
                      "An MCP JSON-RPC response must contain exactly one result or error."
                      :method nil
                      :payload message))
             (when error-present-p
               (unless (and (hash-table-p error-value)
                            (integerp (json-get error-value "code"))
                            (stringp (json-get error-value "message")))
                 (error 'mcp-protocol-error
                        :message "The MCP JSON-RPC error object is malformed."
                        :method nil
                        :payload message)))
             ':response)
            (t
             (error 'mcp-protocol-error
                    :message "The MCP peer emitted an unclassified JSON-RPC object."
                    :method nil
                    :payload message))))))))
