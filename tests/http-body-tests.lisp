(in-package #:mcparen)

;;;; -- HTTP Body Reading --

(defclass test-body-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((octets   :initarg :octets   :reader   test-body-stream-octets)
   (position :initform 0        :accessor test-body-stream-position)
   (unclean  :initarg :unclean  :initform nil :accessor test-body-stream-unclean)
   (error-text :initarg :error-text
               :initform "error:0A000126:SSL routines::unexpected eof while reading"
               :reader test-body-stream-error-text)))

(defun test-unclean-condition (text)
  "Return the TLS condition OpenSSL raises when a peer closes without close_notify."
  (make-condition 'cl+ssl::ssl-error-ssl
                  :queue         (list text)
                  :printed-queue text
                  :handle        nil
                  :ret           -1))

(defmethod trivial-gray-streams:stream-read-sequence
    ((stream test-body-stream) buffer start end &key)
  (let ((position (test-body-stream-position stream))
        (octets   (test-body-stream-octets stream)))
    (cond
      ((< position (length octets))
       (let ((count (min (- end start) (- (length octets) position))))
         (replace buffer octets
                  :start1 start :end1 (+ start count)
                  :start2 position :end2 (+ position count))
         (setf (test-body-stream-position stream) (+ position count))
         (+ start count)))
      ((test-body-stream-unclean stream)
       (setf (test-body-stream-unclean stream) nil)
       (error (test-unclean-condition (test-body-stream-error-text stream))))
        (t start))))

(defmethod sb-gray:stream-read-byte ((stream test-body-stream))
  (let ((position (test-body-stream-position stream))
        (octets   (test-body-stream-octets stream)))
    (cond
      ((< position (length octets))
       (setf (test-body-stream-position stream) (1+ position))
       (aref octets position))
      ((test-body-stream-unclean stream)
       (setf (test-body-stream-unclean stream) nil)
       (error (test-unclean-condition (test-body-stream-error-text stream))))
      (t :eof))))

(defun test-body-octets (text)
  (babel:string-to-octets text :encoding :utf-8))

(define-test http-body-fixture-message
  (test-assert (search "unexpected eof"
                       (princ-to-string (test-unclean-condition "x unexpected eof while reading")))
               "the fixture condition must print the OpenSSL reason"))

(define-test http-body-accepts-complete-body
  (let* ((octets (test-body-octets "{\"jsonrpc\":\"2.0\"}"))
         (stream (make-instance 'test-body-stream :octets octets :unclean t)))
    (test-equal octets
                (mcp-http--body-octets stream :content-length (length octets))
                :test #'equalp)))

(define-test http-body-rejects-truncated-body
  (let* ((octets (test-body-octets "{\"jsonrpc\":\"2.0"))
         (stream (make-instance 'test-body-stream :octets octets :unclean t))
         (condition (handler-case (progn (mcp-http--body-octets stream :content-length 185) nil)
                      (mcp-protocol-error (signalled) signalled))))
    (test-assert condition "a short body must signal mcp-protocol-error")
    (test-assert (search (format nil "~D" (length octets)) (mcp-error-message condition))
                 "the message must name the octets that arrived")
    (test-assert (search "185" (mcp-error-message condition))
                 "the message must name the declared length")))

(define-test http-body-accepts-unclean-close-without-length
  (let* ((octets (test-body-octets "{}"))
         (stream (make-instance 'test-body-stream :octets octets :unclean t)))
    (test-equal octets (mcp-http--body-octets stream) :test #'equalp)))

(define-test http-body-propagates-other-tls-errors
  (let* ((octets (test-body-octets "{}"))
         (stream (make-instance 'test-body-stream
                                :octets octets :unclean t
                                :error-text "error:0A00010B:SSL routines::wrong version number"))
         (signalled (handler-case (progn (mcp-http--body-octets stream) nil)
                      (cl+ssl::ssl-error (condition) condition))))
    (test-assert signalled "an SSL error that is not an unclean close must propagate")))

(define-test http-body-reads-octet-line-past-unclean-close
  (let ((stream (make-instance 'test-body-stream
                               :octets (babel:string-to-octets "data: a" :encoding :utf-8)
                               :unclean t)))
    (multiple-value-bind (line eof) (mcp-http--read-octet-line stream 100 "test")
      (test-equal "data: a" line)
      (test-equal t eof))
    (multiple-value-bind (line eof) (mcp-http--read-octet-line stream 100 "test")
      (test-equal nil line)
      (test-equal t eof))))

(defclass test-torn-body-stream (test-body-stream) ())

(defmethod trivial-gray-streams:stream-read-sequence
    ((stream test-torn-body-stream) buffer start end &key)
    (let* ((position (test-body-stream-position stream))
        (octets   (test-body-stream-octets stream))
        (count    (min (- end start) (- (length octets) position))))
    (replace buffer octets
             :start1 start :end1 (+ start count)
             :start2 position :end2 (+ position count))
    (setf (test-body-stream-position stream) (+ position count))
    (when (test-body-stream-unclean stream)
      (setf (test-body-stream-unclean stream) nil)
      (error (test-unclean-condition (test-body-stream-error-text stream))))
    (+ start count)))

(define-test http-body-keeps-octets-read-before-unclean-close
  (let* ((octets (test-body-octets "{\"jsonrpc\":\"2.0\",\"id\":1}"))
         (stream (make-instance 'test-torn-body-stream :octets octets :unclean t)))
    (test-equal octets
                (mcp-http--body-octets stream :content-length (length octets))
                :test #'equalp)))

(define-test http-body-bounds-reading-by-the-character-limit
  (let* ((octets (make-array 400 :element-type '(unsigned-byte 8) :initial-element 32))
         (stream (make-instance 'test-body-stream :octets octets)))
    (test-assert (handler-case
                     (progn (mcp-http--body-octets stream :source "test" :limit 10) nil)
                   (error () t))
                 "a body past four octets per character must be rejected while reading")
    (test-assert (<= (test-body-stream-position stream) 41)
                 "reading must stop at the ceiling, not at the end of the body")))

(define-test http-body-rejects-malformed-content-length
  (let ((condition (handler-case
                       (progn (mcp-http--content-length '(("content-length" . "12abc"))) nil)
                     (mcp-protocol-error (signalled) signalled))))
    (test-assert condition "a malformed Content-Length must signal mcp-protocol-error")))

