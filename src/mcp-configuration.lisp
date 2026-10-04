(in-package #:mcparen)

;;;; -- Declarative Transport Configuration --

(defparameter *mcp-configuration-maximum-timeout-seconds* 3600
  "The default upper bound for a declarative transport timeout.")

(define-condition mcp-configuration-error (mcp-error)
  ((field :initarg :field :initform nil :reader mcp-configuration-error-field
          :documentation "The invalid transport field, when known.")
   (cause :initarg :cause :initform nil :reader mcp-configuration-error-cause
          :documentation "The underlying validation failure, when any."))
  (:documentation "A declarative MCP transport configuration is malformed."))

(-> mcp-configuration--error (string &key (:field t) (:cause t)) null)

(defun mcp-configuration--error (message &key field cause)
  "Signal a transport configuration failure without retaining credential values."
  (error 'mcp-configuration-error :message message :field field :cause cause))

(-> mcp-configuration--proper-list-p (t) boolean)

(defun mcp-configuration--proper-list-p (value)
  "Return true for a finite proper list, including the empty list."
  (and (listp value)
       (handler-case (integerp (list-length value))
         (type-error () nil))))

(-> mcp-configuration--validate-plist (t list) list)

(defun mcp-configuration--validate-plist (value allowed-keys)
  "Validate a finite, even, unique keyword plist against ALLOWED-KEYS."
  (unless (and (mcp-configuration--proper-list-p value)
               (evenp (length value)))
    (mcp-configuration--error "An MCP transport must be a proper, even property list."
                              :field ':transport))
  (let ((seen (make-hash-table :test #'eq)))
    (loop for (key data) on value by #'cddr
          do (unless (member key allowed-keys)
             (mcp-configuration--error "Unknown MCP transport field."
                                       :field (and (keywordp key) key)))
             (when (gethash key seen)
               (mcp-configuration--error "Duplicate MCP transport field." :field key))
             (setf (gethash key seen) t)))
  value)

(defparameter *mcp-stdio-command-maximum-characters* 4096
  "The maximum character length of an MCP standard-input command.")

(defparameter *mcp-stdio-argument-maximum-characters* 8192
  "The maximum character length of one MCP standard-input argument.")

(defparameter *mcp-stdio-maximum-arguments* 128
  "The maximum number of arguments for one MCP standard-input server.")

(defparameter *mcp-stdio-directory-maximum-characters* 4096
  "The maximum character length of an MCP standard-input directory.")

(defparameter *mcp-environment-name-maximum-characters* 255
  "The maximum character length of an MCP environment variable name.")

(defparameter *mcp-stdio-maximum-environment-bindings* 64
  "The maximum number of environment bindings for one MCP standard-input server.")

(defparameter *mcp-http-url-maximum-characters* 8192
  "The maximum character length of one MCP Streamable HTTP URL.")

(defparameter *mcp-http-header-name-maximum-characters* 255
  "The maximum character length of an MCP HTTP header name.")

(defparameter *mcp-http-maximum-header-bindings* 64
  "The maximum number of header bindings for one MCP Streamable HTTP server.")

(defclass mcp-environment-binding nil
          ((target :initarg :target :reader mcp-environment-binding-target :type string
            :documentation "The header or child-process environment name being set.")
           (source :initarg :source :reader mcp-environment-binding-source :type string
            :documentation "The parent environment variable read only when needed."))
          (:documentation
           "One credential-safe reference to a process environment value."))

(defclass mcp-transport-configuration nil nil
          (:documentation "The immutable native configuration of one MCP transport."))

(defclass mcp-stdio-transport-configuration (mcp-transport-configuration)
          ((command :initarg :command :reader mcp-stdio-configuration-command :type
            string :documentation "The executable used to start the MCP server.")
           (arguments :initarg :arguments :initform nil :reader
            mcp-stdio-configuration-arguments :type list :documentation
            "The exact argument strings following the executable.")
           (directory :initarg :directory :initform ':workspace :reader
                      mcp-stdio-configuration-directory :type t :documentation
                      "The :WORKSPACE marker or configured server directory.")
           (environment-bindings :initarg :environment-bindings :initform nil :reader
            mcp-stdio-configuration-environment-bindings :type list :documentation
            "Environment values resolved from parent variables at launch."))
          (:documentation "A native standard-input and standard-output MCP transport."))

(defclass mcp-http-transport-configuration (mcp-transport-configuration)
          ((url :initarg :url :reader mcp-http-configuration-url :type string
            :documentation "The non-credential Streamable HTTP endpoint.")
           (header-bindings :initarg :header-bindings :initform nil :reader
            mcp-http-configuration-header-bindings :type list :documentation
            "HTTP headers resolved from environment variables per request.")
           (connect-timeout-seconds :initarg :connect-timeout-seconds :initform 10
            :reader mcp-http-configuration-connect-timeout-seconds :type real
            :documentation "The bounded HTTP connection deadline."))
          (:documentation "A native MCP Streamable HTTP transport configuration."))

(-> mcp-configuration--bounded-string-p
    (t integer &key (:empty-p boolean))
    boolean)

(defun mcp-configuration--bounded-string-p
    (value maximum-characters &key empty-p)
  "Return true when VALUE is a bounded, single-line string.

An empty string is accepted only when EMPTY-P is true. NUL and terminal
control characters are rejected even when the native reader accepted them."
  (and (stringp value)
       (<= (length value) maximum-characters)
       (or empty-p (plusp (length value)))
       (loop for character across value
             always
             (and (not (char= character #\Null))
                  (or (graphic-char-p character)
                      (char= character #\Space))))))

(-> mcp-configuration--property (list keyword &key (:required-p boolean)) t)

(defun mcp-configuration--property (properties key &key required-p)
  "Return KEY from PROPERTIES and reject an absent required value."
  (loop for (candidate value) on properties by #'cddr
        when (eq candidate key)
        do (return-from mcp-configuration--property value))
  (when required-p
    (mcp-configuration--error
     (format nil "MCP configuration is missing required key ~S." key) :field key))
  nil)

(-> mcp-configuration--property-present-p (list keyword) boolean)

(defun mcp-configuration--property-present-p (properties key)
  "Return true when PROPERTIES explicitly contains KEY."
  (loop for tail on properties by #'cddr
        thereis (eq (first tail) key)))

(-> mcp-configuration--environment-name-p (t) boolean)

(defun mcp-configuration--environment-name-p (value)
  "Return true when VALUE is a portable POSIX environment name."
  (and (stringp value)
       (plusp (length value))
       (<= (length value) *mcp-environment-name-maximum-characters*)
       (let ((first-character (char value 0)))
         (or (and (<= (char-code first-character) 127)
                  (alpha-char-p first-character))
             (char= first-character #\_)))
       (loop for character across value
             always (or (and (<= (char-code character) 127)
                             (alphanumericp character))
                        (char= character #\_)))))

(-> mcp-configuration--http-header-name-p (t) boolean)

(defun mcp-configuration--http-header-name-p (value)
  "Return true when VALUE is a non-reserved HTTP token header name."
  (and (stringp value)
       (plusp (length value))
       (<= (length value) *mcp-http-header-name-maximum-characters*)
       (loop for character across value
             always
             (or (and (<= (char-code character) 127)
                      (alphanumericp character))
                 (find character "!#$%&'*+-.^_`|~" :test #'char=)))
       (not (member value
                    '("content-type" "accept" "mcp-session-id"
                      "mcp-protocol-version" "last-event-id")
                    :test #'string-equal))))

(-> mcp-configuration--binding (t &key (:header-p boolean)) mcp-environment-binding)

(defun mcp-configuration--binding (form &key header-p)
  "Parse one environment-backed process or HTTP binding FORM."
  (unless
      (and (mcp-configuration--proper-list-p form) (= (length form) 3)
           (eq (second form) :environment))
    (mcp-configuration--error
     "An MCP environment binding must be (TARGET :ENVIRONMENT SOURCE)."))
  (let ((target (first form)) (source (third form)))
    (unless
        (if header-p
            (mcp-configuration--http-header-name-p target)
            (mcp-configuration--environment-name-p target))
      (mcp-configuration--error
       (format nil "Invalid MCP ~A name."
               (if header-p
                   "HTTP header"
                   "environment"))))
    (unless (mcp-configuration--environment-name-p source)
      (mcp-configuration--error "Invalid MCP source environment name."))
    (make-instance 'mcp-environment-binding :target (copy-seq target) :source
                   (copy-seq source))))

(-> mcp-configuration--bindings (t &key (:header-p boolean)) list)

(defun mcp-configuration--bindings (forms &key header-p)
  "Validate and copy unique environment-backed binding FORMS."
  (unless (mcp-configuration--proper-list-p forms)
    (mcp-configuration--error "MCP environment bindings must be a proper list."))
  (let ((maximum
         (if header-p
             *mcp-http-maximum-header-bindings*
             *mcp-stdio-maximum-environment-bindings*)))
    (when (> (length forms) maximum)
      (mcp-configuration--error
       (format nil "MCP ~A bindings exceed the limit of ~D."
               (if header-p
                   "HTTP header"
                   "environment")
               maximum)
       :field
       (if header-p
           :headers
           :environment))))
  (let ((bindings
         (mapcar (lambda (form) (mcp-configuration--binding form :header-p header-p))
                 forms))
        (seen (make-hash-table :test #'equalp)))
    (dolist (binding bindings)
      (let ((target (mcp-environment-binding-target binding)))
        (when (gethash target seen)
          (mcp-configuration--error
           (format nil "Duplicate MCP binding target ~S." target)))
        (setf (gethash target seen) t)))
    bindings))

(-> mcp-configuration--positive-timeout (t keyword) real)

(defun mcp-configuration--positive-timeout (value field)
  "Return a positive bounded real timeout VALUE or reject FIELD."
  (unless
      (and (realp value) (plusp value)
           (<= value *mcp-configuration-maximum-timeout-seconds*))
    (mcp-configuration--error
     (format nil "MCP timeout ~S must be positive and no greater than ~D seconds."
             field *mcp-configuration-maximum-timeout-seconds*)
     :field field))
  value)

(-> mcp-configuration--stdio (list &key (:default-directory t))
    mcp-stdio-transport-configuration)

(defun mcp-configuration--stdio (form &key (default-directory ':workspace))
  "Parse one native standard-input and standard-output transport FORM."
  (mcp-configuration--validate-plist form
                                     '(:type :command :arguments :directory
                                       :environment))
  (let* ((command (mcp-configuration--property form :command :required-p t))
         (arguments (mcp-configuration--property form :arguments))
         (directory
          (if (mcp-configuration--property-present-p form :directory)
              (mcp-configuration--property form :directory)
              default-directory))
         (environment (mcp-configuration--property form :environment)))
    (unless
        (mcp-configuration--bounded-string-p command
                                             *mcp-stdio-command-maximum-characters*)
      (mcp-configuration--error
       "An MCP stdio command must be a bounded non-empty string." :field ':command))
    (unless
        (and (mcp-configuration--proper-list-p arguments)
             (<= (length arguments) *mcp-stdio-maximum-arguments*)
             (every
              (lambda (argument)
                (mcp-configuration--bounded-string-p argument
                                                     *mcp-stdio-argument-maximum-characters*
                                                     :empty-p t))
              arguments))
      (mcp-configuration--error
       "MCP stdio arguments must be a bounded proper list of bounded strings." :field
       ':arguments))
    (unless
        (or (eq directory :workspace)
            (mcp-configuration--bounded-string-p directory
                                                 *mcp-stdio-directory-maximum-characters*))
      (mcp-configuration--error
       "An MCP stdio directory must be :WORKSPACE or a bounded non-empty string."
       :field ':directory))
    (make-instance 'mcp-stdio-transport-configuration :command (copy-seq command)
                   :arguments (mapcar #'copy-seq arguments) :directory
                   (if (stringp directory)
                       (copy-seq directory)
                       directory)
                   :environment-bindings (mcp-configuration--bindings environment))))

(-> mcp-configuration--split-string (string character) list)

(defun mcp-configuration--split-string (value separator)
  "Split VALUE at every SEPARATOR while preserving empty components."
  (loop with start = 0
        for position = (position separator value :start start)
        collect (subseq value start position)
        while position
        do (setf start (1+ position))))

(-> mcp-configuration--domain-host-p (string) boolean)

(defun mcp-configuration--domain-host-p (host)
  "Return true when HOST is a bounded ASCII DNS name."
  (let ((name
          (if (and (plusp (length host))
                   (char= (char host (1- (length host))) #\.))
              (subseq host 0 (1- (length host)))
              host)))
    (and (plusp (length name))
         (<= (length name) 253)
         (every
          (lambda (label)
            (and (plusp (length label))
                 (<= (length label) 63)
                 (let ((first-character (char label 0))
                       (last-character (char label (1- (length label))))
                       (ascii-alphanumeric-p
                         (lambda (character)
                           (and (<= (char-code character) 127)
                                (alphanumericp character)))))
                   (and (funcall ascii-alphanumeric-p first-character)
                        (funcall ascii-alphanumeric-p last-character)
                        (loop for character across label
                              always
                              (or
                               (funcall ascii-alphanumeric-p character)
                               (char= character #\-)))))))
          (mcp-configuration--split-string name #\.)))))

(-> mcp-configuration--http-host-p (string) boolean)

(defun mcp-configuration--http-host-p (host)
  "Return true when HOST is a validated DNS, IPv4, or bracketed IPv6 host."
  (cond
    ((ip-addr-p host)
     t)
    ((find #\: host)
     nil)
    ((loop for character across host
           always (or (digit-char-p character 10)
                      (char= character #\.)))
     nil)
    (t
     (mcp-configuration--domain-host-p host))))

(-> mcp-configuration--loopback-host-p (string) boolean)

(defun mcp-configuration--loopback-host-p (host)
  "Return true only when HOST denotes an unambiguous loopback address."
  (or (string-equal host "localhost")
      (string-equal host "localhost.")
      (and
       (ipv4-addr-p host)
       (let ((first-dot (position #\. host)))
         (and first-dot
              (string= (subseq host 0 first-dot) "127"))))
      (and
       (ipv6-addr-p host)
       (ip-addr= host "[::1]"))))

(-> mcp-configuration--url-authority-port-syntax-p (string) boolean)

(defun mcp-configuration--url-authority-port-syntax-p (url)
  "Return true when URL's authority has a syntactically valid optional port."
  (let ((scheme-end (search "://" url)))
    (unless scheme-end
      (return-from mcp-configuration--url-authority-port-syntax-p nil))
    (let* ((authority-start (+ scheme-end 3))
           (authority-end
             (or (position-if
                  (lambda (character)
                    (member character '(#\/ #\? #\#) :test #'char=))
                  url
                  :start authority-start)
                 (length url)))
           (authority (subseq url authority-start authority-end)))
      (unless (plusp (length authority))
        (return-from mcp-configuration--url-authority-port-syntax-p nil))
      (labels ((decimal-port-p (value)
                 "Return true when VALUE is a non-empty decimal port."
                 (and (plusp (length value))
                      (loop for character across value
                            always
                            (and (<= (char-code character) 127)
                                 (digit-char-p character 10))))))
        (if (char= (char authority 0) #\[)
            (let ((closing-bracket (position #\] authority)))
              (and closing-bracket
                   (let ((tail (subseq authority (1+ closing-bracket))))
                     (or (zerop (length tail))
                         (and (char= (char tail 0) #\:)
                              (decimal-port-p (subseq tail 1)))))))
            (let ((colon (position #\: authority)))
              (or (null colon)
                  (and (= colon (position #\: authority :from-end t))
                       (decimal-port-p
                        (subseq authority (1+ colon)))))))))))

(-> mcp-configuration--validate-http-url (t) string)

(defun mcp-configuration--validate-http-url (url)
  "Return URL after strict Streamable HTTP endpoint validation."
  (unless (mcp-configuration--bounded-string-p url *mcp-http-url-maximum-characters*)
    (mcp-configuration--error
     "An MCP Streamable HTTP URL must be a bounded non-empty string." :field ':url))
  (handler-case
   (let* ((uri (uri url))
          (scheme (uri-scheme uri))
          (host (uri-host uri))
          (port (uri-port uri)))
     (unless
         (and (member scheme '("http" "https") :test #'string-equal)
              (mcp-configuration--url-authority-port-syntax-p url) (stringp host)
              (mcp-configuration--http-host-p host) (integerp port) (<= 1 port 65535))
       (mcp-configuration--error
        "An MCP HTTP URL must contain a valid HTTP host and port." :field ':url))
     (unless
         (or (string-equal scheme "https")
             (and (string-equal scheme "http")
                  (mcp-configuration--loopback-host-p host)))
       (mcp-configuration--error
        "An MCP HTTP URL must use HTTPS unless its host is loopback." :field ':url)))
   (mcp-configuration-error (condition) (error condition))
   (error (cause) (declare (ignore cause))
          (mcp-configuration--error "Invalid MCP Streamable HTTP URL." :field ':url)))
  url)

(-> mcp-configuration--http (list &key (:default-connect-timeout-seconds real))
    mcp-http-transport-configuration)

(defun mcp-configuration--http (form &key (default-connect-timeout-seconds 10))
  "Parse one native Streamable HTTP transport FORM."
  (mcp-configuration--validate-plist form
                                     '(:type :url :headers :connect-timeout-seconds))
  (let* ((url (mcp-configuration--property form :url :required-p t))
         (headers (mcp-configuration--property form :headers))
         (connect-timeout
          (if (mcp-configuration--property-present-p form :connect-timeout-seconds)
              (mcp-configuration--property form :connect-timeout-seconds)
              default-connect-timeout-seconds)))
    (mcp-configuration--validate-http-url url)
    (make-instance 'mcp-http-transport-configuration :url (copy-seq url)
                   :header-bindings (mcp-configuration--bindings headers :header-p t)
                   :connect-timeout-seconds
                   (mcp-configuration--positive-timeout connect-timeout
                                                        :connect-timeout-seconds))))

(-> mcp-read-transport-configuration
    (t &key (:default-directory t) (:default-connect-timeout-seconds t)
            (:maximum-timeout-seconds real))
    mcp-transport-configuration)

(defun mcp-read-transport-configuration
    (form &key (default-directory ':workspace)
               (default-connect-timeout-seconds 10)
               (maximum-timeout-seconds *mcp-configuration-maximum-timeout-seconds*))
  "Validate FORM and return a detached, credential-free transport configuration.

Environment and header bindings contain variable names, never resolved values.
This reader does not inspect the environment, resolve paths, or open transports."
  (unless (and (realp maximum-timeout-seconds) (plusp maximum-timeout-seconds))
    (mcp-configuration--error "The maximum transport timeout must be positive."
                              :field ':connect-timeout-seconds))
  (mcp-configuration--validate-plist
   form '(:type :command :arguments :directory :environment :url :headers
          :connect-timeout-seconds))
  (let ((*mcp-configuration-maximum-timeout-seconds* maximum-timeout-seconds))
    (case (mcp-configuration--property form :type :required-p t)
      (:stdio
       (mcp-configuration--stdio form :default-directory default-directory))
      (:http
       (mcp-configuration--positive-timeout default-connect-timeout-seconds
                                            ':connect-timeout-seconds)
       (mcp-configuration--http
        form :default-connect-timeout-seconds default-connect-timeout-seconds))
      (otherwise
       (mcp-configuration--error "Unsupported MCP transport type." :field ':type)))))
