(in-package #:mcparen)

;;;; -- Declarative Transport Configuration --

(define-test configuration-reads-stdio-and-detaches-values
  (let* ((form (list :type :stdio
                     :command (copy-seq "server")
                     :arguments (list (copy-seq "--mode") (copy-seq "test"))
                     :environment (list (list "API_KEY" :environment "API_KEY"))))
         (configuration
           (mcp-read-transport-configuration form
                                              :default-directory "/work/project"))
         (environment (first (mcp-stdio-configuration-environment-bindings
                              configuration))))
    (test-assert (typep configuration 'mcp-stdio-transport-configuration))
    (test-equal "server" (mcp-stdio-configuration-command configuration)
                :test #'string=)
    (test-equal '("--mode" "test")
                (mcp-stdio-configuration-arguments configuration)
                :test #'equal)
    (test-equal "/work/project" (mcp-stdio-configuration-directory configuration)
                :test #'string=)
    (test-equal "API_KEY" (mcp-environment-binding-target environment)
                :test #'string=)
    (test-equal "API_KEY" (mcp-environment-binding-source environment) :test #'string=)
    (setf (char (getf form :command) 0) #\X
          (char (first (getf form :arguments)) 0) #\X)
    (test-equal "server" (mcp-stdio-configuration-command configuration)
                :test #'string=)
    (test-equal "--mode"
                (first (mcp-stdio-configuration-arguments configuration))
                :test #'string=)
    (test-equal "test"
                (second (mcp-stdio-configuration-arguments configuration))
                :test #'string=)))

(define-test configuration-reads-http-and-preserves-header-bindings
  (let ((configuration
          (mcp-read-transport-configuration
           '(:type :http
             :url "https://example.com:443/mcp"
             :headers (("Authorization" :environment "TOKEN")
                        ("X-Trace" :environment "TRACE"))
             :connect-timeout-seconds 15))))
    (test-assert (typep configuration 'mcp-http-transport-configuration))
    (test-equal "https://example.com:443/mcp"
                (mcp-http-configuration-url configuration)
                :test #'string=)
    (test-equal 15 (mcp-http-configuration-connect-timeout-seconds configuration))
    (let ((bindings (mcp-http-configuration-header-bindings configuration)))
      (test-equal 2 (length bindings))
      (test-equal "Authorization"
                  (mcp-environment-binding-target (first bindings))
                  :test #'string=)
      (test-equal "TOKEN"
                  (mcp-environment-binding-source (first bindings)) :test #'string=))))

(define-test configuration-defaults-directory-and-timeout
  (let ((stdio (mcp-read-transport-configuration
                '(:type :stdio :command "server")
                :default-directory "/tmp/mcp"))
        (http (mcp-read-transport-configuration
               '(:type :http :url "http://localhost/mcp"))))
    (test-equal "/tmp/mcp" (mcp-stdio-configuration-directory stdio)
                :test #'string=)
    (test-equal 10 (mcp-http-configuration-connect-timeout-seconds http))))

(define-test configuration-rejects-structural-plist-errors
  (dolist (form (list '(:type :stdio :command "x" :unknown t)
                     '(:type :stdio :command "x" :command "y")
                     '(:type :stdio :command "x" :arguments ("a" . "b"))
                     '(:type :stdio :command "x" :environment (("A" :environment)))
                     '(:type :stdio :command "x" :environment (("A" :environment "X" "extra")))
                     '(:type :stdio :command "x" :environment (("A" :environment "X") ("a" :environment "Y")))
                     '(:type :http :url "http://localhost" :headers (("X" :environment "A") ("x" :environment "B")))
                     '(:type :stdio :command)
                     '(:type :http :url "http://localhost" :headers (("Accept" :environment "A")))
                     '(:type :stdio :command "x" :environment (("Á" :environment "X")))))
    (test-signals mcp-configuration-error
      (mcp-read-transport-configuration form))))

(define-test configuration-rejects-invalid-http-authorities
  (dolist (url '("http://user:secret@example.com/mcp"
                 "https://user:secret@example.com/mcp"
                 "http://127.0.0.999/mcp"
                 "http://127.0.0.1:0/mcp"
                 "http://127.0.0.1:65536/mcp"
                 "http://[::2]/mcp"
                 "http://example.com/mcp"
                 "ftp://example.com/mcp"))
    (test-signals mcp-configuration-error
      (mcp-read-transport-configuration (list :type :http :url url)))))

(define-test configuration-accepts-supported-http-authorities
  (dolist (url '("https://example.com/mcp"
                 "https://user@example.com/mcp"
                 "https://[::1]:443/mcp"
                 "http://localhost/mcp"
                 "http://127.0.0.1:8080/mcp"
                 "http://[::1]:8080/mcp"))
    (test-assert
     (typep (mcp-read-transport-configuration (list :type :http :url url))
            'mcp-http-transport-configuration))))

(define-test configuration-bounds-stdio-fields
  (dolist (form (list (list :type :stdio :command (make-string 4097 :initial-element #\x))
                     (list :type :stdio :command "x"
                           :arguments (list (make-string 8193 :initial-element #\x)))
                     (list :type :stdio :command "x"
                           :arguments (loop repeat 129 collect "x"))
                     (list :type :stdio :command "x"
                           :directory (make-string 4097 :initial-element #\x))))
    (test-signals mcp-configuration-error
      (mcp-read-transport-configuration form))))


(define-test configuration-rejects-circular-and-improper-forms
  (let ((circular (list :type :stdio :command "server")))
    (setf (cdr (last circular)) circular)
    (test-signals mcp-configuration-error
      (mcp-read-transport-configuration circular)))
  (dolist (form '((:type :stdio . "bad")
                  ("type" :stdio :command "server")
                  (:type :http :url nil)))
    (test-signals mcp-configuration-error
      (mcp-read-transport-configuration form))))
