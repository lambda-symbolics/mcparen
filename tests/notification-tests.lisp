(in-package #:mcparen)

;;;; -- Notification and Subscription Tests --

(-> test-subscription-meta (t) hash-table)
(defun test-subscription-meta (identifier)
  "Build modern metadata for subscription IDENTIFIER."
  (json-object "io.modelcontextprotocol/subscriptionId" identifier))

(define-test modern-stdio-demultiplexes-interleaved-subscriptions
  (let* ((transport (make-test-stdio-transport))
         (client    (make-mcp-client transport :protocol-mode ':auto :startup-timeout 3)))
    (unwind-protect
         (let* ((filter (json-object "toolsListChanged" t "promptsListChanged" t))
                (first  (mcp-client-listen client filter))
                (second (mcp-client-listen client filter)))
           (test-equal "2026-07-28" (mcp-client-protocol-version client))
           (test-assert (/= (mcp-subscription-id first) (mcp-subscription-id second)))
           (test-assert (json-true-p (json-get (mcp-subscription-acknowledged-filter first)
                                             "toolsListChanged")))
           (test-assert (not (nth-value 1 (gethash "promptsListChanged"
                                                 (mcp-subscription-acknowledged-filter first)))))
           (multiple-value-bind (event status) (mcp-subscription-next first :timeout 2)
             (test-equal ':notification status)
             (test-equal "notifications/tools/list_changed" (json-get event "method"))
             (test-equal (mcp-subscription-id first)
                         (mcp-subscription-wire-id (json-get event "params"))))
           (test-equal ':complete (nth-value 1 (mcp-subscription-next second :timeout 2)))
           (mcp-subscription-cancel first)
           (test-equal ':cancelled (nth-value 1 (mcp-subscription-next first :timeout 2)))
           (test-assert
            (test-wait-until
             (lambda ()
               (search (format nil "cancelled:~D" (mcp-subscription-id first))
                       (mcp-stdio-transport-stderr-text transport)))
             2)))
      (mcp-client-close client))))

(define-test legacy-resource-subscriptions-use-the-negotiated-server-capability
  (dolist (version '("2025-06-18" "2025-11-25"))
    (dolist (supports-p (list t (argo:json-false)))
      (let* ((transport
               (make-test-scripted-transport
                (lambda (transport request)
                  (declare (ignore transport))
                  (test-rpc-result
                   request
                   (if (equal (json-get request "method") "initialize")
                       (json-object "protocolVersion" version
                                    "capabilities" (json-object "resources" (json-object "subscribe" supports-p))
                                    "serverInfo" (json-object "name" "test" "version" "1"))
                       (json-object))))))
             (client (make-mcp-client transport))
             (uri    "file:///example"))
        (unwind-protect
             (if (eq supports-p t)
                 (progn
                   (test-assert (hash-table-p (mcp-client-subscribe-resource client uri)))
                   (test-assert (hash-table-p (mcp-client-unsubscribe-resource client uri)))
                   (test-equal '("initialize" "resources/subscribe" "resources/unsubscribe")
                               (mapcar (lambda (request) (json-get request "method"))
                                       (test-scripted-transport-requests transport)))
                   (dolist (request (rest (test-scripted-transport-requests transport)))
                     (test-equal uri (json-get (json-get request "params") "uri"))
                     (test-assert (integerp (json-get request "id"))))
                   (test-equal '("notifications/initialized")
                               (mapcar (lambda (message) (json-get message "method"))
                                       (test-scripted-transport-notifications transport))))
                 (progn
                   (test-signals mcp-protocol-error (mcp-client-subscribe-resource client uri))
                   (test-signals mcp-protocol-error (mcp-client-unsubscribe-resource client uri))
                   (test-equal 1 (length (test-scripted-transport-requests transport)))))
          (mcp-client-close client))))))

(define-test modern-http-mirrors-headers-and-distinguishes-complete-from-drop
  (labels ((respond (server exchange)
             "Serve modern discovery and one stateless SSE response per request."
             (declare (ignore server))
             (let* ((request (json-decode (test-http-request-body exchange)))
                    (method  (json-get request "method"))
                    (params  (json-get request "params"))
                    (meta    (json-get params "_meta"))
                    (id      (json-get request "id")))
               (test-equal "POST" (test-http-request-method exchange))
               (test-equal "2026-07-28" (test-http--header exchange "mcp-protocol-version"))
               (test-equal "2026-07-28" (json-get meta "io.modelcontextprotocol/protocolVersion"))
               (test-equal "mcparen" (json-get (json-get meta "io.modelcontextprotocol/clientInfo") "name"))
               (test-assert (hash-table-p (json-get meta "io.modelcontextprotocol/clientCapabilities")))
               (test-equal method (test-http--header exchange "mcp-method"))
               (test-assert (not (test-http--header exchange "mcp-session-id")))
               (cond
                 ((equal method "server/discover")
                  (values 200 (list (cons "Content-Type" "application/json")
                                    (cons "Mcp-Session-Id" "ignored"))
                          (test-json-response-body
                           request (json-object "resultType" "complete" "supportedVersions" #("2026-07-28")
                                                 "capabilities" (json-object "resources" (json-object)
                                                                             "tools" (json-object))))))
                 ((equal method "tools/list")
                  (let* ((flag    (json-object "type" "boolean" "x-mcp-header" "Active"))
                         (nested  (json-object "type" "object" "properties" (json-object "active" flag)))
                         (valid   (json-object "type" "object" "properties" (json-object "nested" nested)))
                         (invalid (json-object "type" "object" "x-mcp-header" "root")))
                    (values 200 (list (cons "Content-Type" "application/json"))
                            (test-json-response-body
                             request (json-object "resultType" "complete"
                                                  "tools" (vector
                                                           (json-object "name" "invalid" "inputSchema" invalid)
                                                           (json-object "name" "valid" "inputSchema" valid)))))))
                 ((equal method "tools/call")
                  (test-equal "valid" (test-http--header exchange "mcp-name"))
                  (test-equal "false" (test-http--header exchange "mcp-param-active"))
                  (test-equal "valid" (json-get params "name"))
                  (values 200 (list (cons "Content-Type" "application/json"))
                          (test-json-response-body request (json-object "resultType" "complete" "content" #()))))
                 ((equal method "resources/read")
                  (test-equal "=?base64?ZmlsZTovLy/DqQ==?=" (test-http--header exchange "mcp-name"))
                  (values 200 (list (cons "Content-Type" "application/json"))
                          (test-json-response-body request (json-object "resultType" "complete" "contents" #()))))
                 ((equal method "subscriptions/listen")
                  (let* ((filter   (json-get params "notifications"))
                         (ack      (json-object "jsonrpc" "2.0" "method" "notifications/subscriptions/acknowledged"
                                           "params" (json-object "_meta" (test-subscription-meta id)
                                                                 "notifications" filter)))
                         (complete (test-rpc-result
                                    request (json-object "resultType" "complete" "_meta" (test-subscription-meta id)))))
                    (values 200 (list (cons "Content-Type" "text/event-stream"))
                            (if (json-true-p (json-get filter "toolsListChanged"))
                                (test-sse-body ack complete)
                                (test-sse-body ack)))))
                 (t
                  (error "Unexpected method ~S" method))))))
    (with-test-http-server (server #'respond)
      (with-mcp-client (client (make-mcp-client (make-mcp-streamable-http-transport (test-http-server-url server))
                                              :protocol-mode ':auto :startup-timeout 3))
        (mcp-client-read-resource client "file:///é")
        (let ((tools (handler-bind ((warning #'muffle-warning)) (mcp-client-list-tools client))))
          (test-equal '("valid") (mapcar #'mcp-tool-name tools))
          (mcp-client-call-tool client (first tools) (json-object "nested" (json-object "active" (argo:json-false)))))
        (dolist (outcome '(:complete :dropped))
          (let ((subscription (mcp-client-listen client (json-object (if (eq outcome ':complete)
                                                                        "toolsListChanged"
                                                                        "promptsListChanged") t))))
            (test-equal outcome (nth-value 1 (mcp-subscription-next subscription :timeout 2)))))
        (test-assert (not (mcp-http-transport-session-identifier (mcp-client-transport client))))))))

(define-test modern-http-routing-uses-static-paths-and-typed-values
  (let* ((schema
           (json-object "properties"
                        (json-object "flag" (json-object "type" "boolean" "x-mcp-header" "flag")
                                     "missing" (json-object "type" "string" "x-mcp-header" "missing")
                                     "nested" (json-object "properties"
                                                           (json-object "count" (json-object "type" "integer"
                                                                                             "x-mcp-header" "count"))))))
         (headers (mcp-http--tool-routing-headers schema
                                                (json-object "flag" (argo:json-false)
                                                             "nested" (json-object "count" -42)))))
    (test-equal "false" (mcp-http--header-value headers "Mcp-Param-flag"))
    (test-equal "-42" (mcp-http--header-value headers "Mcp-Param-count"))
    (test-assert (not (mcp-http--header-value headers "Mcp-Param-missing")))
    (test-signals mcp-invalid-tool-header
      (mcp-http--tool-header-paths
       (json-object "anyOf" (vector (json-object "type" "string" "x-mcp-header" "wrong")))))))

(define-test modern-stdio-subscription-overflow-does-not-block-a-reply
  (with-mcp-client (client (make-mcp-client (make-test-stdio-transport)
                                           :protocol-mode ':auto :startup-timeout 3))
    (let ((subscription (mcp-client-listen client (json-object "toolsListChanged" t))))
      (mcp-client--request client "fixture/flood-subscription" :timeout 2)
      (dotimes (index 128)
        (multiple-value-bind (event status) (mcp-subscription-next subscription :timeout 2)
          (test-equal ':notification status)
          (test-equal index (json-get (json-get event "params") "index"))))
      (multiple-value-bind (event status failure) (mcp-subscription-next subscription :timeout 2)
        (test-assert (not event))
        (test-equal ':failed status)
        (test-assert (typep failure 'mcp-protocol-error))))))

(define-test modern-stdio-restores-subscription-after-unexpected-exit
  (let ((transport (make-test-stdio-transport)))
    (with-mcp-client (client (make-mcp-client transport :protocol-mode ':auto :startup-timeout 3))
      (let* ((subscription (mcp-client-listen client (json-object "toolsListChanged" t)))
             (original-id  (mcp-subscription-id subscription))
             (process      (mcp-stdio-transport-process transport)))
        (test-signals mcp-transport-error
          (mcp-client--request client "fixture/exit-unexpectedly" :timeout 3))
        (mcp-client-connect client)
        (test-assert (not (eq process (mcp-stdio-transport-process transport))))
        (test-assert (test-wait-until
                      (lambda () (/= original-id (mcp-subscription-id subscription))) 3)
                     "Reconnection must reissue the active subscription with a fresh ID.")
        (mcp-client--request client "fixture/subscription-event" :timeout 3)
        (multiple-value-bind (event status) (mcp-subscription-next subscription :timeout 3)
          (test-equal ':notification status)
          (test-equal (mcp-subscription-id subscription)
                      (mcp-subscription-wire-id (json-get event "params"))))))))

(define-test modern-stdio-restores-concurrent-streams-and-keeps-queued-events
  (let ((transport (make-test-stdio-transport :fixture-arguments '("persistent")))
        (request   (json-object "toolsListChanged" t)))
    (with-mcp-client (client (make-mcp-client transport :protocol-mode ':auto :startup-timeout 3))
      (let* ((first-stream  (mcp-client-listen client request))
             (second-stream (mcp-client-listen client request))
             (cancelled     (mcp-client-listen client request))
             (streams       (list first-stream second-stream))
             (original-ids  (mapcar #'mcp-subscription-id streams))
             (generation    (mcp-client-connection-generation client)))
        (mcp-subscription-cancel cancelled)
        (mcp-client--request client "fixture/subscription-event" :timeout 3)
        (test-signals mcp-transport-error
          (mcp-client--request client "fixture/exit-unexpectedly" :timeout 3))
        ;; Only the subscription workers can start the replacement here.
        (test-assert (test-wait-until
                      (lambda () (every #'> (mapcar #'mcp-subscription-id streams) original-ids)) 5))
        (let ((state (mcp-client--request client "fixture/subscription-state" :timeout 3)))
          (test-equal 2 (json-get state "listens"))
          (test-equal -1 (json-get state "cancelled")))
        (test-equal (1+ generation) (mcp-client-connection-generation client))
        (mcp-client--request client "fixture/subscription-event" :timeout 3)
        (loop for stream in streams
              for original-id in original-ids
              do (dolist (identifier (list original-id (mcp-subscription-id stream)))
                   (multiple-value-bind (event status) (mcp-subscription-next stream :timeout 3)
                     (test-equal ':notification status)
                     (test-equal identifier (mcp-subscription-wire-id (json-get event "params"))))))
        (mcp-subscription-cancel first-stream)
        (let ((state (mcp-client--request client "fixture/subscription-state" :timeout 3)))
          (test-equal (mcp-subscription-id first-stream) (json-get state "cancelled")))
        (test-equal ':cancelled (nth-value 1 (mcp-subscription-next first-stream :timeout 1)))
        (mcp-client-close client)
        (test-equal ':dropped (nth-value 1 (mcp-subscription-next second-stream :timeout 1)))
        (mcp-client-connect client)
        (test-equal 0 (json-get (mcp-client--request client "fixture/subscription-state" :timeout 3) "listens"))))))

(define-test modern-stdio-restoration-requires-a-fresh-acknowledgment
  (dolist (mode '(:no-ack :invalid-ack :early-event :exit :cancel :close))
    (let* ((seen-ack-p nil)
           (transport
             (make-test-stdio-transport
              :fixture-arguments '("persistent")
              :ingress-projector
              (lambda (kind message)
                (if (and (eq kind ':notification)
                         (equal "notifications/subscriptions/acknowledged" (json-get message "method")))
                    (cond
                      ((not seen-ack-p)
                       (setf seen-ack-p t)
                       message)
                      ((eq mode ':invalid-ack)
                       (setf (gethash "notifications" (json-get message "params"))
                             (json-object "promptsListChanged" t))
                       message)
                      (t
                       nil))
                    message)))))
      (with-mcp-client (client (make-mcp-client transport :protocol-mode ':auto :startup-timeout 3))
        (let* ((subscription (mcp-client-listen client (json-object "toolsListChanged" t) :timeout 1))
               (original-id  (mcp-subscription-id subscription))
               (generation   (mcp-client-connection-generation client)))
          (test-signals mcp-transport-error
            (mcp-client--request client "fixture/exit-unexpectedly" :timeout 3))
          (test-assert (test-wait-until (lambda () (> (mcp-subscription-id subscription) original-id)) 5))
          (ecase mode
            (:no-ack
             (multiple-value-bind (event status failure) (mcp-subscription-next subscription :timeout 3)
               (test-assert (not event))
               (test-equal ':dropped status)
               (test-assert (typep failure 'mcp-timeout))))
            ((:invalid-ack :early-event)
             (when (eq mode ':early-event)
               (mcp-client--request client "fixture/subscription-event" :timeout 3))
             (multiple-value-bind (event status failure) (mcp-subscription-next subscription :timeout 3)
               (test-assert (not event))
               (test-equal ':failed status)
               (test-assert (typep failure 'mcp-protocol-error))))
            (:exit
             (test-signals mcp-transport-error
               (mcp-client--request client "fixture/exit-unexpectedly" :timeout 3))
             (test-equal ':dropped (nth-value 1 (mcp-subscription-next subscription :timeout 3)))
             (test-equal (1+ generation) (mcp-client-connection-generation client))
             (test-assert (not (mcp-transport-open-p transport))))
            (:cancel
             (mcp-subscription-cancel subscription)
             (test-equal ':cancelled (nth-value 1 (mcp-subscription-next subscription :timeout 1))))
            (:close
             (mcp-client-close client)
             (test-equal ':dropped (nth-value 1 (mcp-subscription-next subscription :timeout 1)))
             (test-assert (not (mcp-transport-open-p transport))))))))))

(define-test modern-stdio-close-and-cancel-wake-a-waiting-consumer
  (dolist (action '(:close :cancel))
    (let ((client (make-mcp-client (make-test-stdio-transport) :protocol-mode ':auto :startup-timeout 3)))
      (unwind-protect
           (let* ((subscription (mcp-client-listen client (json-object "toolsListChanged" t)))
                  (reader       (test-start-thread
                                 (lambda () (nth-value 1 (mcp-subscription-next subscription)))
                                 "subscription consumer")))
             (test-assert (not (test-wait-until (lambda () (test-thread-result-finished-p reader)) 0.05)))
             (ecase action
               (:close
                (mcp-client-close client))
               (:cancel
                (mcp-subscription-cancel subscription)))
             (test-assert (test-wait-until (lambda () (test-thread-result-finished-p reader)) 2))
             (test-equal (if (eq action ':close) ':dropped ':cancelled)
                         (test-thread-result-value reader)))
        (mcp-client-close client)))))

(define-test modern-http-legacy-fallback-reads-400-before-initialize
  (dolist (modern-error-p '(nil t))
    (let ((initialization-count 0))
      (labels ((respond (server exchange)
                 "Return recognized modern rejection or an unrecognized legacy 400."
                 (declare (ignore server))
                 (if (string= (test-http-request-method exchange) "GET")
                     (values 405 nil "")
                     (let* ((request (json-decode (test-http-request-body exchange)))
                            (method  (json-get request "method"))
                            (headers (list (cons "Content-Type" "application/json"))))
                       (cond
                         ((equal method "server/discover")
                          (values 400 headers
                                  (if modern-error-p
                                      (json-encode (json-object "jsonrpc" "2.0" "id" (json-get request "id")
                                                                "error" (json-object "code" -32022 "message" "version"
                                                                                     "data" (json-object "supported" #("1900-01-01")))))
                                      "")))
                         ((equal method "initialize")
                          (incf initialization-count)
                          (test-assert (not (test-http--header exchange "mcp-method")))
                          (values 200 headers (test-json-response-body
                                               request (json-object "protocolVersion" "2025-06-18"
                                                                    "capabilities" (json-object)
                                                                    "serverInfo" (json-object "name" "fixture" "version" "1")))))
                         ((equal method "notifications/initialized")
                          (values 202 nil ""))
                         (t
                          (error "Unexpected method ~S" method)))))))
        (with-test-http-server (server #'respond)
          (let ((client (make-mcp-client (make-mcp-streamable-http-transport (test-http-server-url server))
                                         :protocol-mode ':auto :startup-timeout 3)))
            (unwind-protect
                 (if modern-error-p
                     (test-equal -32022 (mcp-rpc-error-code (test-signals mcp-rpc-error (mcp-client-connect client))))
                     (progn
                       (mcp-client-connect client)
                       (test-equal "2025-06-18" (mcp-client-protocol-version client))))
              (mcp-client-close client))))
        (test-equal (if modern-error-p 0 1) initialization-count)))))

(define-test modern-http-subscription-rejects-pre-ack-and-wrong-id-messages
  (dolist (scenario '(:pre-ack :wrong-ack :wrong-response :server-request))
    (labels ((respond (server exchange)
               "Return a well-formed exchange with a deliberately invalid message sequence."
               (declare (ignore server))
               (let* ((request (json-decode (test-http-request-body exchange)))
                      (id      (json-get request "id")))
                 (if (equal (json-get request "method") "server/discover")
                     (values 200 (list (cons "Content-Type" "application/json"))
                             (test-json-response-body
                              request (json-object "resultType" "complete"
                                                   "supportedVersions" #("2026-07-28")
                                                   "capabilities" (json-object))))
                     (values 200 (list (cons "Content-Type" "text/event-stream"))
                             (test-sse-body
                              (ecase scenario
                                (:pre-ack
                                 (json-object "jsonrpc" "2.0" "method" "notifications/tools/list_changed"
                                              "params" (json-object "_meta" (test-subscription-meta id))))
                                (:wrong-ack
                                 (json-object "jsonrpc" "2.0" "method" "notifications/subscriptions/acknowledged"
                                              "params" (json-object "_meta" (test-subscription-meta (1+ id))
                                                                    "notifications" (json-object))))
                                (:wrong-response
                                 (json-object "jsonrpc" "2.0" "id" (1+ id)
                                              "result" (json-object "resultType" "complete")))
                                (:server-request
                                 (json-object "jsonrpc" "2.0" "id" 42 "method" "ping")))))))))
      (with-test-http-server (server #'respond)
        (with-mcp-client (client (make-mcp-client
                                  (make-mcp-streamable-http-transport (test-http-server-url server))
                                  :protocol-mode ':auto :startup-timeout 2))
          (test-signals mcp-protocol-error
            (mcp-client-listen client (json-object "toolsListChanged" t))))))))

(define-test modern-http-subscription-stop-retires-idle-sse-reader
  (dolist (action '(:cancel :drop))
    (let ((state-lock       (make-lock "idle SSE fixture"))
          (scope            (make-instance 'test-http-exchange-scope))
          (read-line        (symbol-function 'mcp-http--read-utf8-line))
          (read-count       0)
          (idle-reader      nil)
          (release-server-p nil))
      (labels ((released-p ()
                 "Whether the fixture may close its side of the subscription."
                 (with-lock-held (state-lock) release-server-p))

               (observed-read (stream limit source)
                 "Signal entry into the real socket read following the acknowledgment."
                 (when *mcp-http-listener-stop-tag*
                   (with-lock-held (state-lock)
                     (when (= (incf read-count) 3)
                       (setf idle-reader (current-thread)))))
                 (funcall read-line stream limit source))

               (respond (server exchange)
                 "Flush an acknowledgment and leave the subscription stream idle."
                 (declare (ignore server))
                 (let* ((request (json-decode (test-http-request-body exchange)))
                        (method  (json-get request "method"))
                        (headers (list (cons "Content-Type" "application/json"))))
                   (cond
                     ((equal method "server/discover")
                      (values 200 headers
                              (test-json-response-body
                               request (json-object "resultType" "complete"
                                                    "supportedVersions" #("2026-07-28")
                                                    "capabilities" (json-object)))))
                     ((equal method "ping")
                      (values 200 headers
                              (test-json-response-body
                               request (json-object "resultType" "complete"))))
                     ((equal method "subscriptions/listen")
                      (values 200 (list (cons "Content-Type" "text/event-stream"))
                              (make-test-http-streaming-body
                               (lambda (stream)
                                 (write-string
                                  (test-sse-body
                                   (json-object "jsonrpc" "2.0"
                                                "method" "notifications/subscriptions/acknowledged"
                                                "params" (json-object
                                                          "_meta" (test-subscription-meta (json-get request "id"))
                                                          "notifications" (json-object))))
                                  stream)
                                 (finish-output stream)
                                 (test-assert (test-wait-until #'released-p 10))))))
                     (t
                      (error "Unexpected method ~S" method))))))
        (unwind-protect
             (progn
               (setf (symbol-function 'mcp-http--read-utf8-line) #'observed-read)
               (with-test-http-server (server #'respond)
                 (unwind-protect
                      (with-mcp-client (client (make-mcp-client
                                                (make-mcp-streamable-http-transport
                                                 (test-http-server-url server)
                                                 :exchange-scope-function (test-http-exchange-scope-function scope))
                                                :protocol-mode ':auto :startup-timeout 2))
                        (let* ((subscription (mcp-client-listen client (json-object)))
                               (consumer     (test-start-thread
                                              (lambda () (nth-value 1 (mcp-subscription-next subscription)))
                                              "HTTP SSE consumer"))
                               (worker       nil))
                          (test-assert (test-wait-until
                                        (lambda () (with-lock-held (state-lock) idle-reader)) 2))
                          (setf worker (with-lock-held (state-lock) idle-reader))
                          (test-assert (not (test-wait-until
                                             (lambda () (test-thread-result-finished-p consumer)) 0.05)))
                          (ecase action
                            (:cancel
                             (mcp-subscription-cancel subscription))
                            (:drop
                             (with-lock-held (state-lock) (setf release-server-p t))))
                          (test-assert (test-wait-until
                                        (lambda () (test-thread-result-finished-p consumer)) 1))
                          (test-equal (if (eq action ':cancel) ':cancelled ':dropped)
                                      (test-thread-result-value consumer))
                          (test-assert (test-wait-until (lambda () (not (thread-alive-p worker))) 1))
                          (test-assert (not (test-http-exchange-scope-active-p scope)))
                          (when (eq action ':cancel)
                            (test-assert (not (released-p))))
                          (test-assert (hash-table-p (mcp-client--request client "ping" :timeout 1)))))
                   (with-lock-held (state-lock) (setf release-server-p t)))))
          (setf (symbol-function 'mcp-http--read-utf8-line) read-line))))))
