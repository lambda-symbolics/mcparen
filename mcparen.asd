(asdf:defsystem #:mcparen
  :description "A small Model Context Protocol client for Common Lisp."
  :author "Lukáš Hozda"
  :license "ISC"
  :version "0.1.0"
  :serial t
  :depends-on (#:bordeaux-threads
               #:dexador
               #:ls-compat
               #:ls-compat/posix
               #:serapeum
               #:yason)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "mcp-conditions")
                             (:file "json")
                             (:file "mcp-transport")
                             (:file "mcp-http")
                             (:file "mcp-stdio")
                             (:file "mcp-client"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:mcparen/tests))))

(asdf:defsystem #:mcparen/managed
  :description "Shared restartable MCP connections and bounded discovery snapshots."
  :depends-on (#:mcparen #:babel)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "managed")
                             (:file "managed-operations")
                             (:file "managed-discovery")))))

(asdf:defsystem #:mcparen/tests
  :description "Tests for Mcparen."
  :depends-on (#:mcparen/managed)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "test-support")
                             (:file "mcp-tests")
                             (:file "managed-tests")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:mcparen '#:run-tests)))
