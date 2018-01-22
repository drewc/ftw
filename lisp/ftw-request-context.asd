
(asdf:defsystem #:ftw-request-context
  :author "Drew Crampsie <drewc@tech.coop>"
  :licence "MIT"
  :components (( :module :request-context
		:components ((:file "request-context-package")
			     (:file "request-context")
			     (:file "request-context-syntax")
			     (:file "strings")
			     (:file "http-utilities")
			     (:file "http-status-codes"))
		:serial t))

  :depends-on (#:contextl #:arnesi #:hunchentoot))