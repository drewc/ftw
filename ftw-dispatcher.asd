(asdf:defsystem #:ftw-dispatcher
  :author "Drew Crampsie <drewc@tech.coop>"
  :licence "MIT"
  :components ((:module :dispatcher
		:components ((:file "dispatcher")
			     (:file "dispatcher-api")
			     (:file "dispatcher-syntax")
			     (:file "dispatcher-package")
			     (:file "monads")
			     (:file "functional-dispatcher"))
		:serial t))
  :depends-on (:ftw-request-context))