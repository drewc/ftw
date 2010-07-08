
(asdf:defsystem #:ftw
  :author "Drew Crampsie <drewc@tech.coop>"
  :licence "MIT"
  :components ((:module :src
		:components ((:file "packages")
			     (:file "request-context-dispatcher")
			     			     
			     (:file "parameters")
			     (:file "sessions")
			     (:file "validation"))
		
		
		:serial t)
	       (:file "forms/forms"))

  :depends-on (:ftw-dispatcher
	       :ftw-request-context
	       :ftw-presentation
	       :arnesi
	       :yaclml
	       :cl-utilities))