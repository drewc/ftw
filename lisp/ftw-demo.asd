(asdf:defsystem #:ftw-demo
  :author "Drew Crampsie <drewc@tech.coop>"
  :licence "MIT"
  :components ((:module :demo :components
			((:file "demo-package")
			 (:file "demo-rucksack")
			 (:file "demo-dispatcher")
			 (:file "demo-page-presentation")
			 (:file "front-page")
			 (:file "article")
			 (:file "comments"))))
  :serial t
  :depends-on (:contextl :ftw :ftw-yaclml :ftw-persistence))