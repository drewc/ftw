

(asdf:defsystem #:ftw-ucw
  :author "Drew Crampsie <drewc@tech.coop>"
  :licence "MIT"
  :components ((:module :ucw
		:components ((:file "ucw-request-context"))))

  :depends-on (#:ftw-request-context :ucw))