
(asdf:defsystem #:ftw-yaclml
  :author "Drew Crampsie <drewc@tech.coop>"
  :licence "MIT"
  :components ((:module :yaclml 
                :components ((:file "ftw-yaclml"))))
  :serial t
  :depends-on (:ftw :yaclml))