
(asdf:defsystem #:ftw-display
  :author "Drew Crampsie <drewc@tech.coop>"
  :licence "MIT"
  :components ((:module :display 
                :components ((:file "display"))))
  :serial t
  :depends-on (:ftw :lisp-on-lines :ftw-yaclml))