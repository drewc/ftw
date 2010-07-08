
(asdf:defsystem #:ftw-presentation
  :author "Drew Crampsie <drewc@tech.coop>"
  :licence "MIT"
  :components ((:module :presentation
                :components ((:file "presentation"))))
  :serial t
  :depends-on (:contextl))