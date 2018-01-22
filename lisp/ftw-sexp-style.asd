
(asdf:defsystem #:ftw-sexp-style
  :author "Drew Crampsie <drewc@tech.coop>"
  :licence "MIT"
  :components ((:module :sexp-style
                :components ((:file "ftw-sexp-style"))))
  :serial t
  :depends-on (:ftw :sexp-style))