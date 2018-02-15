;;; -*- Gerbil -*-
(export shtml5-page->string shtml5->string)
(import :std/xml
  (only-in :gerbil/gambit/ports write-string))

(def html5-doctype "<!DOCTYPE html>")

(def (shtml5-page->string shtml5 doctype: (doctype html5-doctype))
  (call-with-output-string
    (or doctype "")
    (lambda (p)
      (when doctype (write-string "\n" p))
      (print-sxml->html-fast
       (list '*TOP* shtml5) p))))

(def (shtml5->string shtml5)
  (shtml5-page->string shtml5 doctype: #f))
