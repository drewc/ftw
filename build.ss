#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/make
        (only-in :gerbil/tools/gxtags make-tags))

(def srcdir
  (path-normalize (path-directory (this-source-file))))

(def build-spec
  (append
   (filter-map
    (lambda (filename)
      (and (equal? (path-extension filename) ".ss")
	   (not (equal? "build.ss" (path-strip-directory filename)))
	   (path-strip-extension filename)))
    (read-all (open-directory)))
   (filter-map
    (lambda (filename)
      (and (equal? (path-extension filename) ".ss")
	   (path-expand (path-strip-extension filename) "server")))
    (read-all (open-directory "server")))))

(def (main . args)
  (match args
    (["meta"]
     (write '("spec" "deps" "compile" "tags"))
     (newline))
    (["spec"]
     (pretty-print build-spec))
    (["deps"]
     (cons-load-path srcdir)
     (let (build-deps (make-depgraph/spec build-spec))
       (call-with-output-file "build-deps" (cut write build-deps <>))))
    (["tags"]
     (make-tags ["" "server"] "TAGS"))
    (["compile"]
     (let (depgraph (call-with-input-file "build-deps" read))
       (make srcdir: srcdir
             optimize: #t
             debug: 'src
             static: #t
             depgraph: depgraph
             prefix: "ftw"
             build-spec)))
    ([]
     (displayln "... make deps")
     (main "deps")
     (displayln "... compile")
     (main "compile")
     (displayln "... make tags")
     (main "tags"))))
