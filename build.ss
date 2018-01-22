#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
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
