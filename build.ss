#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("ftw/httpd/handler"
    "ftw/httpd/parameters"
    "ftw/httpd/endpoint/struct"
    "ftw/httpd/endpoint/queue"
    "ftw/httpd/endpoint/mux"
    "ftw/httpd/endpoint-test"
    "ftw/httpd/endpoint"
    "ftw/httpd/cookies"
    "ftw/timestamp"
    "ftw/file/mime-type"
    "ftw/file"
    "ftw/httpd"
    "ftw"
    "ftw/test/all-tests"))
