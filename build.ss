#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("ftw/file"
    "ftw/httpd/handler"
    "ftw/httpd/parameters"
    "ftw/httpd/endpoint/struct"
    "ftw/httpd/endpoint/queue"
    "ftw/httpd/endpoint/mux"
    "ftw/httpd/endpoint-test"
    "ftw/httpd/endpoint"
    "ftw/httpd/cookies"
    "ftw/httpd/json"
    "ftw/timestamp"
    "ftw/file/mime-type"
    "ftw/httpd"
    "ftw"
    "ftw/test/all-tests"))
