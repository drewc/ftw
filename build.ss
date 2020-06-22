#!/usr/bin/env gxi
;; -*- Gerbil -*-

(import :std/build-script)

(defbuild-script
  '("src/httpd/handler"
    "src/httpd/parameters"
    ;; Endpoints
    
    "src/httpd/endpoint/struct" "src/httpd/endpoint/queue"
    "src/httpd/endpoint/mux"
    "src/httpd/endpoint-test"
    "src/httpd/endpoint"
    "src/httpd/cookies"
    "src/httpd"
    "src/ftw"
    "test/all-tests"))
