;;; -*- Gerbil -*-
;; [[file:~/src/ftw/src/httpd/handler.org::*/File/%20handler.ss][/File/ handler.ss:1]]
;;; -*- Gerbil -*-
;;; (C) me at drewc.ca

;; This file is literately tangled from handler.org. 

(import :std/net/httpd/handler)
(export #t (import: :std/net/httpd/handler))

;; [[file:~/src/ftw/src/httpd/handler.org][]]
(def current-http-request (make-parameter #f)) 
(def current-http-response (make-parameter #f))
;; ends here
;; [[file:~/src/ftw/src/httpd/handler.org][]]
(defrules ftw-handler () 
  ((_ body ...)
   (lambda (req res)
     (parameterize ((current-http-request req) (current-http-response res))
       body ...))))
;; ends here
;; [[file:~/src/ftw/src/httpd/handler.org][]]
(def (http-request-method* request: (req #f) . args)
  (apply http-request-method (or req (current-http-request)) args))
(def (http-request-url* request: (req #f) . args)
  (apply http-request-url (or req (current-http-request)) args)) 
(def (http-request-path* request: (req #f) . args)
  (apply http-request-path (or req (current-http-request)) args))
(def (http-request-params* request: (req #f) . args)
  (apply http-request-params (or req (current-http-request)) args))
(def (http-request-proto* request: (req #f) . args)
  (apply http-request-proto (or req (current-http-request)) args))
(def (http-request-client* request: (req #f) . args)
  (apply http-request-client (or req (current-http-request)) args))
(def (http-request-headers* request: (req #f) . args)
  (apply http-request-headers (or req (current-http-request)) args))
(def (http-request-body* request: (req #f) . args)
  (apply http-request-body (or req (current-http-request)) args))
(def (http-request-timeout*-set! request: (req #f) . args)
  (apply http-request-timeout-set! (or req (current-http-request)) args))
(def (http-response-write* response: (res #f) . args)
  (apply http-response-write (or res (current-http-response)) args))
(def (http-response-begin* response: (res #f) . args)
  (apply http-response-begin (or res (current-http-response)) args))
(def (http-response-chunk* response: (res #f) . args)
  (apply http-response-chunk (or res (current-http-response)) args))
(def (http-response-end* response: (res #f) . args)
  (apply http-response-end (or res (current-http-response)) args))
(def (http-response-force-output* response: (res #f) . args)
  (apply http-response-force-output (or res (current-http-response)) args))
(def (http-response-timeout*-set! response: (res #f) . args)
  (apply http-response-timeout-set! (or res (current-http-response)) args))

;; ends here
;; /File/ handler.ss:1 ends here
