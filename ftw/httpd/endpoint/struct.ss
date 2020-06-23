;; [[file:~/src/ftw/src/httpd/endpoint.org::*endpoint/struct.ss][endpoint/struct.ss:1]]
(import :std/net/httpd 
        :std/pregexp :gerbil/expander
        :std/sugar :std/format :std/srfi/95 :std/iter :std/error
       :drewc/ftw/httpd/handler)
(export #t)

;; [[file:~/src/ftw/src/httpd/endpoint.org::#struct_endpoint][]]
(defstruct endpoint (name match handler) constructor: :init!)

(defmethod {:init! endpoint}
  (lambda (self name match handler: (h (endpoint-name->handler name)))
    (struct-instance-init! self name match h))
  rebind: #t)

(def (endpoint-dispatcher endpoint)
  (let ((predicate? {endpoint-match-predicate endpoint})
        (handler (endpoint-handler endpoint)))
    (lambda (req res)
      (let (args (predicate? req))
        (and args (begin0 #t (handler req res args)))))))
;; ends here
;; [[file:~/src/ftw/src/httpd/endpoint.org::#endpoint_match][]]
(def (endpoint-pregexp-match-predicate endpoint)
  (lambda (request (path (http-request-path request)))
    (let ((groups (pregexp-match (endpoint-match endpoint) path)))
      (and groups (cdr groups)))))
;; ends here
;; [[file:~/src/ftw/src/httpd/endpoint.org::#endpoint_match][]]
(defmethod {endpoint-match-predicate endpoint}
  (lambda (self (m (endpoint-match self)))
    (cond ((string? m) (endpoint-pregexp-match-predicate self))
          ((procedure? m) m)
          (#t (error "Endpoint Match is not a string or a procedure")))))
;; ends here
;; [[file:~/src/ftw/src/httpd/endpoint.org::*Handling%20a%20matched%20request][]]
(def (endpoint-name->handler name)
  (defsyntax (handler stx)
    (syntax-case stx  ()
      ((h meth)
       (let (M (syntax->datum #'meth))
         (datum->syntax #'h
           `(let (fn (or ,M
                         (parameterize ((gx#current-expander-context ctx))
                           (##eval
                            `(lambda (args)
                               (apply ,(string->symbol (format "~a/~a" name ',M)) args))
                            ctx))))
              (begin0 (fn args)
                (unless ,M (set! ,M fn)))))))
      ((h ms ...)
       (let* ((methods (syntax->datum #'(ms ...)))
              (lets (map (lambda (m) [m #f]) methods))
              (cases (map (lambda (m) [[m] `(handler ,m)]) methods)))
         (datum->syntax #'h
           `(let ,lets
              (lambda (req res args)
                (let ((method (http-request-method req)))
                  ((ftw-handler (case method ,@cases))
                   req res)))))))))
  (let (ctx (gx#current-expander-context))
      (handler GET HEAD POST PUT DELETE CONNECT OPTIONS TRACE PATCH)))

;; ends here
;; endpoint/struct.ss:1 ends here
