(import :drewc/ftw/httpd/endpoint/struct
        :drewc/ftw/httpd/endpoint/queue
        :drewc/ftw/httpd/endpoint/mux
        (for-syntax  :drewc/ftw/httpd/endpoint/struct)
         :gerbil/expander)
(export #t
 (import: :drewc/ftw/httpd/endpoint/queue)
 (import: :drewc/ftw/httpd/endpoint/struct)
 (import: :drewc/ftw/httpd/endpoint/mux))

 (def default-endpoint-http-mux (endpoint-http-mux))
 (defsyntax (define-endpoint stx)
   (syntax-case stx ()
     ((macro name match args ...)
      (let* ((props (syntax->datum #'(args ...)))
             (name (syntax->datum #'name))
             (m (syntax->datum #'match))
             (mux (or (pget mux: props)
                      'drewc/ftw/httpd/endpoint#default-endpoint-http-mux))
             (p (pget priority: props))
             (h (pget handler: props))
             (endpoint `(drewc/ftw/httpd/endpoint/struct#make-endpoint
                         ',name ,m ,@(if h [handler: h] [])))
             (defname (string->symbol
                       (string-append (symbol->string name) "::endpoint")))
             (form
              `(begin (def ,defname ,endpoint)
                      (drewc/ftw/httpd/endpoint/mux#add-endpoint-to-mux!
                       ,defname ,mux ,@(if p [priority: p] []))
                      ,defname)))
        (datum->syntax #'macro form)))))
