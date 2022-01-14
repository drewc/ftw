  (import :drewc/ftw/httpd/endpoint/struct
          :drewc/ftw/httpd/endpoint/queue
          :drewc/ftw/httpd/endpoint/mux
          (for-syntax ./handler)
          (for-syntax  :drewc/ftw/httpd/endpoint/struct)
           (for-syntax :gerbil/expander) :std/format)
  (export #t
   (import: :drewc/ftw/httpd/endpoint/queue)
   (import: :drewc/ftw/httpd/endpoint/struct)
   (import: :drewc/ftw/httpd/endpoint/mux))

     (def default-endpoint-http-mux (endpoint-http-mux))
   (defsyntax (define-endpoint stx)
     (def (one-handler name ns method)
       (let (n (string->symbol
                 (string-append
                  (if (symbol? ns) (symbol->string ns) ns)
                  "#"
                  (symbol->string name) "/" (symbol->string method))))
         `(let ((handler
                 (or ,method
                     (begin
                       (let ((bound?
                              (##with-exception-catcher
                               (lambda (e) #f)
                               (lambda () (let ((t (eval ',n)))
                                       (and (procedure? t)
                                            (eval '(lambda args (apply ,n args)))))))))
                         (if bound? (set! ,method bound?) #f))
                       ,method))))
            (if handler (apply handler args) (error "No handler named ~a" ',n)))))
     (def (handler name ns)
       (let* ((methods
                    '(GET HEAD POST PUT DELETE CONNECT OPTIONS TRACE PATCH))
              (cases (map (lambda (m) `((,m) ,(one-handler name ns m))) methods)))
         `(let ,(map (cut list <> #f) methods)
            (lambda (req res args)
            (let ((method (std/net/httpd/handler#http-request-method req)))
              ((ftw-handler (case method ,@cases))
               req res))))))
     (syntax-case stx ()
       ((macro name match args ...)
        (let (ns (or (module-context-ns (current-expander-context))
                     (expander-context-id (current-expander-context))))
          (let* ((props (syntax->datum #'(args ...)))
                 (name (syntax->datum #'name))
                 (m (syntax->datum #'match))
                 (mux (or (pget mux: props)
                          'drewc/ftw/httpd/endpoint#default-endpoint-http-mux))
                 (p (pget priority: props))
                 (h (pget handler: props))
                 (endpoint
                  `(drewc/ftw/httpd/endpoint/struct#make-endpoint
                    ',name ,m
                    ,@(if h [handler: h]
                            [handler: (handler name ns)])))
                   (defname (string->symbol
                             (string-append (symbol->string name) "::endpoint")))
                   (form
                    `(begin (def ,defname ,endpoint)
                            (drewc/ftw/httpd/endpoint/mux#add-endpoint-to-mux!
                             ,defname ,mux ,@(if p [priority: p] []))
                            ,defname)))
              (datum->syntax #'macro form))))))
