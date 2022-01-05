  (import :std/test  :std/sugar  :std/net/request :std/net/httpd 
              :drewc/ftw/httpd/handler :std/format :std/iter :std/srfi/95
              :drewc/ftw/httpd/endpoint/struct
              :drewc/ftw/httpd/endpoint/queue
              :drewc/ftw/httpd/endpoint/mux
              :gerbil/gambit/exceptions
              :gerbil/gambit/ports
              (only-in :gerbil/gambit/random random-integer))

     (import :std/test :std/net/httpd :std/net/request)
  
     (def server-address "127.0.0.1:9942")
  
     (def server-url (string-append "http://" server-address)) 
  
     (def (test-http-get url) (http-get (string-append server-url url))) 
  
     (def httpd #f) 
  
     (defrules with-httpd []
       ((_ mux body ...)
        (with-unwind-protect
         (lambda ()
           (when httpd 
             (stop-http-server! httpd))
           (set! httpd (start-http-server! server-address mux: mux))
           body ...)
         (lambda () (stop-http-server! httpd)))))

  (export endpoint-test)

  (def greeting-not-found "these aren't the droids you are looking for")

  (def (default-handler req res)
    (http-response-write
     res 404 '(("Content-Type" . "text/plain")) greeting-not-found))

    (def test::endpoint (endpoint 'test "^/([^/]*)/?test/?(.*)"))
    (def (test/GET . args)
      (http-response-write* 200 '(("Content-Type" . "text/plain"))
                            (format "~a" args)))
    (def (match-function? req)
      (and (string=? (http-request-path req) "/function")
           [(http-request-url req)]))
  
    (def match::endpoint (endpoint 'match match-function?))
  
    (def match/GET test/GET)
    (def (test-endpoint-handler req res)
      (try 
       (or ((endpoint-dispatcher test::endpoint) req res)
           ((endpoint-dispatcher match::endpoint) req res)
           (default-handler req res))
       (catch (e) (http-response-write
                   res 500 '(("Content-Type" . "text/plain"))
                   (with-output-to-string (cut display-exception e))))))
    (def eq (endpoint-queue))
  
    (def (test-endpoint-queue-handler req res)
      (try 
       (or ((endpoint-queue-dispatch-function eq) req res)
           (default-handler req res))
       (catch (e) (http-response-write
                   res 500 '(("Content-Type" . "text/plain"))
                   (with-output-to-string (cut display-exception e))))))
  
    (def (testq/GET num arg)
      (http-response-write* 200 '(("Content-Type" . "text/plain"))
                            (format "num:~a ~a" num arg)))
  
    (def ep1 (endpoint '1 "/ep(1)(.*)"))
    (def 1/GET testq/GET)
    (def ep2 (endpoint '2 "/ep(2)(.*)"))
    (def 2/GET testq/GET)
    (def ep3 (endpoint '3 "/ep(3)(.*)"))
    (def 3/GET testq/GET)
    (def ep4 (endpoint '4 "/ep(4)(.*)"))
    (def 4/GET testq/GET)
    (def ep4-aussi (endpoint '4-aussi "/ep(4)(.*)"))
    (def 4-aussi/GET testq/GET)
  
    (def tctx (gx#current-expander-context))
    (def (teval form)
      (parameterize ((gx#current-expander-context tctx))
        (##eval form tctx)))
     (def epmux (endpoint '42 "/ep1(.*)"))
     (def (42/GET text)
       (http-response-write*
        400 [] (format "EP1!!:~a" text)))
  
  (def endpoint-test
    (test-suite
     "test :drewc/ftw/httpd/endpoint"


     (test-case
      "Endpoint Struct: basic dispatch"
      (with-httpd (make-recursive-http-mux)
          (def end-dis::endpoint
            (make-endpoint 'end-dis (? (and list? identity))
                           handler: 
                           (lambda (req res args)
                             (displayln "Dis!"))))
        
          (let (strp (open-string ""))
            (check (parameterize ((current-output-port strp))
                      ((endpoint-dispatcher end-dis::endpoint)
                     
                       ["request-as-list-lol"] 'response-as-sym))
                   => #t)
            (check (get-output-string strp) => "Dis!\n")
            (check (parameterize ((current-output-port strp))
                     ((endpoint-dispatcher end-dis::endpoint)
                     
                      "request-as-string" 'response-as-sym))
                   => #f)
            (check (get-output-string strp) => ""))
          ;; (def httpd
          ;;   (start-http-server! server-address mux: (make-recursive-http-mux)))
        
          (http-register-handler httpd "" test-endpoint-handler)
        
          (let (req (test-http-get "/this-is-a/test"))
            (check (request-status req) => 200)
            (check (request-text req) => "(this-is-a )"))
        
        
            (let (req (test-http-get "/this-is-atest"))
              (check (request-status req) => 200)
              (check (request-text req) => "(this-is-a )"))
        
            (let (req (test-http-get "/this-is-a/test/request"))
              (check (request-status req) => 200)
              (check (request-text req) => "(this-is-a request)"))
        
        
            (let (req (test-http-get "/this-is-atestrequest"))
              (check (request-status req) => 200)
              (check (request-text req) => "(this-is-a request)"))
        
            (let (req (test-http-get "/function?bar=baz"))
              (check (request-status req) => 200)
              (check (request-text req) => "(/function?bar=baz)"))
        
          ;; (stop-http-server! httpd)
        
        ))

     (test-case "Endpoint Queue"
      (with-httpd (make-recursive-http-mux)
                    (with-httpd
                     (make-recursive-http-mux)
                     (http-register-handler httpd "" test-endpoint-queue-handler)
                  
                  
                     ;; Make sure the hit counter works
                  
                     (let ((hits (+ 10 (random-integer 100))))
                       (map (cut add-endpoint-to-queue! <> eq) [ep1 ep2 ep3 ep4])
                       (add-endpoint-to-queue! ep4-aussi eq priority: 1)
                       (check (endpoint-queue-endpoint-hits eq ep1) => 0)
                       (for (x (in-range hits))
                         (let (r (test-http-get "/ep1"))
                           (check (request-text r) => "num:1 ")))
                  
                       (check (endpoint-queue-endpoint-hits eq ep1) => hits)
                       (reset-endpoint-queue-hit-counter! eq))
                     ;; Now, 4-aussi comes first, and it should be the only hit for /ep4
                  
                     (let ((hits (+ 10 (random-integer 100))))
                       (check (endpoint-queue-endpoint-index eq ep4-aussi) => 0)
                       (check (endpoint-queue-endpoint-hits eq ep4) => 0)
                       (check (endpoint-queue-endpoint-hits eq ep4-aussi) => 0)
                       (for (x (in-range hits))
                         (test-http-get "/ep4"))
                       (check (endpoint-queue-endpoint-hits eq ep4) => 0)
                       (check (endpoint-queue-endpoint-hits eq ep4-aussi) => hits)
                       (reset-endpoint-queue-hit-counter! eq))
                  
                    ;; ;; Finally, the actual sorting.
                  
                      (let* ((hits (+ 10 (random-integer 1000)))
                             (count [[1 . 0] [2 . 0] [3 . 0] [4 . 0]])
                             (ran (cut 1+ (random-integer 4)))
                             (hit (lambda ((r (ran))) (test-http-get (format "/ep~a" r ))
                                     (set!  (cdr (assoc r count))
                                      (1+ (cdr (assoc r count)))))))
                    ;;    ;; remove-aussi
                  
                       (remove-endpoint-from-queue! ep4-aussi eq)
                       (for (x (in-range hits))
                          (hit))
                    ;;   ;;; the hitcount matches?
                       (check (endpoint-queue-endpoint-hits eq ep4) => (cdr (assoc 4 count)))
                    ;;   ;; Now sort
                       (sort-endpoint-queue! eq)
                       (sort! count > cdr)
                  
                    ;;   ;; ;; And check
                       (let ((first-ep (teval (string->symbol (format "ep~a" (caar count)))))
                             (last-ep (teval (string->symbol (format "ep~a" (car (last count)))))))
                         (check (endpoint-queue-endpoint-index eq first-ep) => 0)
                         (check (endpoint-queue-endpoint-index eq last-ep) => 3))
                  
                       (reset-endpoint-queue-hit-counter! eq)))))

      (test-case "Endpoint Mux"
                    (def emux (make-endpoint-http-mux))
                 
                    (add-endpoint-to-mux! epmux emux priority: 42)
                    (def default-ep
                      (endpoint
                       'default "(.*)"
                       handler: (lambda (req res args)
                                  (http-response-write
                                   res 404 '(("Content-Type" . "text/plain"))
                                   (format "Default Not Found: ~a" (car args))))))
                 
                 
                 
                    (add-endpoint-to-mux! default-ep emux priority: +inf.0)
                 
                    (with-httpd emux
                      (let ((r1 (test-http-get "/ep1"))
                            (r2 (test-http-get "/ep1foo"))
                            (r3 (test-http-get "/nope")))
                        (check (request-text r1) => "EP1!!:")
                        (check (request-text r2) => "EP1!!:foo")
                        (check (request-status r3) => 404)))
                 )))
