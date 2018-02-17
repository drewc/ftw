;; -*- Gerbil -*-
(export test-request-context)
(import :ftw/server/request-context
	:ftw/http-status-code
	:ftw/server
	:ftw/server/reply
	(only-in :gerbil/gambit/exceptions display-exception)
	:std/net/httpd :std/sugar :std/generic :std/net/request)


(def (return-status-code (code +http-ok+))
  (respond*
   (http-status-code-message code)
   status-code: code
   content-type: "text/plain; charset=us-ascii"))

(def request-context-handlers (make-hash-table test: equal?))

(def (request-context-handler request-context)
  (let ((response
	 (hash-get
	  request-context-handlers
	  (http-request-url (request-context-request
			     request-context)))))
    (try
     (if response
     (response)
     (return-status-code +http-not-found+))
     (catch (e)
       (respond* (with-output-to-string
		   "Error: " (cut display-exception e))
		 content-type: "text/plain; charset=us-ascii"
		 status-code: +http-internal-server-error+)))))

(defclass (test-request-context-server ftw-server)())

(defmethod (ftw-server-handler (server test-request-context-server))
  (lambda (req res)
    (call-with-request-context
     request-context-handler
     request: req response: res)))

(def server (make-test-request-context-server address: "localhost:8443"))

;; (start-ftw-server! server)

;;  (stop-ftw-server! server)

(defrules define-response ()
  ((_ url body ...)
   (hash-put! request-context-handlers url
	      (lambda () body ...))))

(defrules test> (=>)
  ((_ test => predicate result)
   (let ((actual test))
     (if (predicate actual result)
       #t
       (error "Test: " test "Failed. Wanted: "
	      result " Got: " actual))))
  ((_ test => result)
   (test> test => equal? result))) 

(def (test-request-context)
  (dynamic-wind
    (cut start-ftw-server! server)
    (lambda ()
      (let* ((request (http-get "http://localhost:8443/not-found"))
             (text (request-text request))
             (status (request-status request)))
      
        (test> text
      	 => "Not Found")
      
        (test> status
      	 => 404))

      (def test-html  "<strong>Test!</strong>")
      
      (define-response "/test" test-html)
      
      (let* ((request (http-get "http://localhost:8443/test"))
             (text (request-text request))
             (status (request-status request))
             (content-type (cdr (assoc "Content-Type" (request-headers request)))))
      
          (test> text => test-html)
      
          (test> status => 200)
      
          (test> content-type =>  "text/html"))

      (define-response "/error"
        (error "Testing Errors"))
      
      (let* ((request (http-get "http://localhost:8443/error"))
      	 (text (request-text request))
      	 (status (request-status request))
      	 (content-type (cdr (assoc "Content-Type" (request-headers request)))))
      
            (test> text => "Error: Testing Errors\n")
      
            (test> status => 500)
      
            (test> content-type =>  "text/plain; charset=us-ascii"))
      
     )

    (cut stop-ftw-server! server)))
