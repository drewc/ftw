;; -*- Gerbil -*-
(export #t)
(import :ftw/server/reply :std/generic :std/net/httpd)

(defclass request-context
  (request reply return response))

(def (request-context-respond request-context value
			      status-code: (sc #f)
			      content-type: (ct #f)
			      headers: (h #f))
  (let (reply (request-context-reply request-context))
    (when ct
      (reply-content-type-set! reply ct))
    (http-response-write
     (request-context-response request-context)
     (or sc (reply-status-code reply))
     (or h (reply-headers reply))
     value)))

(def current-request-context
  (make-parameter #f))

(def (call-with-request-context function
				request: (request (void))
				reply: (reply (create-reply))
				response: (response (void)))
  (let (request-context
	(make-request-context
	 request: request
	 reply: reply
	 response: response))
    (parameterize ((current-request-context request-context))
      (let/cc k
	(set! (request-context-return request-context) k)
	(let (value (function request-context))
	  (request-context-respond request-context value))))))

(def (respond* value
	       content-type: (ct #f)
	       status-code: (c #f)
	       headers: (h #f))
  ((request-context-return (current-request-context))
   (request-context-respond
    (current-request-context) 
    value status-code: c headers: h content-type: ct)))

(def (abort*)
  ((request-context-return (current-request-context))
   (void)))  

(def (content-type*)
  (reply-content-type (request-context-reply (current-request-context))))

(def (content-type*-set! content-type)
  (reply-content-type-set!
   (request-context-reply (current-request-context))
   content-type))
