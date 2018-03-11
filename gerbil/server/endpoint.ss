;; -*- Gerbil -*-
;;(export define-endpoint delete-endpoint *endpoints*)
(export #t)
(import :ftw/server/request-context :ftw/server
	:ftw/http-status-code
	:std/net/httpd
	:std/pregexp
	(phi: +1
	      :std/srfi/13
	      :std/format)
	:std/sugar
	:std/generic
	:std/format
	:std/srfi/1
        (only-in :gerbil/gambit/exceptions display-exception))

(defclass endpoint
  (name match function))

(def (endpoint-scan endpoint string)
  "=> list-of-submatches or #f

Returns a list of submatches if the scanner matches the string. The
list could be '().

Returns #f if there is no match"
  (let (match (endpoint-match endpoint))
    (if (or (string? match)
	    (pair? match))
      (let (r (pregexp-match match string))
	(if r (cdr r) r))
      (match string))))

(defsyntax (construct-endpoint-function stx)

  (def http-request-methods
      '(GET POST PUT DELETE CONNECT OPTIONS TRACE PATCH))

  (def (method-case-form name)
  
    (def (name->fn-name method)
          (string->symbol
           (format
    	"~A/~A" name
    	(string-map char-downcase (symbol->string method)))))
  
    (def (clause method)
      `((,method) (apply ,(name->fn-name method) args)))
  
    `(case method
    ,@(append
       (map clause http-request-methods)
       '((else (error "this:" method " is not an http request method"))))))

  (syntax-case stx ()
      ((macro name)
       (with-syntax ((body (method-case-form (syntax-e #'name))))
         #'(lambda (method args) body)))))

(defrules construct-endpoint ()
  ((_ name match)
   (make-endpoint 
    name: (quote name)
    match: match
    function: (construct-endpoint-function name))))

(def *endpoints* (list 'endpoints))

(defsyntax (define-endpoint stx)

  (def (%defvar name match in)
    (let ((e (gensym))
	  (i (gensym))
	  (c (gensym))
	  (varname (string->symbol (format "~A::endpoint"
					   name))))
      `(define ,varname
	 (let ((,e
		(with-catch
		 (lambda (_)
		   (construct-endpoint ,name ,match))
		 (lambda ()
		   (let ((,e ,varname))
		     (set! (endpoint-match ,e) ,match)
		     ,e))))
	       (,i ,in))
	   (unless (memq ,e ,i)
	     (set! (cdr ,i)
	       (cons ,e (cdr ,i))))
	   ,e))))


  (syntax-case stx ()
    ((macro name match in: endpoints)
     (with-syntax ((var (datum->syntax #'macro
			  (%defvar (syntax-e #'name)
				   (syntax-e #'match)
				   (syntax-e #'endpoints)))))
       #'var))
    ((macro name match)
     (with-syntax ((var (datum->syntax #'macro
			  (%defvar (syntax-e #'name)
				   (syntax-e #'match)
				   '*endpoints*))))
       #'var))))



(def (endpoint-handler request-context using: (endpoints *endpoints*))
    (try
     (let loop ((eps (cdr endpoints)))
       (if (null? eps)
	 (return-status-code +http-not-found+
			     (format "Not found: ~A~%~A"
				     (request-context-path request-context)
				     (request-context-parameters request-context)))
	 (let* ((ep (car eps))
		(args (endpoint-scan ep (request-context-path request-context))))
	   (if args
	     ((endpoint-function ep)
	      (request-context-method request-context)
	      args)
	     (loop (cdr eps))))))
     (catch (e)
       (respond* (with-output-to-string
		       "Error: " (cut display-exception e))
		     content-type: "text/plain; charset=us-ascii"
		     status-code: +http-internal-server-error+))))
 

(def (return-status-code (code +http-ok+) (message #f))
   (respond*
    (or message (http-status-code-message code))
    status-code: code
    content-type: "text/plain; charset=us-ascii"))
