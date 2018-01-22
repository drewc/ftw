(in-package :ftw-request-context)

(define-layered-class ucw-request-context (request-context)
 ((ucw-context :accessor ucw-context :initarg ucw-context)))

(define-layered-method html-stream ((context ucw-request-context))
 (ucw-core:html-stream (ucw-core:context.response (ucw-context context))))

(define-layered-method map-parameters (function (context ucw-request-context) &key method)
 (declare (ignore method))		       
 (ucw-core:map-parameters (ucw-core:context.request (ucw-context context)) function))

(define-layered-method parameter-value ((context ucw-request-context) name &key method)
 (declare (ignore method))
 (ucw-core:get-parameter (ucw-core:context.request (ucw-context context)) 
			 (etypecase name
			     (symbol (string-downcase (symbol-name name)))
			     (string name))))

(define-layered-method send-redirect ((context ucw-request-context) target message)
 (ucw-core::send-redirect target (ucw-core:context.response (ucw-context context))))

(define-layered-method request-path ((context ucw-request-context))
 (ucw-core:query-path (ucw-core:context.request (ucw-context context))))

(define-layered-method request-uri ((context ucw-request-context))
 (ucw-core::raw-uri (ucw-core:context.request (ucw-context context))))

(define-layered-method http-method ((context ucw-request-context))
 (ucw-core::http-method (ucw-core:context.request (ucw-context context))))

(define-layered-method add-cookie (context name value &key comment domain max-age (path nil path-p) secure)
 (ucw-core::add-cookie (apply #'ucw-core::make-cookie name value :comment comment :domain domain :max-age max-age  :secure secure (when path-p (list :path path)))))

(define-layered-method cookie-value (context name &optional default)
 (ucw-core::cookie-value-using-request 
  (ucw-core:context.request (ucw-context context))
  name default))
				       
		       
(defclass ftw-ucw-application (ucw-core:basic-application)
  ())

(defparameter *ftw-ucw-application* (make-instance 'ftw-ucw-application))

(defclass ftw-ucw-server (ucw-core:standard-server)
  ())

(defun make-ftw-ucw-server (&rest args)
  (make-instance
   'ftw-ucw-server
   :backend (apply #'ucw-core:make-backend 
		   (append args '(:httpd
				  :port 9090
				  :host "0.0.0.0")))))

(defvar *ftw-ucw-server* (make-ftw-ucw-server))

(defun startup-ftw-ucw ()
  (ucw-core:startup-server *ftw-ucw-server*))

(defun shutdown-ftw-ucw ()
 (ucw-core:shutdown-server *ftw-ucw-server*))

(defmethod ucw-core::find-associated-application ((server ftw-ucw-server) request)
  *ftw-ucw-application*)

(defmethod ucw-core:service ((application ftw-ucw-application) context)
  (let ((response (ucw-core:context.response context)))
    (ucw-core:call-as-response-handler
     response
     (lambda ()
       (handle-request (make-instance 'ucw-request-context 'ucw-context context))
       (ucw-core:send-response response)
       ))))


#+nil	 (flet ((service () 	       
		  (contextl:dlet ((calling-continuation (contextl:dynamic calling-continuation)))
		    (cm-call 
		     (cm-do (cm-return callbacks)
			    (cm-fn (v k)
			      (funcall k (cons v k)))
			    (cm-fn (list.k k)
			      (destructuring-bind (callbacks . loop) list.k
				(cm-call (if callbacks 
					     (cm-do (cm-return (car callbacks))
						    (cm-fn (callback k)
						      (cm-call 
						       (cm-bind (cm-return (get-parameter request (ucw-core::callback-id callback)))
								(callback-lambda callback))
						       k)))
					     (cm-return nil))
					 (lambda (result)
					   (declare (ignore result))
					   (funcall k (cons (rest callbacks) loop))))))
			    (cm-fn (list.value k)
			      (destructuring-bind (callbacks . loop) list.value
				(if callbacks                                 
				    (funcall loop callbacks)
				    (funcall k t))
				))
			    (if action  
				action
				(cm-fn (v) v))
			    (find-default-render-function))  
		     (lambda (result)
			       (finish-request application result))))))
		      
 
	   (if old-frame
	       (contextl:with-dynamic-environment ((frame.environment old-frame))
		 (service))
	       (service)))



