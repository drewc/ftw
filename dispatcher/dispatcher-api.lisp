(defpackage #:ftw-dispatcher-api
  (:use :cl :ftw-dispatcher-implementation)
  (:export #:ENSURE-DISPATCHER
	   #:FIND-DISPATCHER
	   #:DISPATCHER
	   #:ADD-HANDLER
	   #:DELETE-HANDLER)
  (:documentation 
   "Function based API for DISPATCHER"))

(in-package :ftw-dispatcher-api)

(defpackage #:%ftw-dispatcher-accessors 
  (:use )
  (:documentation "A namespace for dispatchers"))

(defun dispatcher-symbol-name
 (name)
  (intern (format nil "~A::~A" (package-name (symbol-package name)) 
		  (symbol-name name)) 
	  '#:%ftw-dispatcher-accessors))

(defun ensure-dispatcher (name &rest handlers)
  (let ((dispatcher (make-dispatcher :name name)))
    (set (dispatcher-symbol-name name)
	 dispatcher)
    (mapc (lambda (handler) (add-dispatch-handler dispatcher handler)) 
	  handlers) dispatcher))

(defun find-dispatcher (name)
  (symbol-value (dispatcher-symbol-name name)))

(defun %add-handler (dispatcher 
		     &key name (function name) priority 
		     before after &allow-other-keys)
  (delete-handler dispatcher name)
  (if (or before after)
      (let ((existing-handler 
	     (find-dispatch-handler dispatcher (or before after))))
	(assert existing-handler (dispatcher) 
		"No handler ~A in ~A" (or before after)  dispatcher)
	(let* ((before/after-handler 
		(find-dispatch-handler 
		 dispatcher 
		 (dispatch-handler-priority existing-handler)
		 :key #'dispatch-handler-priority
		 :test (if after #'> #'<)
		 :from-end before))
	       (new-priority 
		(funcall '- (dispatch-handler-priority existing-handler) 
			 (if before/after-handler
			     (let ((num (/ (- (dispatch-handler-priority existing-handler)
					      (dispatch-handler-priority before/after-handler)) 
					   2))) 
			       (if (>= (or num 1) 1) 
				   1 
				   num))
			     1))))

	  (when before/after-handler 
	    (assert (funcall (if before '< '>)
			   (dispatch-handler-priority existing-handler)
			   new-priority
			   (dispatch-handler-priority before/after-handler))
		  ()
		  "Priority gone mad! ~A should come between ~A and ~A"
		  new-priority
		  (dispatch-handler-priority existing-handler)
		  (dispatch-handler-priority before/after-handler)))
			   
			      
	  (add-dispatch-handler 
	   dispatcher 
	   (make-dispatch-handler 
	    :name name 
	    :function function 
	    :priority new-priority))))
	 (add-dispatch-handler 
	  dispatcher 
	  (make-dispatch-handler 
	   :name name 
	   :function function 
	   :priority (or priority 0)))))

(defgeneric add-handler (dispatcher &rest args &key name function priority before after &allow-other-keys)
  (:method ((dispatcher dispatcher) &rest args 
	    &key (name (error "must provide name")) 
	    priority before after 
	    &allow-other-keys)    
    (assert (not (or (and (or before after) priority) (and before after))) 
	    () "Must provide only one of :before :after or :priority")
    (apply #'%add-handler dispatcher args))

  (:method ((dispatcher symbol) &rest args)
    (apply #'add-handler (find-dispatcher dispatcher) args)))

(defgeneric find-handler (dispatcher name &rest args)
  (:method ((dispatcher dispatcher) name &rest args)
    (apply #'find-dispatch-handler dispatcher name args))
  (:method ((dispatcher symbol) name &rest args)
    (apply #'find-handler (find-dispatcher dispatcher) name args)))

(defgeneric delete-handler (dispatcher name)
  (:method ((dispatcher dispatcher) name)
    (delete-dispatch-handler dispatcher name))
  (:method ((dispatcher symbol) name)
    (delete-handler (find-dispatcher dispatcher) name)))

























