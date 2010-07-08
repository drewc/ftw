(defpackage #:ftw-dispatcher-implementation
  (:use :cl)
  (:export #:MAKE-DISPATCHER
	   #:DISPATCHER
	   #:MAKE-DISPATCH-HANDLER
	   #:DISPATCHER-HANDLERS
	   #:DISPATCH-HANDLER
	   #:DISPATCH-HANDLER-NAME
	   #:DISPATCH-HANDLER-FUNCTION
	   #:DISPATCH-HANDLER-PRIORITY
	   #:ADD-DISPATCH-HANDLER
	   #:FIND-DISPATCH-HANDLER
	   #:DELETE-DISPATCH-HANDLER
	   #:MAKE-DISPATCH-FUNCTION
	   #:MAKE-DISPATCH-HANDLER)
  (:documentation 
   "Low level implementation functions for the DISPATCHER API"))

(in-package :ftw-dispatcher-implementation)

(defstruct dispatcher 
  "Dispatchers are simply containers for handlers. See MAKE-DISPATCH-FUNCTION" 
  name handlers)

(defstruct dispatch-handler
  "Handlers have a name, a function and a priority. Th
  function must be a function that takes a single argument, the
  context, and returns a function that takes a single argument, the
  continuation. See MAKE-DISPATCH-FUNCTION."  
  name function priority)

(defun add-dispatch-handler (dispatcher handler)
  "Add a handler ``handler'' to ``dispatcher''. This will set
DISPATCHER-HANDLERS to a list including ``handler'' and the
current handlers, sorted in descending order by
HANDLER-PRIORITY. 

If a handler with the same name exists, it will be removed prior to adding ``handler''"
  (setf (dispatcher-handlers dispatcher)
	(sort (copy-list (cons handler 
			       (remove (dispatch-handler-name handler) (dispatcher-handlers dispatcher) 
				       :key #'dispatch-handler-name)))  
	      #'> :key #'dispatch-handler-priority)))

(defun find-dispatch-handler (dispatcher item &rest args)
  "Return handler named ``name'' or NIL"
  (apply #'find item (dispatcher-handlers dispatcher) (or args (list :key #'dispatch-handler-name))))


(defun delete-dispatch-handler (dispatcher handler)
  "destructively remove ``handler'' from ``dispatcher'' "
    (setf (dispatcher-handlers dispatcher)
	  (remove handler (dispatcher-handlers dispatcher) :key #'dispatch-handler-name)))

(defun make-dispatch-function (dispatcher &rest context)
  (lambda (continuation) 
    (labels ((dispatch (handlers context)
	       (funcall (apply 
			 (dispatch-handler-function  
			  (first handlers))
			 context)
		      (flet ((dispatch-next-handler (&rest next-context)
			       (let ((context (or next-context context)))
				 (if (rest handlers)
				     (dispatch (rest handlers) context)
				     (apply continuation context)))))
		#'dispatch-next-handler))))
      (dispatch (dispatcher-handlers dispatcher) context))))



























