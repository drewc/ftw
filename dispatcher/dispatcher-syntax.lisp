(defpackage #:ftw-dispatcher-syntax
  (:use :cl 
	:ftw-dispatcher-api
	:ftw-dispatcher-implementation)
  (:export #:define-dispatcher
	   #:define-dispatch-handler
	   #:expand-dispatch-handler-options
	   #:call-next-handler)
  (:documentation 
   "Defining and convenience macros "))

(in-package :ftw-dispatcher-syntax)

(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list designated by LIST."
  (if (listp list)
      list
      (list list)))


(defgeneric expand-dispatcher-option (handler name &rest values)
  (:method (handler (name (eql :add-to)) &rest values)
    (declare (ignore name))
    `(add-handler ',(first values) 
		  ,@`(:name ',handler :function 
			    ,(or (getf (cdr values) :function ) 
				`(lambda (context)
				  (make-dispatch-function (find-dispatcher ',handler)  
							  context))) 
			    ,@(rest  values)))))

(defun expand-dispatcher-options (name options)
  (let ((options (if (listp (first options)) 
		      options
		      (list options))))
    (loop for option in options collect (apply #'expand-dispatcher-option name (car option) (cdr  option)))))

(defmacro define-dispatcher (name &rest options)
  `(progn  
     (ensure-dispatcher ',name)
     ,@(expand-dispatcher-options name options)))

(defmacro dispatcher (name)
  `(find-request-dispatcher ',name))

(defgeneric expand-dispatch-handler-option (handler name &rest values)
  (:method (handler (name (eql :add-to)) &rest values)
    (declare (ignore name))
    `(add-handler ',(first values) 
		  ,@`(:name ',handler  
			    ,@(rest  values)))))

(defun expand-dispatch-handler-options (name options)
  (let ((options (if (listp (first options)) 
		      (rest options)
		      (list options))))
    (loop for option in options collect (apply #'expand-dispatch-handler-option name (car option) (cdr  option)))))

(defmacro define-dispatch-handler-function (name lambda-list &body body)
  (let ((continuation (gensym "Continuation-"))
	(value-name (gensym (format nil "~A~A" name '-handler-value)))
	(&rest (intern (princ-to-string lambda-list))))
    `(progn 
       (defun ,name (&rest ,&rest)
	 (apply (lambda ,lambda-list 
		  (flet ((,value-name
			     (,continuation)
			   (flet ((call-next-handler (&rest args)
				    (if args 
					(apply ,continuation args)
					(apply ,continuation ,&rest))))
			     ,@body)))
		    #',value-name))
		,&rest)))))



(defmacro define-dispatch-handler (name-and-options lambda-list &body body)
  (let* ((name-and-options (ensure-list name-and-options))
	 (name (car name-and-options))
	 (options (rest name-and-options)))
    (print options)
    `(progn  
       (define-dispatch-handler-function ,name ,lambda-list
	 ,@body)
       ,@(expand-dispatch-handler-options name options)
       ',name)))







