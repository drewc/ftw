(in-package :ftw)

(defmacro parameter-bind (bindings &body body)
  `(let  
       ,(loop for binding in bindings collect 
	     `(,(car (ftw-rq::ensure-list binding)) 
		(apply #'read-parameter 
		       (ftw-rq:current-request-context) 
		       ',(ftw-rq::ensure-list binding))))
     ,@body))

(defun read-parameter (context name &key 
		       (type 'string)
		       (reader #'identity)
		       (validate t)
		       (method '(or :GET :POST))
		       (errorp nil))

  (let* ((parameter-value (parameter-value context name))
	 (lisp-value (multiple-value-bind (value error)
			 (ignore-errors (funcall reader parameter-value))
		       (when error 
			 (let ((condition (make-condition 'parameter-validation-type-error 
							  :expected-type type 
							  :datum value 
							  :parameter-name name 
							  :format-control "parameter ~A is not of type ~A" 
							  :format-arguments (list name type))))
			   (values (funcall (if errorp #'error #'signal) condition)
				   condition)))

		       value))
	 )
    lisp-value))