(defpackage :ftw-forms 
 (:use :cl :yaclml
       :contextl
       :ftw-request-context)
 (:export
  :make-input-map
  :match-input-map
  #:present-input-map
  #:input-map-valid-p
  #:find-input
  #:input-value))

(in-package :ftw-forms)

(defclass input-map ()
  ((canonicalized-inputs 
    :initarg inputs
    :reader input-map-canonicalized-inputs)
  (invalid-inputs :accessor input-map-invalid-inputs
		  :initform nil)))

(defun find-input (map name)
  (find name (input-map-canonicalized-inputs map)
	:key (lambda (input)
	       (getf input :name))))

(defun input-value (input)
  (parameter-value 
   (current-request-context)
   (getf input :input-name)))
    

(define-layered-function present-input-map (input-map &key input-wrapper 
							       )
  (:method ((input-map input-map) &key (input-wrapper 
					   (lambda (render)
					     (funcall render))))
     (dolist (input (input-map-canonicalized-inputs input-map))
       (let* ((type (getf input :type))
	      (submit (string-equal type 'submit))
	      (value (parameter-value 
		      (current-request-context)
		      (getf input :input-name)))
	      (invalid (when (not submit)
			 (assoc (getf input :input-name)
				(input-map-invalid-inputs input-map)
				:test #'equalp)))
	      (success-message (getf input :validation-success-message)))

	 (funcall 
	  input-wrapper
	  (lambda () 
	    (<:div 
	  :class "ftw-form-input"
	  (when (not (alexandria:emptyp (string (getf input :label))))
	     (<:div :class "ftw-form-input-label"
		    (<:label 
		     (<:as-html (getf input :label)))))
	  (<:div 
		 :class "ftw-form-input-value" 
		 (if (string-equal type "radio")
		     (loop for option in (getf input :options)
			:do 		 
			  (<:as-html  " "
			   (getf option :label))
			  (<:input :type "radio"
				   :name (getf input :input-name)
				   :checked (getf option :checked)
				   :value (or (getf option :value)
					      "")))
		     (progn
		       (if (getf input :id)
			   (<:input :type type
				    :id (getf input :id)
				    :name (getf input :input-name)
				    :value (and value value))
			   (<:input :type type
				    :name (getf input :input-name)
				    :value (and value value)))
		       (if invalid 
			   (<:div 
			    :class "validation-error-message error rounded"
			    (<:span 
			     (<:as-html (rest invalid))))
			   (when (and success-message
				      (not (null value)))
			     (<:div 
			      :class "validation-success-message success rounded"
			(<:span
			 (<:as-html success-message)))))))))))))))

(defun match-input-map (input-map)
  (every 
   #'identity 
   (mapcar (lambda (input) 
	     (parameter-value (current-request-context)
			      (getf input :input-name)))
	   (input-map-canonicalized-inputs input-map))))

(defun input-map-valid-p (input-map)
  (setf (input-map-invalid-inputs input-map) nil)
  (mapc (lambda (input) 
	  (let* ((name (getf input :input-name))
		 (value (parameter-value (current-request-context)
					 name))
		 (error-message (getf input :validation-error-message))
		 (validator (getf input :validate-using)))
	    (cond ((not (null validator))
		   (multiple-value-bind (result message)
		       (funcall validator value :input-map input-map)
		     (unless result
		       (push (cons name (or message error-message))
			     (input-map-invalid-inputs input-map)))))
		  ((and (not (getf input :optional)) 
			(not (listp value)) (alexandria:emptyp value))
		   (push (cons name (or error-message "this field is required"))
			 (input-map-invalid-inputs input-map))))))			    
	(input-map-canonicalized-inputs input-map))
  (not (input-map-invalid-inputs input-map)))
  
  

(defun make-input-map (name inputs)
  (let ((canonicalized-inputs 
	 (mapcar 
	  (lambda (input)
	    (let ((input (if (listp input) input (list input))))
	      (list :validate-using (getf (cdr input) :validate-using)
		    :validation-error-message (getf (cdr input) :validation-error-message)
		    :validation-success-message (getf (cdr input) :validation-success-message)
		    :options (getf (cdr input) :options)
		    :optional (getf (cdr input) :optional)
		    :name (first input)
		    :id (getf (cdr input) :id)
		    :label (or (getf (cdr input) :label) 
			       (first input))
		    :input-name (string-downcase 
				 (format nil "~A[~A]" name (first input)))
		    :type (or (getf (cdr input) :type) "text"))))
	  inputs)))
    (make-instance 'input-map 'inputs canonicalized-inputs)))

  
	    