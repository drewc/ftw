(in-package :ftw-rq)

(defmacro context (name)
  `(context-symbol-value (current-request-context) ',name))

(defun (setf context) (value name)
  (setf (context-symbol-value (current-request-context) name) value))

(defun ensure-list (list)
  "If LIST is a list, it is returned. Otherwise returns the list designated by LIST."
  (if (listp list)
      list
      (list list)))

(macrolet ((def (name value &optional doc)
             (declare (ignorable doc))
             `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                ,@(when doc (list doc)))))
  (def request-context-lambda-list-keywords
      '(&path 
	&query
	&stream
	&method
	&activate
	&deactivate)
    "A list of symbols used as lambda list keyworks for request context pattern matching"))

(defun group-lambda-list-keywords (lambda-list)
  (loop with table = (make-hash-table :test #'equal) 
	with current-keyword = nil
	for (first . rest) on lambda-list 
	 :if (and (symbolp first) 
		       (eql #\& (aref (symbol-name first) 0)))
	:do (setf current-keyword (symbol-name first))
	    (setf (gethash current-keyword table) nil)
	:else :if current-keyword 
	:do (push first (gethash current-keyword table nil))
	:finally (return (loop for k being the hash-keys in table 
			       using (hash-value v) collect (cons k (nreverse v))))))

(defun get-lambda-list-keyword-value (keyword group)
  (cdr (assoc keyword group :test #'string-equal)))

(defun plist-name (symbol)
  (let* ((string (symbol-name symbol)))
    (when  (and (eql #\[ (aref string (- (length string) 2)))
				 (eql #\] (aref string (- (length string) 1))))
      (subseq string 0 (- (length string) 2)))))

(defun expand-request-context-lambda-list (lambda-list body 
					   &key (fail '(call-next-handler))
					   (context '(current-request-context)))  
  (let* ((fail-tag (gensym))
	 (win-tag (gensym))
	 (path-var (gensym))
	 (group (group-lambda-list-keywords lambda-list))
	 (&path (get-lambda-list-keyword-value '&path group))
	 (&method (get-lambda-list-keyword-value '&method group))
	 (plists nil)
	 (&query (mapcar (lambda (q &aux (qlist (ensure-list q))) 
			   (let ((name (plist-name (car qlist))))
			     (if name 
				 (let ((name (intern name))) 
				   (push name plists)
				   (if (listp q) 
				       (cons name (cdr q))
				       name
				       ))
				 q))) 
			 (get-lambda-list-keyword-value '&query group)))
	 (&activate (get-lambda-list-keyword-value '&activate group))
	 (&deactivate (get-lambda-list-keyword-value '&deactivate group))


	 (context-var (gensym))
	 (variables (cons (list context-var context)  (mapcar (lambda (arg) (first (ensure-list arg))) (append &path  &query))))
	 (block (gensym)))
    
    (destructuring-bind (&optional path test) (ensure-list (car &path)) 
      `(block ,block
	 (let ,variables
	   ,@(when variables  `((declare (ignorable ,(caar variables),@(cdr variables)))))
	   (tagbody 
	      ,@(when path 
		      `((let ((,path-var (request-path ,context))) 
			  (if ,(if test (list* (car test) path-var (cdr test)) path-var)
			      (setf ,path ,path-var)
			      (go ,fail-tag)))))
	      ,@ (destructuring-bind (&optional method test) (ensure-list (car &method))
		   (when method 
		      `((let ((,path-var (http-method ,context))) 
			  (if ,(if test (list* (car test) path-var (cdr test)) path-var)
			      (setf ,method ,path-var)
			      (go ,fail-tag)))))
		   )
	      ,@ (loop :for param :in &query with var = (gensym) for i upfrom 1
		    :collect     (destructuring-bind (&optional name test) (ensure-list param) 
				   `(let (,`(,var ,(if (member name plists) 
						       `(parameters-as-plist ,context ',name :package ,*package*)
						       `(parameter-value ,context ',name))))				      
				      (if ,(if test (list* (car test) var (cdr test)) var)
					  (setf ,name ,var)
					  (go ,fail-tag)))))
	  
	      (go ,win-tag)
	      ,fail-tag (return-from ,block ,fail)
	      ,win-tag (return-from ,block  (with-active-layers ,&activate
					      (with-inactive-layers ,&deactivate,@body)))))))))


