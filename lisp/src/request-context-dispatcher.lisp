(in-package :ftw)

(defun starts-with (string starts-with &key (test #'string=))
  (and (>= (length string)
	   (length starts-with)) 
       (funcall test starts-with (subseq string 0 (length starts-with)))))

(defun optional (value)
  (declare (ignore value))
  t)

(defun call-handler (name &key (context (current-request-context))
		               (continuation (lambda ()
					       (call-next-handler))))
  (funcall (funcall name context) continuation))

(deflayer ftw)
(ensure-active-layer 'ftw)

(define-dispatcher toplevel)

(define-dispatch-handler (404-error :add-to toplevel :priority -1000000) (context)
  (format (html-stream context) "404 (FIXME: not a 404) : ~A not found." (request-path context)))

(define-dispatch-handler (dynamic-context :add-to toplevel :priority 1000000) (context)
  (ftw-request-context::funcall-with-dynamic-request-context 
   context 
   (lambda () (call-next-handler))))

(define-layered-method ftw-request-context:handle-request :in-layer ftw (context)
  (funcall  (ftw-dispatcher:make-dispatch-function (find-dispatcher 'toplevel) context) 'identity))

(defun preprocess-lambda-list (name lambda-list path)
  (let* ((symbol-name (symbol-name name))
	 (name-path-name 
	  (when (eql #\/ (aref  symbol-name 0))
	    (subseq symbol-name 0 (position #\? symbol-name))))
	 (name-query (subseq symbol-name (or (position #\? symbol-name) (length symbol-name))))
	 (lambda-list (if (or path (not name-path-name))
			  lambda-list
			  (list*  '&path 
				  (list (gensym) 
					`(string-equal 
					  ',name-path-name)) lambda-list))))

    (if (not (zerop (length name-query)))
	(let ((bindings 
	       (mapcar (lambda (x)
			 (let ((b (cl-utilities:split-sequence #\= x)))
			   (if (cadr b)
			       `(,(intern (car b)) (string-equal ,(cadr b)))
			       (intern (car b)))))
			 
		       (cl-utilities:split-sequence #\& (subseq name-query 1)))))
	  (loop 
	     :for (item . rest) :on lambda-list 
	     :collect item :into before 
	     :when (and (symbolp item) 
			(string-equal item '&query))
	     :do (return (append before bindings rest))
	     :finally (return `(&query ,@bindings ,@lambda-list))))
	lambda-list)))

(defmacro defhandler (name-and-options request-handler-lambda-list &body body)
  (setf name-and-options (ftw-dispatcher-syntax::ensure-list name-and-options))
  (multiple-value-bind (context  request-context-lambda-list)
      (loop 
	 :with &context = nil
	 :with &path = nil
	 :for (thing . rest) 
	 :on request-handler-lambda-list
	 :if (and (symbolp thing)  
		  (string-equal thing '&context))
	 :do (setf &context t)

	 :else :if (and (symbolp thing)
			(eql #\& (aref (symbol-name thing) 0)))
	 :collect (prog1 thing 
		    (setf &context nil)
		    (when (string-equal '&path thing)
		      (setf &path t))) 
	 :into lambda-list
	 :else :if &context
	 :collect thing into context 	 
	 :else
	 :collect thing into lambda-list
	 :finally (return (values (or (car context) (gensym)) 
				  (preprocess-lambda-list (car name-and-options) lambda-list &path))))
    `(define-dispatch-handler ,name-and-options  (,context)
       ,(ftw-rq::expand-request-context-lambda-list request-context-lambda-list 
         body :context context :fail '(call-next-handler)))))

(defgeneric expand-dispatcher-option (handler lambda-list name &rest values)
  (:method (handler lambda-list (name (eql :add-to)) &rest values)
    (declare (ignore name))
    `(:add-to ,@values :function (lambda (context)
				   (lambda (next)
				     ,(ftw-rq::expand-request-context-lambda-list 
				       lambda-list
				       `((funcall (ftw-dispatcher:make-dispatch-function (find-dispatcher ',handler) context) next))
				       :fail '(funcall next)
				       :context 'context)))))
  (:method (handler lambda-list (name (eql :handler-macro)) &rest values)
    (declare (ignore name))
    `(:handler-macro . ,(first values))))

(defun expand-dispatcher-options (name lambda-list options)
  (let ((options (if (listp (first options)) 
		      (rest options)
		      (list options))))
    (loop for option in options collect (apply #'expand-dispatcher-option name lambda-list (car option) (cdr  option)))))

(defmacro define-handler-macro (name dispatcher)
  `(defmacro ,name (handler handler-lambda-list &body body)
     (destructuring-bind (handler-name &rest handler-args) 
	 (ftw-dispatcher-syntax::ensure-list handler)
       `(progn  
	  (defhandler ,handler-name ,handler-lambda-list ,@body)
	  (add-handler ',',dispatcher :name ',handler-name ,@handler-args)))))

(defmacro defdispatcher (name request-context-lambda-list &rest options)
  "Arguments and Values:

name--a symbol
request-context-lamba-list--a REQUEST-CONTEXT-LAMBDA-LIST
options--an alist of (keyword . value)

Description:

" 
  (let* ((options (expand-dispatcher-options name request-context-lambda-list options))
	 (handler-macro-name (or (cdr (assoc :handler-macro options)) (intern (format nil "~A~A~A" 'define- name '-handler))))
	 (clean-options (loop for (name . rest) in options unless (eql name :handler-name) :collect (cons name rest))))

    `(progn 
       (define-dispatcher ,name ,@clean-options)
       ,(unless (and (assoc :handler-macro options)
		     (not (cdr (assoc :handler-macro options))))
		`(define-handler-macro ,handler-macro-name ,name))
    )))


