(defpackage #:ftw-monadic-dispatcher
  (:use :cl :ips-monads)
  (:export #:ftw-dispatch 
	   #:.let* #:.return
	   #:finish
	   #:try-next-handler
	   #:context
	   #:path
	   #:path=
	   #:param
	   #:.or
	   #:.set
	   #:.get
	   #:send
	   #:maybe)
  (:documentation 
   "A monadic, side effect free, continuation using dispatcher handler"))

(in-package :ftw-monadic-dispatcher)

(defclass <dispatcher> (<cont-transformer> <zero-plus>)
  ()
  (:default-initargs :inner (make-instance '<state>)))

(defparameter <dispatcher> (make-instance '<dispatcher>))

(defmacro .let* (bindings &body body)
  `(mlet* (,(gensym) <dispatcher>)
       ,bindings ,@body))

(defun run (dispatcher &key context)
  (funcall 
   (funcall 
    (let ((m <dispatcher>))			  
      (call/cc m (lambda (finish)
		   (mlet* m 
		       ((_ (put m (list finish context))))
		     dispatcher))))
    'identity)		    
   context))

(defun finish (&optional (result t))
  (.let* ((state (fetch <dispatcher>)))
    (funcall (car state) (lambda (a) (cons result a)))))

(defmethod zero ((m <dispatcher>))
  (finish nil))
  
(defmethod plus ((m <dispatcher>) &rest mvs)
  (call/cc 
   m (lambda (win)
       (.let* ((state (fetch <dispatcher>))
	       (_ (call/cc 
		   m (lambda (fail)

		       (.let* ((_ (put m (cons fail (cdr state))))
			       (v (first mvs)))
			 (funcall win (lambda (s) 
					(list* v (car state) (cdr s))))))))
	       (_ (put m state)))

	 (if (rest mvs) 
	     (apply #'plus m (rest mvs))
	     (zero m))))))

		  
(defun ftw-dispatch (context dispatcher)
  (lambda (next-handler)
    (destructuring-bind (value . _)
	(run dispatcher :context context)
      (declare (ignore _))
      (or value	
	  (funcall next-handler context)))))

(defun send (function &rest args)
  (lambda (k) 
    (declare (ignore k))
    (lambda (s)   
      (apply function args)
      (cons (list* 'sent function args) s))))

(defun .or (&rest mvs)
  (apply #'plus <dispatcher> mvs))

(defun .return (value)
  (result <dispatcher> value))

(defun try-next-handler ()
  (ips-monads::zero <dispatcher>))

(defun context ()
  (.let* ((state (fetch <dispatcher>)))
    (.return (second state))))

(defun update-context-value-map (new-map &key (m <dispatcher>))
  (.let* ((state (fetch m)))
    (put m (list* (first state) (second state) new-map))))

(defun context-value-map (&key (m <dispatcher>))
  (.let* ((state (fetch m)))
    (.return (cddr state))))

(defun .set (name value)
  (.let* ((map (context-value-map)))
    (update-context-value-map (acons name value map))))

(defun .get (name)
  (.let* ((map (context-value-map)))
    (.return (cdr (assoc name map)))))
   	  
(defun path  ()
  (.let* ((context (context)))
    (.return (ftw-request-context:request-path context))))

(defun path= (test)
  (.let* ((path (path)))
    (if (typecase test
	  (string (equal path test))
	  (function (funcall test path)))
	(.return path)
	(try-next-handler))))

(defun param (name &key method parser)
  (.let* ((context (context)))
    (let ((parameter-value 
	   (ftw-request-context:parameter-value context name :method method)))
      (if parameter-value 
	  (.return (funcall (or parser 'identity) parameter-value))
	  (try-next-handler)))))

(defun maybe (dispatcher)
  (.or dispatcher (.return nil)))


	



			  





  




		      
	      
	    


  
  


