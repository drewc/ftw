(defpackage :ips-monads 
  (:use :cl)
  (:export 
   #:<monad>
   #:result
   #:bind
   #:mlet*

   #:<zero-plus>
   #:zero
   #:plus

   #:<monad-transformer>
   #:lift
   #:inner

   #:<cont>
   #:<cont-transformer>
   #:call/cc
   
   #:<state>
   #:<state-transformer>
   #:update
   #:fetch
   #:put 

   #:<maybe>
   ))
(in-package :ips-monads)

(defclass <interface> () ())

(defclass <monad> (<interface>) ())

(defgeneric result (monad value))
(defgeneric bind (monad monadic-value monadic-function))

(defmacro mlet* (monad bindings &body body)
  (if bindings
      (let* ((symbol (first (first bindings)))
	     (monad-binding (if (listp monad) 
				monad
				`(,(gensym) ,monad)))
	     (monad-name (car monad-binding)))
	`(let (,monad-binding)
	   (bind ,monad-name ,@(cdr (first bindings))
		 (lambda (,symbol)
		   ,@(when (string-equal (symbol-name symbol) "_")
			   `((declare (ignorable ,symbol))))
		   (mlet* ,monad-name  ,(cdr bindings)
		     ,@body)))))
      `(progn ,@body)))

(defclass <monad-transformer> ()
  ((inner-monad :accessor inner 
		:initarg :inner 
		:initform <identity>)))  

(defclass <identity> (<monad>) ())
(defvar <identity> (make-instance '<identity>))

(defmethod result ((m <identity>) value) value)
(defmethod bind ((m <identity>) mv mf)
  (funcall mf mv))

(assert (eql 3 (mlet* (m <identity>)
		   ((a 1)
		    (b 2))
		 (result m (+ a b)))))


(defclass <state> (<monad>) ())
(defvar <state> (make-instance '<state>))

(defgeneric update (m thing &rest args))

(defmethod result ((m <state>) value)
  (lambda (state)
    (cons value state)))

(defmethod bind ((m <state>) mv f)
  (lambda (context)
    (destructuring-bind (result . context) 
	(funcall mv context)
      (funcall (funcall f result) context))))

(defmethod update ((m <state>) f &rest args)
  (declare (ignore args))
  (lambda (s)
    (cons s (funcall f s))))

(defmethod fetch ((m <state>))
  (update m #'identity))

(defmethod put ((m <state>) state)
  (update m (lambda (_) 
	      (declare (ignore _)) 
	      state)))

(defun test-state (m)
  (assert (eql 3 (car 
		  (funcall 
		   (mlet* m
		       ((x (fetch m))
			(_ (put m 2))
			(y (fetch m)))
		     (result m (+ x y)))
		   1)))))

(test-state <state>)
			
			    
(defclass <cont> (<monad>) ())
(defvar <cont> (make-instance '<cont>))

(defmethod result ((m <cont>) value)
  (lambda (k) (funcall k value)))

;m >>= f  = Cont (\k -> runCont m (\a -> runCont (f a) k))

(defmethod bind ((m <cont>) mv mf)
  (lambda (k)
    (funcall mv
	     (lambda (a) 
	       (funcall (funcall mf a) 
			k)))))
; callCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k

(defmethod call/cc ((m <cont>) fn)
  (lambda (k) 
    (funcall (funcall fn (lambda (a) 
			   (lambda (_)  
			     (declare (ignore _)) 
			     (funcall k a)))) 
	     k)))
  

(defclass <zero-plus> () ())

(defgeneric zero (m))

(defgeneric plus (m &rest args))


(defclass <maybe> (<monad> <zero-plus>) ())
(defvar <maybe> (make-instance '<maybe>))

(defmethod result ((m <maybe>) v)
  v)

(defmethod bind ((m <maybe>) mv mf)
  (when mv (funcall mf mv)))
  
(defmethod plus ((m <maybe>) &rest mvs)
  (or (first mvs) 
      (when (rest mvs) 
	(apply #'plus m (rest mvs)))
      (zero m)))

(defmethod fail ((m <maybe>))
  (zero m))

(defmethod zero ((m <maybe>))
  nil)

(defclass <error> (<maybe>) ())
(defvar <error> (make-instance '<error>))
(defstruct exception (message "error"))

(defmethod bind ((m <error>) mv mf)
  (if (exception-p mv)
      mv 
      (funcall mf mv)))

(defmethod zero ((m <error>))
  (make-exception))
  
(defmethod plus ((m <error>) &rest mvs)
  (if (exception-p (first mvs))
      (if (rest mvs) 
	  (apply #'plus m (rest mvs))
	  (first mvs))
      (first mvs)))

(defmethod signal-error ((m <error>) message)
  (make-exception :message message))


(defclass <state-transformer> (<monad-transformer> <state>)
  ())

(defmethod result ((m <state-transformer>) value)
  (lambda (state) 
    (cons (result (inner m) value) state)))

;stm ‘bind‘ f = \s -> stm s ‘bind‘ \(v,s’) -> f v s’

(defmethod bind ((m <state-transformer>) mv mf)
  (lambda (state) 
    (bind (inner m) 
      (funcall mv state) 
      (lambda (v.s)
	(destructuring-bind (v . s) v.s
	  (funcall (funcall mf v) s))))))

(defmethod lift ((m <state-transformer>) inner-mv)
  (lambda (s) (bind (inner m) 
		inner-mv 
		(lambda (v) (result (inner m) (cons v s))))))

(defmethod zero ((m <state-transformer>))
  (lift m (zero (inner m))))

(defmethod plus ((m <state-transformer>) &rest mvs)
  (lambda (state)
    (let ((fail (gensym)))
      (labels ((%plus (mvs)
	       (bind (inner m)
		 (plus (inner m) 
		       (funcall (first mvs) state)
		       (result (inner m) fail))
		 (lambda (v) (if (eq fail v)
				 (if (rest mvs)
				     (%plus (rest mvs))
				     (zero (inner m)))
				 v)))))
	(%plus mvs)))))
       

(defclass <cont-transformer> (<monad-transformer> <cont>)
  ())

(defmethod result ((m <cont-transformer>) value)
  (lambda (k)     (funcall k (result (inner m) value))))

(defmethod bind ((m <cont-transformer>) mv mf)
  (lambda (k)
    (funcall mv
	     (lambda (contt-inner-mv)
	       (bind (inner m) contt-inner-mv 
		     (lambda (v)
		       (funcall (funcall mf v) 
				k)))))))

(defmethod lift ((m <cont-transformer>) c)
  (lambda (k) 
    (bind (inner m) 
      c
      (lambda (v) (funcall k (result (inner m) v))))))

(defmethod put ((m <cont-transformer>) v)
  (lift m (put (inner m) v)))

(defmethod fetch ((m <cont-transformer>))
  (lift m (fetch (inner m))))

	       

       
	   
	   
	    

