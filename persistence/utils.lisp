(in-package :ftw-persistence)

(defclass common-superclass-class (standard-class)
  ((common-superclass
    :accessor class-common-superclass
    :initform (find-class t)
    :initarg :common-superclass)))

(defmethod c2mop:validate-superclass ((class common-superclass-class) (superclass standard-class)) 
  t)

(defmethod initialize-instance :around ((class common-superclass-class) 
					&rest initargs 
					&key (direct-superclasses '())
					     (common-superclass '()))
  (declare (dynamic-extent initargs))
  (if (loop for direct-superclass in direct-superclasses
	     thereis (ignore-errors (subtypep direct-superclass common-superclass)))
      (call-next-method)
      (apply #'call-next-method
	     class
	     :direct-superclasses
	     (append direct-superclasses
		     (list (find-class common-superclass)))
	     initargs)))

(defmethod reinitialize-instance :around ((class common-superclass-class) &rest initargs 
					  &key (direct-superclasses '() direct-superclasses-p)
					        (common-superclass (class-common-superclass class) common-superclass-p))
  (declare (dynamic-extent initargs))
        (if (or 
		(not direct-superclasses-p)
		(not common-superclass-p)
		(loop for direct-superclass in direct-superclasses
		      thereis (ignore-errors (subtypep direct-superclass common-superclass))))
	  (call-next-method)
	  (apply #'call-next-method
		 class
		 :direct-superclasses
		 (append direct-superclasses
			 (list (find-class common-superclass)))
		 initargs)))


(defparameter *rucksack-path* #P"/tmp/rucksack/")

(defun merge-rucksack-pathname (path)
  (merge-pathnames path *rucksack-path*))

(defun class-all-subclasses (class)
  (remove-duplicates 
   (let ((subs (rs::class-direct-subclasses class)))
     (append subs (loop for sub in subs append (class-all-subclasses sub))))))

(defmethod print-object ((object persistent-object) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "#~D~@[ with transaction id ~D~]"
            (slot-value object 'object-id)
            (and (slot-boundp object 'transaction-id)
		 (slot-value object 'transaction-id)
		 ))))

(defun find-object-using-slot (class slot &rest args-to-map-index)
  (apply 
   #'rucksack-map-slot *rucksack* class slot 
   #'(lambda (object)
       (return-from find-object-using-slot object))
   args-to-map-index))

(defun find-objects-using-slot (class slot &rest args-to-map-index)
  (let ((objects '()))
    (apply 
     #'rucksack-map-slot *rucksack* class slot 
     #'(lambda (object)
	 (push object objects))
     args-to-map-index)
    ( nreverse objects)))

(defun reload-object-from-cache (object)
  (cache-get-object (object-id object) (cache object)))



(defclass common-superclasses-class (standard-class)
  ((common-superclass-list 
    :accessor class-common-superclass-list
    :initform (find-class t)
    :initarg :common-superclass-list)))

(defmethod c2mop:validate-superclass ((class common-superclass-class) (superclass standard-class)) 
  t)

(defmethod initialize-instance :around ((class common-superclasses-class) 
					&rest initargs 
					&key (direct-superclasses '())
					     (common-superclass '()))
  (declare (dynamic-extent initargs))
  (if (loop for direct-superclass in direct-superclasses
	     thereis (ignore-errors (subtypep direct-superclass common-superclass)))
      (call-next-method)
      (apply #'call-next-method
	     class
	     :direct-superclasses
	     (append direct-superclasses
		     (setf (class-common-superclass-list class)
			   (cons (find-class common-superclass)
				 (class-common-superclass-list class))
			   ) )
	     initargs)))

(defmethod reinitialize-instance :around ((class common-superclasses-class) &rest initargs 
					  &key (direct-superclasses '() direct-superclasses-p)
					        (common-superclass (class-common-superclass class) common-superclass-p))
  (declare (dynamic-extent initargs))
        (if (or 
		(not direct-superclasses-p)
		(not common-superclass-p)
		(loop for direct-superclass in direct-superclasses
		      thereis (ignore-errors (subtypep direct-superclass common-superclass))))
	  (call-next-method)
	  (apply #'call-next-method
		 class
		 :direct-superclasses
		 (append direct-superclasses
			 (list (find-class common-superclass)))
		 initargs)))