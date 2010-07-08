(in-package :ftw-persistence)

(defclass node-class (distributed-persistent-class)
  ())

(defmethod initialize-instance :around ((class node-class) &rest initargs &key (direct-superclasses '()))
  (declare (dynamic-extent initargs))
  (if (loop for direct-superclass in direct-superclasses
	     thereis (ignore-errors (subtypep direct-superclass 'node)))
      (call-next-method)
      (apply #'call-next-method
	     class
	     :direct-superclasses
	     (append direct-superclasses
		     (list (find-class 'node)))
	     initargs)))

(defmethod reinitialize-instance :around ((class node-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
        (if (or 
		(not direct-superclasses-p)
		(loop for direct-superclass in direct-superclasses
		      thereis (ignore-errors (subtypep direct-superclass 'node))))
	  (call-next-method)
	  (apply #'call-next-method
		 class
		 :direct-superclasses
		 (append direct-superclasses
			 (list (find-class 'node)))
		 initargs)))

