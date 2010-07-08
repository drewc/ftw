(in-package :ftw-persistence)

(defclass distributed-rucksack (rs::serial-transaction-rucksack) ())

(defclass distributed-persistent-class (persistent-class)
  ())

(defclass distributed-persistent-object () 
  ())

(defmethod initialize-instance :around ((class distributed-persistent-class) &rest initargs &key (direct-superclasses '()))
  (if (loop for direct-superclass in direct-superclasses
	     thereis (ignore-errors (subtypep direct-superclass 'distributed-persistent-object)))
      (call-next-method)
      (apply #'call-next-method
	     class
	     :direct-superclasses
	     (append direct-superclasses
		     (list (find-class 'distributed-persistent-object)))
	     initargs)))

(defmethod reinitialize-instance :around ((class distributed-persistent-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
        (if (or 
		(not direct-superclasses-p)
		(loop for direct-superclass in direct-superclasses
		      thereis (ignore-errors (subtypep direct-superclass 'distributed-persistent-object))))
	  (call-next-method)
	  (apply #'call-next-method
		 class
		 :direct-superclasses
		 (append direct-superclasses
			 (list (find-class 'distributed-persistent-object)))
		 initargs)))

(defmethod initialize-rucksack (rucksack)
  (let ((rs:*rucksack* rucksack))
    (map nil #'add-class-to-rucksack 
	 (print (class-all-subclasses (find-class 'distributed-persistent-object))))))


(defgeneric add-class-to-rucksack (class)
  (:method ((class symbol))
    (add-class-to-rucksack (find-class class)))
  (:method ((class distributed-persistent-class))
   (warn "Adding ~A" class)
    (rs::update-indexes class)
    (rs::update-slot-info class))
  (:method :before ((class distributed-persistent-class)) 
    (unless (rucksack::class-finalized-p class)
      (rucksack::finalize-inheritance class))))





