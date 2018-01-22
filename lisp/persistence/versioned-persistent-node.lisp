(in-package :ftw-persistence)

(defclass versioned-node (node-with-unique-id
			  node-with-creation-time)
  ()
  (:metaclass node-class)
  (:index t))

(defclass versioned-node-current-version 
    (versioned-node)
  ((%previous-versions :accessor versioned-node-previous-versions)
   (#:current-version :accessor %versioned-node-current-version
		      :persistence nil
		      :initform (gensym)))
  (:metaclass node-class)
  (:index t))

(defclass versioned-node-previous-version (versioned-node)
  ()
  (:metaclass node-class))

(defclass versioned-node-class (node-class)
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
(defun make-new-version-using-modification-context 
    (versioned-node 
     &key (class 
	   (class-of (versioned-node-current-node versioned-node)))
     (modifications (versioned-node-modification-context versioned-node)))
  (let ((new-node (make-instance class)))
    (loop for (slot-name . value) in modifications
       :do (setf (slot-value new-node slot-name) value))
    (loop 
       :with current-node := (versioned-node-current-node versioned-node)
       :for slot-name :in (proxied-slot-names current-node)
       :unless (assoc slot-name modifications)
       :do (when (slot-boundp current-node slot-name) 
	     (setf (slot-value new-node slot-name)
		   (slot-value current-node slot-name))))
    (add-new-version versioned-node new-node)))