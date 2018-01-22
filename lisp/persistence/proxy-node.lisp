(in-package :ftw-persistence)

(defclass proxy-node-class (node-class)
  ())

(defclass proxy-node 
    () ())

(defgeneric dereference-proxy-node (node))

(defmethod rs::slot-missing ((class proxy-node-class) (object proxy-node) 
			     slot-name 
			     (operation (eql 'slot-value)) 
			     &optional new-value)
  (declare (ignore new-value))
  (slot-value (dereference-proxy-node object) slot-name))

(defmethod rs::slot-missing ((class proxy-node-class) (object proxy-node) 
			     slot-name 
			     (operation (eql 'setf)) 
			     &optional new-value)
  
  (setf (slot-value (dereference-proxy-node object) slot-name) new-value))

(defclass proxied-node-class (node-class)
  ())

(defgeneric proxied-slot-names (proxied-node-class)
  (:method-combination append))

(defun add-proxy-readers (slotd)
  (dolist (reader (rs::slot-definition-readers slotd)) 
    (c2mop:ensure-method 
     (rs::ensure-generic-function 
      reader :lambda-list '(object))
     `(lambda (obj)
	(slot-value obj ',(rs::slot-definition-name slotd)))
     :specializers (list (find-class 'proxy-node)))))

(defun add-proxy-writers (slotd)
  (dolist (writer (rs::slot-definition-writers slotd))
    (c2mop:ensure-method 
     (rs::ensure-generic-function 
      writer :lambda-list '(value object))
     `(lambda (value object)
	(setf (slot-value object ',(rs::slot-definition-name slotd)) value))
     :specializers (list (find-class t) (find-class 'proxy-node)))))

(defun add-proxy-accessors (slotd)
  (prog1 (rs::slot-definition-name slotd)
    (add-proxy-readers slotd)
    (add-proxy-writers slotd)))

(defun ensure-proxied-slot-names (class slot-names)
  (c2mop:ensure-method 
   #'proxied-slot-names 
   `(lambda (class)
      ',slot-names)
   :specializers (list class)
   :qualifiers '(append)))

(defmethod rs::finalize-inheritance :after ((class proxied-node-class))
  (ensure-proxied-slot-names class (mapcar #'add-proxy-accessors (rs::class-direct-slots class))))