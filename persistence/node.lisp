(in-package :ftw-persistence)

(defclass node () ()   
  (:metaclass distributed-persistent-class)
  (:index t))

(defclass node-with-user ()
  ((user :reader node-user 
	 :initform (current-user)))
  (:metaclass node-class)
  (:index t))

(defclass node-with-unique-id ()
  ((id :reader node-id 
       :initarg :id 
       :initform (next-id :node-id)
       :index :number-index))
  (:metaclass node-class)
  (:index t))

(defmethod add-class-to-rucksack :before 
    ((class (eql (find-class 'node-with-unique-id))))
  (add-id-sequence-to-rucksack :node-id))

(defclass node-with-creation-time ()
  ((creation-time :reader node-creation-time 
		  :initform (get-universal-time)
		  :index :number-index))
  (:metaclass node-class)
  (:index t))

(defun find-node-in-rucksack (node-id)
  (find-object-using-slot 'node-with-unique-id 'id :equal node-id))

