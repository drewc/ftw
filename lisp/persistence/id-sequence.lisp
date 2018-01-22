(in-package :ftw-persistence)


(defclass id-sequence ()
  ((value :initform 0)
   (name :initform (error "ID sequence name must be a symbol.") 
	 :index :symbol-index
	 :initarg :name))
  (:metaclass distributed-persistent-class)
  (:index t))

(defun find-id-sequence (name)
  (rucksack-map-slot 
   *rucksack* 'id-sequence  'name  
   #'(lambda (instance)
       (return-from find-id-sequence instance))
   :equal name))

(defun id-sequence-value (name)
  (slot-value (find-id-sequence name) 'value))

(defun (setf id-sequence-value) (value name)
  (setf (slot-value (find-id-sequence name) 'value) value))

(defun next-id (name)
  (incf (id-sequence-value name)))

(defun add-id-sequence-to-rucksack (name)
  (add-class-to-rucksack 'id-sequence)
  (or (find-id-sequence name)
      (make-instance 'id-sequence :name name)))
