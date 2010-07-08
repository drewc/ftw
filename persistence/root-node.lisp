(in-package :ftw-persistence)

(defclass root-node (parent-node) 
  ()  
  (:metaclass versioned-node-class)
  (:index t))

(defun find-root-node ()
  (rucksack-map-class  
   *rucksack* 'root-node   
   #'(lambda (instance)
       (return-from find-root-node instance))))

(defun make-root-node (&rest initargs)
  (or (find-root-node) 
      (apply #'make-instance 'root-node initargs)))

(defmethod add-class-to-rucksack :after ((class (eql (find-class 'root-node))))
  )