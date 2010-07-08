(in-package :ftw-persistence)

(defclass parent-node (versioned-node)
  ()
  (:metaclass versioned-node-class)
  (:index t))

(defclass parent-storage-node (storage-node)
  ((children :initform nil :accessor node-children :initarg :children))
  (:default-initargs :properties '(:action :create))
  (:metaclass proxied-node-class))

(defun make-parent-node (parent))


(defun create-parent-node (&key (children nil))  
  (reload-object-from-cache 
   (apply #'make-versioned-node 'parent-node 'parent-storage-node 
	  :properties '(:action :create)
	  (when children 
	    (list :children )))))

(defun find-child-node (parent index)
  (p-aref (node-children parent) index))

(defmethod insert-child-nodes (parent children &key (index 0))
  (with-modification-context (:augment-context-p t)
    (let ((old-children (rs::persistent-data-read #'identity (node-children parent)))
	  (properties `(:action :add :nodes ,children)))
      
      (setf (node-children parent)
	    (rs::p-concatenate 'persistent-array 
			       (subseq old-children 0 index)
			       children
			       (when index 
				 (subseq old-children index)))
	    (node-properties parent) 
	    properties))))
