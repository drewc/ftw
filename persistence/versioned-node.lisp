(in-package :ftw-persistence)

(defclass versioned-node-class (modification-context-class 
				proxy-node-class
				node-class)
  ())


(defmethod versioned-node-current-version (node)
  (when (boundp (%versioned-node-current-version node))
    (symbol-value (%versioned-node-current-version node))))

(defmethod versioned-node-current-version :around (node)
  (or (call-next-method) 
      (1- (p-length (versioned-node-storage-nodes node)))))

(defmethod versioned-node-current-node (node)
  (p-aref (versioned-node-storage-nodes node)
	  (versioned-node-current-version node)))

(defmethod dereference-proxy-node ((node versioned-node))
  (versioned-node-current-node node))

(defmethod versioned-node-find-version (node version)
  (p-aref (versioned-node-storage-nodes node) version))

(defun funcall-with-node-version (node version thunk)
  (progv (list (%versioned-node-current-version node))
      (list version) 
    (funcall thunk)))

(defmacro with-node-version ((node version) &body body)
  `(funcall-with-node-version 
    ,node ,version
    (lambda () ,@body)))

(defun make-versioned-node (versioned-node-class storage-class &rest initargs)
  (make-instance versioned-node-class
    :storage-nodes 
    (p-make-array 1 :initial-element 
     (apply #'make-instance storage-class initargs))))

(defun add-new-version (versioned-node new-node)
  (prog1 versioned-node  
    (let* ((storage-nodes (versioned-node-storage-nodes versioned-node))
	   (new-storage-nodes (p-make-array (1+ (p-length storage-nodes)))))
      ;; TODO need to write p-vector-push-extend i suppose.
      (loop 
	 :for n :from 0 :to (1- (p-length storage-nodes))
	 :do (setf (p-aref new-storage-nodes n)
		   (p-aref storage-nodes n)) 
	 :finally (setf (p-aref new-storage-nodes n) new-node))
      (setf (versioned-node-storage-nodes versioned-node)
	    new-storage-nodes))))

(defun make-new-version (versioned-node class &rest initargs)
  (let* ((new-node (apply #'make-instance class initargs)))
    (add-new-version versioned-node new-node)))

(defun versioned-node-modification-context (versioned-node)
  (gethash versioned-node *modification-context*))

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

(defun create-versioned-node (class &rest node-initargs)
  (reload-object-from-cache (apply #'make-versioned-node 'versioned-node class node-initargs)))
