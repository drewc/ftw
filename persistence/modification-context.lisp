(in-package :ftw-persistence)

(defvar *modification-context*)

(defun node-modification-context (node)
  (if *modification-context*
      (gethash node *modification-context*)
      (error "Attempt to access NULL modification context. See FUNCALL-WITH-MODIFICATION-CONTEXT")))

(defun (setf node-modification-context) (value node)
  (if *modification-context*
      (setf (gethash node *modification-context*) value)
      (error "Illegal attempt to modify node ~A without a modification context. See FUNCALL-WITH-MODIFICATION-CONTEXT" node)))

(defun add-modification-to-context (node slot-name new-value)
  (let ((previous-mod (assoc slot-name (node-modification-context node))))
    (if previous-mod 
	(setf (cdr previous-mod) new-value)
	(setf (node-modification-context node)
	      (acons slot-name new-value 
		     (node-modification-context node))))))

(defun funcall-with-modification-context (thunk &key augment-context-p)
  (let ((entry-point-p (or (not (boundp '*modification-context*))
			   (not augment-context-p))))
    (let ((*modification-context* 
	   (if (or entry-point-p
		   (not (boundp '*modification-context*)))
	       (make-hash-table)
	       *modification-context*)))

      (prog1 (funcall thunk))
      (when entry-point-p
	(maphash (lambda (versioned-node modifications)
		   (make-new-version-using-modification-context 
		    versioned-node 
		    :modifications modifications))
		 *modification-context*)))))

(defmacro with-modification-context ((&key augment-context-p) &body body)
  `(funcall-with-modification-context 
    (lambda () ,@body) 
    :augment-context-p ,augment-context-p))

(defclass modification-context-class ()
  ())

(defmethod rs::slot-missing :around ((class modification-context-class) 
				     node
				     slot-name 
				     (operation (eql 'setf)) 
				     &optional new-value)
  (prog1 new-value 
    (funcall-with-modification-context 
     (lambda ()
       (add-modification-to-context node slot-name new-value))
     :augment-context-p t)))