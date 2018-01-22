(in-package :rucksack)

(defmethod p-length ((vector persistent-array))
  (check-p-vector vector 'p-length)
  (if (p-array-has-fill-pointer-p vector)
      (p-fill-pointer vector)
      (first (p-array-dimensions vector))))

(defun p-list->list (p-list)
  (if (p-consp p-list)
      (cons (p-car p-list) (p-list->list (p-cdr p-list)))))

(defmethod p-array-has-fill-pointer-p ((array persistent-array))
  (persistent-data-read #'array-has-fill-pointer-p array))

(defmethod p-fill-pointer ((array persistent-array))
  (persistent-data-read #'fill-pointer array))

(defmethod p-vector-push-extend (value (vector persistent-array))
  (rs::persistent-data-write (lambda (val vec)
			       (vector-push-extend val vec))
			     vector
			     value)) 

(defmethod p-concatenate (result-type &rest sequences)
  (flet ((seq (seq)
	   (etypecase seq
	     (sequence seq)
	     (persistent-array (persistent-data-read #'identity seq)))))
    (let ((result (apply #'concatenate 'vector 
			 (mapcar #'seq sequences))))
      (ecase result-type
	(persistent-array (p-make-array (length result) :initial-contents result))))))

(defmethod slot-persistence (slotd)
  nil)

(defmethod rs::load-object :around (object-id transaction cache)
  (flet ((loader () 
	     (multiple-value-bind (object most-recent-p) 
		 (call-next-method object-id transaction cache)
	       (loop 
		  :for slotd :in (remove-if #'rs::slot-persistence (rs::class-slots (class-of object)))
		  :when (and (rs::slot-definition-initfunction slotd)
			     (not (rs::slot-boundp-using-class (class-of object) object slotd)))
		  :do
		  (setf (rs::slot-value-using-class (class-of object) object slotd)
			(funcall (rs::slot-definition-initfunction slotd))))
	       (values object most-recent-p))))
    
      (if transaction 
	  (loader)
	  (with-transaction (:rucksack (rs::rucksack cache))
	    (setf transaction (current-transaction))   
	    (loader)))))


