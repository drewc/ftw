(defpackage :ftw-display
  (:use :cl :ftw :lol :contextl :ftw-yaclml))

(in-package :ftw-display)

(define-layered-function display-using-description 
    (object description 
	    &rest args
	    &key &allow-other-keys)
  (:method-combination arnesi:wrapping-standard))

(define-layered-function display (object &rest args 
				  &key activate deactivate
				  &allow-other-keys)
  (:method (object &rest args
	    &key activate deactivate
	    &allow-other-keys)
    (funcall-with-layer-context 
     (current-layer-context)
     (lambda ()
       (mapc #'ensure-active-layer activate)
       (mapc #'ensure-inactive-layer deactivate)
       (apply #'display-using-description 
	      object (class-of object) args)))))

(define-layered-method 
    display-using-description (object description 
			       &rest args
			       &key &allow-other-keys)
    (declare (ignore description args))
    (<:as-html object))

(define-layered-method 
    display-using-description (object (class standard-class)
			       &rest args
			       &key attributes &allow-other-keys)
    (let ((description (description-of object)))
      (if description
	  (apply #'display-using-description object description args)
	  (let ((slots (c2mop:class-slots class)))
	    (<:table
	     (if attributes 
		 (dolist (a attributes)
		   (<:tr (<:td (<:as-html a)) (<:td (<:as-html (slot-value object a)))))
		 (dolist (a slots)
		   (<:tr (<:td (<:as-html (c2mop:slot-definition-name a))) (<:td (<:as-html (ignore-errors (slot-value object (c2mop:slot-definition-name a)))))))))))))












  
  