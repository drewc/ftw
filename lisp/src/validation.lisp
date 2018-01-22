(in-package :ftw)

(deflayer validation)

(define-layered-class ftw-rq:request-context :in-layer validation ()
 ((validation-conditions :accessor validation-conditions :initform nil)))

(defun funcall-with-caught-validation-type-errors (thunk)
  (with-active-layers (validation)
    (handler-bind ((validation-condition 
		    (lambda (c)
		      (push c (validation-conditions (current-request-context))))))
      (funcall thunk))))

(defvar *validators* (make-hash-table))

(define-condition validation-condition (simple-condition)
  ())

(define-condition parameter-validation-condition (validation-condition)
  ((name :accessor parameter-validation-condition-parameter-name
	 :initarg :parameter-name)))

(define-layered-function validate-parameter (type name value &rest args &key &allow-other-keys))

(define-layered-function make-validation-condition ())

(define-layered-method validate-parameter (type name value 
						&key 
						(format-control "~A is not of type ~A")
						(format-arguments (list value type)))
 (let ((typep (typep value type)))
   (or typep (values nil (make-condition 'parameter-validation-condition :expected-type type :datum value :parameter-name name 
					 :format-control format-control :format-arguments format-arguments"parameter ~A is not of type ~A")))))




