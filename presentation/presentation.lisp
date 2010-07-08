(defpackage #:ftw-presentation
  (:use :cl :contextl)
  (:export #:present
	   #:present*
	   #:presentation-page-title
	   #:page-title))

(in-package :ftw-presentation)

(define-layered-function present (thunk)
  (:method-combination arnesi:wrapping-standard))

(defun page-title ()
  (ftw-presentation:presentation-page-title
   (contextl:find-layer 'presentation)))

(deflayer presentation ()
  ((page-title :accessor presentation-page-title
	       :initform "Welcome to For The Web!"
	       :initarg :page-title
	       :special t)))

(defmacro present* (&body body)
  `(present (lambda () ,@body)))


(define-layered-method present (thunk)
  (funcall thunk))











