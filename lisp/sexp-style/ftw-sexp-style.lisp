(defpackage #:ftw-sexp-style
  (:use :ftw :cl)
  (:export
   #:sexp-style-handler-wrapper))

(in-package :ftw-sexp-style)

(defun sexp-style-handler-wrapper (fn)
  (let ((sexp-style:*css-stream* 
	 ftw-monadic-dispatcher::*output-stream*))
    (funcall fn)))