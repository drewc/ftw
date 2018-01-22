(defpackage #:ftw-yaclml 
  (:use :ftw :cl )
  (:export
   #:yaclml-handler-wrapper))

(in-package :ftw-yaclml)

(defun yaclml-handler-wrapper (fn)
  (let ((yaclml:*yaclml-stream* 
	 ftw-monadic-dispatcher::*output-stream*))
    (funcall fn)))