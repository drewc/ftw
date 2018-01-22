(cl:defpackage #:ftw-dispatcher
  (:use :ftw-dispatcher-implementation
	:ftw-dispatcher-api
	:ftw-dispatcher-syntax)
  (:export :define-dispatcher
	   :define-dispatch-handler
	   :define-handler-macro
	   :delete-handler
	   :make-dispatch-function
	   :find-dispatcher
	   :add-handler
	   :call-next-handler
	   
	   ))