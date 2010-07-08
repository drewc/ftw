(in-package :cl-user)

(defpackage :for-the-web
  (:nicknames :ftw)
  (:use :cl :contextl 
	:ftw-dispatcher 
	:ftw-request-context
	:ftw-presentation)
  (:export 
   :current-request-context
   :funcall-with-caught-validation-type-errors
   :http-method
   :optional
   :parameter-bind
   :context
   :context-value
   :toplevel
   :starts-with
   :compute-url
   :redirect-to
   :defdispatcher
   :define-handler-macro
   :add-handler
   :defhandler
   :call-next-handler
   :html-stream
   :present
   :present*
   #:call-handler))
