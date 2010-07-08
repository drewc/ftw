(defpackage #:ftw-yaclml 
  (:export #:yaclml-handler #:page-surround-handler)
  (:use :ftw :cl))

(in-package :ftw-yaclml)

(ftw:defhandler ftw-yaclml:yaclml-handler (&context context)
  (let ((yaclml:*yaclml-stream* (ftw:html-stream context)))
    (ftw:call-next-handler)))

(ftw:defhandler ftw-yaclml:page-surround-handler (&context context)
  (render-yaclml-page-surround (lambda () (call-next-handler))))

(defun render-yaclml-page-surround (thunk)
  (<:html :prologue "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
	  (<:head 
	   (<:title (<:as-html (ftw-presentation:page-title)))
	   (<:link :rel "stylesheet" :href "http://common-lisp.net/~dcrampsie/blueprint/screen.css" :type "text/css" :media "screen, projection")

	   (<:link :rel "stylesheet" :href "http://common-lisp.net/~dcrampsie/blueprint/print.css" :type "text/css" :media "print")

	   (<:as-is "<!--[if lt IE 8]>")
	   (<:link :rel "stylesheet" :href "http://common-lisp.net/~dcrampsie/blueprint/ie.css" :type "text/css" :media "screen, projection")
	   (<:as-is "<![endif]-->")
	   
          #+nil  (<:script :type "text/javascript" :src "http://common-lisp.net/~dcra\
mpsie/javascript/curvycorners/curvycorners.js"))
	  

	  (<:body :class "body main-background" :id "body"
	   
		  (funcall thunk))))