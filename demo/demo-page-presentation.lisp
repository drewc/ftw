(in-package :ftw-demo)

(defun render-demo-page-surround (thunk &key (title "CL : For The Web"))
  (<:html 
   (<:head 
    (<:title (<:as-html title))
    (<:link :href "http://www.codequarterly.com/styles/reset.css" :rel "stylesheet" :type "text/css")
    (<:link :href "http://www.codequarterly.com/styles/cq.css" :rel "stylesheet" :type "text/css"))
   (<:body 
    :class "widebar"
    (<:div
     :id "container"
     (<:div 
      :id "header" :Class "gradient_white"
      (<:a :href "index.html" :id "logo"
	   (<:h1 "Common Lisp : For The Web!")
	   (<:h6 "A Language for Web Programming")))

     (funcall thunk)))))

(deflayer sidebar)

(define-layered-function present-sidebar ()
  (:method-combination arnesi:wrapping-standard)
  (:method ())
  (:method :in-layer sidebar :wrapping ()
   (<:div 
    :id "sidebar" :class "wide"
    (call-next-layered-method))))


(define-layered-method present :wrapping (thunk)
		       (call-next-layered-method))

(define-layered-method present :in-layer demo :wrapping (thunk)
  (render-demo-page-surround 
   (lambda () 
     (present-sidebar) (call-next-layered-method))))




