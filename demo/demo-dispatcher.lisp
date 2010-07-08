(in-package #:ftw-demo)

(deflayer demo)
(defdispatcher demo () :add-to toplevel)

(add-handler 'demo :name 'ftw-yaclml:handler :priority 1000)

(define-demo-handler (default-handler :priority -1) ()
  (present* (call-next-handler)))

(define-demo-handler (first-demo-handler :priority 9999) (&activate demo)
  (setf (context-value node) nil)
  (funcall-with-caught-validation-type-errors 
   (lambda () 
     (call-next-handler))))

(define-demo-handler (node-context-handler :priority 1000) (&query node-id)
  (setf (context-value node) 
	(with-rs ()  
	  (find-node-in-rucksack (parse-integer node-id))))
  (call-next-handler))













