(in-package :ftw-demo)

(defun find-index-article ()
  (with-rs ()  (ftw-persistence::find-node-in-rucksack 1)))

(deflayer front-page ())

(define-layered-method present-sidebar :in-layer front-page ()
 (parameter-bind (comment)
   (if comment 
       (<:h4 "Thanks for the Feedback!")
       (<:form 	:class "sans" :method "POST" :action (apply #'compute-url "/store-comment" :source "/?comment=t"
						    (when (context-value node)
						      (list :node-id (node-id (context-value node)))))
	(<:h4 "Have something to say? Please fill out this form:")
	(<:p (<:br)(<:input :type "checkbox" :name "interests[writing]") (<:label "Writing for the Quarterly")
	     (<:br)(<:input :type "checkbox" :name "interests[subscribing]") (<:label "Subscribing to the Quarterly")
	     (<:br)(<:input :type "checkbox" :name "interests[buying]")  (<:label "Buying individual articles")
	     (<:br)(<:input :type "checkbox" :name "interests[support]") (<:label "Supporting the Quarterly in some other way"))

	(<:p (<:label :for "name" "Name") (<:br) (<:input :type "text" :name "name" :id "name"))
	(<:p (<:label :for "email" "Email") (<:br) (<:input :type "text" :name "email" :id "email"))
	(<:p (<:label :for "comment" "Comment") (<:br) (<:textarea  :name "comment" :id "comment"))
	(<:input :type "submit")))))


(define-demo-handler index-handler (&path (path (member '("/" "/index.html") :test #'string=))
				  &activate sidebar front-page)
  (let ((article (find-index-article)))
    (setf (context-value node) article)
    (if article
	(present* 
	  (render-view-article article))
	(present*
	  (<:h2 "Welcome to FTW-CMS.")
	  (<:p "You Have no content, " (<:a :href (compute-url "/add-article") "Click Here") " to add the index page")))))