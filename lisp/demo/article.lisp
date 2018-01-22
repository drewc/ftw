(in-package :ftw-demo)

;;;; MODEL
(with-rs ()
  (defclass article (content)
    ((title :accessor title :index :case-insensitive-string-index
	    :initarg :title))
    (:index t)
    (:metaclass ftw-persistence:node-class)))

;;;; VIEW
(defun render-article-form (&key (action (compute-url "/store-article")) (author "") (title "") (content ""))
  (<:form 
    :class "sans" :method "POST" :action action
    (<:p (<:label :for "name" "Author") (<:br) (<:input :type "text" :name "author" :id "author" :value author))
    (<:p (<:label :for "title" "Title") (<:br) (<:input :type "text" :name "title" :id "title" :value title))
    (<:p (<:label :for "content" "Content") (<:br) (<:textarea  :rows 15 :cols 80 :name "content" :id "content" (<:as-html  content)))
    (<:input :type "submit")))

(defun render-add-article ()
  (<:h4 "Add Article")
  (render-article-form))

(defun render-edit-article (article)
  (<:h2 "Edit Article")
  (render-article-form :action (compute-url "/modify-article" :node-id (node-id article))
		       :title (title article)
		       :author (author article)
		       :content (content article)))

(defun render-view-article (article)
  (<:div 
   :id "body" 
   (<:h2 (<:as-html (title article)))
   (<:small  (<:a :href (compute-url "/edit-article" :node-id (node-id article)) " (edit)"))
   (<:h6 (<:as-html "By: " (author article)))
   (<:div 
    (<:as-html  (content article)))
   (<:hr)
   (<:fieldset 
    (<:legend "Comments:(" (<:as-html (length (article-comments article))) ")")
    (<:ul
     (dolist (comment (reverse  (article-comments article)))
       (<:li (render-view-comment comment)))))))
 

;;;; CONTROLLER
(define-demo-handler /add-article ()
  (present*  (render-add-article)))

(define-demo-handler /store-article (&query title author content)  
  (let ((node-id (node-id 
		  (with-rs () 
		    (make-instance 'article 
				   :title title 
				   :author author 
				   :content content)))))
    (redirect-to (compute-url "/view-article" :node-id node-id))))

(define-demo-handler /view-article (&activate sidebar front-page)
  (let ((article (context-value node)))
     (if article
	(with-rs ()
	  (present*  (render-view-article article)))
	(call-next-handler))))

(define-demo-handler /edit-article ()
  (let ((article (context-value node)))
    (if article
	(with-rs ()
	  (present* (render-edit-article article))))))

(define-demo-handler /modify-article (&query title author content)
  (let ((article (context-value node)))
    (if article
	(with-rs ()
	    (setf (title article) title 
		  (author article) author 
		  (content article) content)
	    (redirect-to (compute-url "/view-article" :node-id (node-id article))))
	(call-next-handler))))



