(in-package :ftw-demo)

(with-rs () 
  (defclass comment (content)
    ((email-address :initarg :email-address 
		    :accessor email-address 
		    :index :case-insensitive-string-index)
     (properties :initarg :properties 
		 :accessor properties
		 :initform nil)
     (parent-node-id :initarg :parent-id
		     :accessor parent-id
		     :index :number-index))
    (:index t)
    (:metaclass ftw-persistence:node-class)))

(defun render-view-comment (comment)
  (<:p "comment from " (<:as-html (author comment)))
  (<:pre 
   (<:as-html (content comment)))
  (<:p 
   "interests : "
   (<:ul 
    (loop for (key val) on (properties comment) by #'cddr do (<:li (<:as-html  key))))))

(defun article-comments (article)
  (ftw-persistence::find-objects-using-slot 'comment 'parent-node-id :equal (node-id article)))

(define-demo-handler ?_action=store-comment (&query  name comment email 
						  (interests[] (optional)))
  
  
  (let ((node (context-value node)))
    (parameter-bind (source)
      (if node
	  (with-rs () 
	    (make-instance 'comment 
			   :email-address email
			   :content comment
			   :author name
			   :properties interests
			   :parent-id (node-id node))
		   (redirect-to (or source (compute-url "/view-article" :node-id (node-id node)))))
	  (call-next-handler)))))