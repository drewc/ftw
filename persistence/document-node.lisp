(in-package :ftw-persistence)

(defclass document-node (versioned-node)
  ()
  (:metaclass versioned-node-class)
  (:index t))

(defclass composite-document-node (document-node)
  ()
  (:metaclass versioned-node-class)
  (:index t))

(defclass composite-document-storage-node (parent-storage-node)
   ()
  (:metaclass proxied-node-class))

(defclass document-text-node (document-node)
  ()
  (:metaclass versioned-node-class)
  (:index t))

(defclass document-text-storage-node (storage-node)
   ()
  (:metaclass proxied-node-class))

(defun make-document-text-node (&key text)
  (make-versioned-node 'document-text-node 'document-text-storage-node :payload text))


(defclass document-section-node (document-node)
  ()
  (:metaclass versioned-node-class)
  (:index t))

(defclass document-section-storage-node (storage-node)
   ((heading :accessor node-heading 
	     :initarg :heading 
	     :initform "New Document Node"))
  (:metaclass proxied-node-class))


(defun make-document-section-node  (&key heading body)
  (make-versioned-node 'document-section-node 'document-section-storage-inclusion :payload body :heading heading))

;; should link be a hard link to a node? it is not displayed immediately
;; link is totally inline, rather than a structural block of a document

(defclass document-link-node (document-node)
  ()
  (:metaclass versioned-node-class)
  (:index t))

(defclass document-link-storage-node (storage-node)
  ((text :accessor link-text :initarg :text :initform nil)
   (target :accessor link-target :initarg :target :initform nil))
  (:metaclass proxied-node-class))

(defmethod make-document-link-node (&key text target)
  (make-versioned-node 'document-link 'document-storage-link :text text :target target))