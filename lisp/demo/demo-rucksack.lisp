(in-package :ftw-demo)

(defvar *rucksack* nil)

(defun demo-rucksack ()
  (or *rucksack* 
      (setf *rucksack* (rs:open-rucksack #P"/tmp/cq/" :if-does-not-exist :create 
					 :if-exists :overwrite))))

(defun start-db ()
  (ftw-persistence::initialize-rucksack (cq-rucksack)))

(defmacro with-rs (args &body body)
  (declare (ignore args))  
  `(let ((rs:*rucksack* (demo-rucksack)))
     (rs:with-transaction (:rucksack (demo-rucksack))
    ,@body)))

(defclass content (ftw-persistence:node-with-creation-time
		   ftw-persistence:node-with-unique-id)
    ((author :accessor author :index :case-insensitive-string-index
	     :initarg :author)
     (content :accessor content 
	      :initarg :content
	      :initform "This space intentionally left blank"))
    (:index t)
    (:metaclass ftw-persistence:node-class))

#+nil(with-rs ()
  (bub::add-id-sequence-to-rucksack :node-id)
  (add-class-to-rucksack 'node)
  (add-class-to-rucksack 'node-with-creation-time)
  (add-class-to-rucksack 'node-with-unique-id)
  (add-class-to-rucksack 'content))


