(in-package :ftw-persistence)

(defclass user ()
  ((username :accessor user-name :initarg :name 
	     :index :string-index)
   (password :accessor user-password :initarg :password)
   (email :accessor user-email :initarg :email :index :string-index))
  (:metaclass distributed-persistent-class)
  (:index t))

(defclass user-rucksack (distributed-rucksack) 
  ((username :accessor rucksack-username :initarg :username)))

(defmethod user-rucksack-path (user)
  (merge-user-rucksack-pathname (user-name user)))

(defun merge-user-rucksack-pathname (username)
  (merge-pathnames (make-pathname 
		    :directory `(:relative ,username)) 
		   *rucksack-path*))

(defun make-user (&rest initargs 
		  &key (name (error "User must have a name"))
		  (if-does-not-exist :create) 
		  (if-exists :error)
		  &allow-other-keys
		  &aux (user-initargs (remove-from-plist initargs :if-does-not-exist :if-exists)))

  (with-rucksack (rs (merge-user-rucksack-pathname name) 
		     :if-does-not-exist if-does-not-exist 
		     :if-exists if-exists)
    (with-transaction () 
      (add-class-to-rucksack 'user)
      (let ((user-exists? (find-user name)))
	(if user-exists?
	    (ecase if-exists
	      (:error (error "User ~A already exists" name))
	      (:overwrite (return-from make-user (apply #'reinitialize-instance user-exists? user-initargs)))
	      (:supersede (rucksack-delete-object *rucksack* user-exists?)
			  (apply #'make-instance 'user user-initargs)))
	    (reload-object-from-cache 
	     (apply #'make-instance 'user user-initargs)))))))

(defun find-user (username)
  (find-object-using-slot 'user 'username :equal username)
  (if (and *rucksack* (equalp (rucksack-directory *rucksack*)
			      (merge-user-rucksack-pathname username)))
      (find-object-using-slot 'user 'username :equal username)
      (with-rucksack (rs (merge-user-rucksack-pathname username)
			 :if-does-not-exist :error)
	(with-transaction () 
	  ))))


(defun initialize-rucksack-for-user (user)
  (with-rucksack (rs (user-rucksack-path user) 
		     :if-does-not-exist :error)
    (with-transaction () 
      (initialize-rucksack *rucksack*))))

(defun create-user (&rest make-user-args)
  (let ((user (apply #'make-user make-user-args)))
    (prog1 user
      (initialize-rucksack-for-user user))))

(defvar *current-user*)

(defun current-user ()
  *current-user*)

(defun funcall-with-current-user (thunk user)
  (let ((*current-user* user))
    (funcall thunk)))

(defmacro with-current-user ((user) &body body)
  `(funcall-with-current-user (lambda (),@body ) ,user))

(defun funcall-with-user-rucksack (thunk &optional (user (current-user)))
  (with-current-user (user) 
    (with-rucksack (rs (user-rucksack-path user) :if-does-not-exist :error)
      (with-transaction (:inhibit-gc t)
	(funcall thunk)))))

(defmacro with-user-rucksack ((&optional (user (current-user))) &body body)
  `(funcall-with-user-rucksack
    (lambda ()
      ,@body)
    ,user))