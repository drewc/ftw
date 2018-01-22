(in-package :ftw)

(defparameter *session-table* 
  nil)

(defun make-session-table ()
  (make-hash-table 
   :test #'equal 
   :synchronized t))

(define-layered-class session () 
  ((value-table :initform   
		(make-hash-table 
		 :test #'equal 
		 :synchronized t))
   (creation-time :accessor session-creation-time
		  :initform (get-universal-time))))

(defun session-value (session key)
  (gethash key (slot-value session 'value-table)))

(defun (setf session-value) (value session key)
  (setf (gethash key (slot-value session 'value-table))
	value))

(defun find-session (key &optional (session-table *session-table*))
  (gethash key session-table))

(defun (setf find-session) (value key)
  (setf (gethash key *session-table*) value))

(defun find-new-key (&optional (session-table *session-table*))
  (loop for key = (random-string) unless (find-session key session-table) :do (return key)))

(defun make-new-session ()
  (let ((key (find-new-key)))
    (setf (find-session key) (make-instance 'session))
    key))

(defun make-session-handler (session-table)
  (lambda (context)
    (lambda (k)
      (let ((*session-table* session-table))
	(funcall k context)))))

(defhandler cookie-session-handler (&context context) 
  (let* ((name (ftw-rq::cookie-value context "ftw-session-cookie"))
	(session (find-session name)))
    (setf (context :session) session)
    (call-next-handler)))
  
(defun random-string (&optional (length 32) (alphabet "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  "Returns a random alphabetic string.

The returned string will contain LENGTH characters chosen from
the vector ALPHABET.
"
  (loop with id = (make-string length)
        with alphabet-length = (length alphabet)
        for i below length
        do (setf (cl:aref id i)
                 (cl:aref alphabet (random alphabet-length)))
        finally (return id)))




		     