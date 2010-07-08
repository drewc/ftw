(cl:in-package :ftw-request-context)

(defvar *request-context*)

(defun current-request-context ()
  *request-context*)

(defun funcall-with-dynamic-request-context (context thunk)
  (let ((*request-context* context)) (funcall thunk)))

(define-layered-class request-context () 
 ((env :special t :initform nil)))

(defun find-context-symbol (context symbol)
  (cdr (assoc symbol (slot-value context 'env))))

(defun intern-context-symbol (context symbol)
  (or (find-context-symbol context symbol)
      (cdar  
       (push (cons symbol (make-special-symbol ))
	     (slot-value context 'env)))))

(defun context-symbol-value (context symbol)
  (dynamic-symbol-value (intern-context-symbol context symbol)))

(defun (setf context-symbol-value) (value context symbol)
  (setf (dynamic-symbol-value (intern-context-symbol context symbol)) value))

(define-layered-function handle-request (context))
(define-layered-function html-stream (context))
(define-layered-function http-method (context))
(define-layered-function parameter-value (context name &key method))
(define-layered-function map-parameters (function context &key method))
(define-layered-function find-cookie (context name))
(define-layered-function cookie-value (context name &optional default))
(define-layered-function add-cookie (context name value &key comment domain max-age path secure))

(define-layered-function parameters-as-plist (context name &key method package)
  (:method (context name &key method (package *package*))
    (setf name (subseq (symbol-name name) 0 (- (length (symbol-name name)) 1)))
    (let ((plist (list)))
      (map-parameters 
       (lambda (k v)
	 (let ((name-len (length
			  name))
	       (key-len (length k)))
	   (when (> key-len name-len)

	     (if (and (string-equal name (ignore-errors  (subseq k 0 name-len)))
		      (eql #\[ (aref k (1+  name-len)))
		      (eql #\] (aref k (1- key-len))))
		 (setf plist (list* (intern (string-upcase (subseq k (+ 2 name-len) (1- key-len))) package) v plist))))))
       context :method method)
      plist)))

(define-layered-function request-uri (context))
(define-layered-function request-path (context))

(define-layered-function request-path-name (path)
  (:method (path)
   (let ((pos (position #\/ path :from-end t)))
     	   
     (when pos (subseq path pos)))))

(define-layered-function send-redirect (context target message))

(defun redirect-to (target &key message (context (current-request-context)))
  (send-redirect context target message))


(deflayer url ()
  ((path :special t :accessor url-path)
   (query :special t :accessor url-query)))

(define-layered-function compute-url (path &rest query)
  (:method-combination arnesi:wrapping-standard))

(define-layered-method compute-url (path &rest query)		       
  (with-output-to-string (href)
    (write-string path href)
    (when query
            (write-char #\? href)
      (loop
	 for (key value . rest) on query by #'cddr
	 do (etypecase key
              (string (write-string key href))
              (symbol (write-string (string-downcase key) href)))
	   
	 do (write-char #\= href)
	 do (princ (escape-as-uri (princ-to-string  value)) href)
	 when rest
	 do (write-char #\& href)))))

(define-layered-function continue-url (path &rest query)
  (:method-combination arnesi:wrapping-standard))

(define-layered-method continue-url (path &rest query)
  (with-output-to-string (href)
    (write-string path href)
    (when query
      (let* (names)		       
      (write-char #\? href)
      (loop
	 for (key value . rest) on query by #'cddr
	 do (etypecase key
              (string (write-string key href))
              (symbol (write-string (string-downcase key) href)))
	    (push key names)	   
	 do (write-char #\= href)
	 do (princ (escape-as-uri (princ-to-string  value)) href)
	 when rest
	 do (write-char #\& href))
      	(map-parameters 
	 (lambda (k v)
	   (unless (find k names :test #'string-equal)
	     (format href "&~A=~A" k v)))
	 (current-request-context))))))








