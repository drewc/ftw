(export #t)
(import :ftw/http-status-code
	:std/generic
	:clan/utils/base)

(defclass reply
  (content-type
   status-code
   cookie-jar
   header-list))

(def (reply-headers reply)
  ;; TODO: cookies!
  (def headers (reply-header-list reply))
  ;; See if head has content type,
  (unless (assoc "Content-Type" headers)
    ;; If not, add ours.
    (set! headers
      (cons (cons "Content-Type"
		  (reply-content-type reply))
	    headers)))
  headers)

(def (create-reply content-type: (type "text/html")
		   status-code: (code +http-ok+)
		   cookie-jar: (jar '())
		   headers: (headers '()))
  (let (self (make-reply))
    (set! (@ self content-type) type)
    (set! (@ self status-code) code)
    (set! (@ self cookie-jar) jar)
    (set! (@ self header-list) headers)))
