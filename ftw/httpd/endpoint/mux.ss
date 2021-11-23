(import :drewc/ftw/httpd/endpoint/queue :std/net/httpd/mux :std/net/httpd
        :gerbil/gambit/exceptions :std/logger :clan/error)
(export #t)
(defstruct endpoint-http-mux
  (queue sort-every reset-every handler error-handler 404-handler)
  constructor: :init!)

(def (default-endpoint-error-handler err req res)
  (log-error
   (string-append "Endpoint Handler Error for" (http-request-path req)) err)
  (http-response-write
   res 500 '(("Content-Type" . "text/plain")) "Endpoint Handler Error"))

(def (default-endpoint-404-handler req res)
  (http-response-write
   res 404 '(("Content-Type" . "text/plain"))
   "these aren't the droids you are looking for"))

(def (endpoint-http-mux-dispatch-function
      mux
      (q (endpoint-http-mux-queue mux))
      (sort-every (endpoint-http-mux-sort-every mux))
      (reset-every (endpoint-http-mux-reset-every mux)))
  (let ((sc 0) (rc 0))
    (lambda (req res)
      (with-catch
       (lambda (e) (begin0 #t ((endpoint-http-mux-error-handler mux) e req res)))
       (lambda ()
         (if (not ((endpoint-queue-dispatch-function q) req res))
           (begin0 #t ((endpoint-http-mux-404-handler mux) req res))
           (begin0 #t
             (when (= sort-every sc)
               (sort-endpoint-queue! q) (set! sc 0))
             (set! sc (1+ sc))
             (when (= reset-every rc)
               (reset-endpoint-queue-hit-counter! q) (set! rc 0))
             (set! rc (1+ rc)))))))))

(defmethod {:init! endpoint-http-mux}
  (lambda (self sort-every: (se 100) reset-every: (re 1000)
           queue: (q (endpoint-queue)))
    (struct-instance-init!
     self q se re
     (endpoint-http-mux-dispatch-function self q se re)
     default-endpoint-error-handler
     default-endpoint-404-handler)))

(def (add-endpoint-to-mux! ep mux . queue-args)
  (apply add-endpoint-to-queue! ep (endpoint-http-mux-queue mux) queue-args))

(def (remove-endpoint-from-mux! ep mux)
  (remove-endpoint-from-queue! ep (endpoint-http-mux-queue mux)))

(defmethod {get-handler endpoint-http-mux}
  (lambda (self . _)
    (endpoint-http-mux-handler self)))

(defmethod {put-handler! endpoint-http-mux}
  (lambda (mux host path handler)
    (error "Put handler unbound")))
