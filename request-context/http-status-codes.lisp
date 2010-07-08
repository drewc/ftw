(in-package :ftw-request-context)

(macrolet ((x (&body defs)
             `(progn
                ,@(loop :for (name value reason-phrase)   :on defs :by #'cdddr 
			:collect  `(define-symbol-macro ,name
                                      ,(format nil "~A ~A" value reason-phrase))
                        
			))))
  (x
    +http-continue+                        100 "Continue"
    +http-switching-protocols+             101 "Switching Protocols"
    +http-ok+                              200 "OK"
    +http-created+                         201 "Created"
    +http-accepted+                        202 "Accepted"
    +http-non-authoritative-information+   203 "Non-Authoritative Information"
    +http-no-content+                      204 "No Content"
    +http-reset-content+                   205 "Reset Content"
    +http-partial-content+                 206 "Partial Content"
    +http-multi-status+                    207 "Multi-Status"
    +http-multiple-choices+                300 "Multiple Choices"
    +http-moved-permanently+               301 "Moved Permanently"
    +http-moved-temporarily+               302 "Moved Temporarily"
    +http-see-other+                       303 "See Other"
    +http-not-modified+                    304 "Not Modified"
    +http-use-proxy+                       305 "Use Proxy"
    +http-temporary-redirect+              307 "Temporary Redirect"
    +http-bad-request+                     400 "Bad Request"
    +http-authorization-required+          401 "Authorization Required"
    +http-payment-required+                402  "Payment Required"
    +http-forbidden+                       403 "Forbidden"
    +http-not-found+                       404 "Not Found"
    +http-method-not-allowed+              405 "Method Not Allowed"
    +http-not-acceptable+                  406 "Not Acceptable"
    +http-proxy-authentication-required+   407 "Proxy Authentication Required"
    +http-request-time-out+                408 "Request Time-out"
    +http-conflict+                        409 "Conflict"
    +http-gone+                            410 "Gone"
    +http-length-required+                 411 "Length Required"
    +http-precondition-failed+             412 "Precondition Failed"
    +http-request-entity-too-large+        413 "Request Entity Too Large"
    +http-request-uri-too-large+           414 "Request-URI Too Large"
    +http-unsupported-media-type+          415 "Unsupported Media Type"
    +http-requested-range-not-satisfiable+ 416 "Requested range not satisfiable"
    +http-expectation-failed+              417 "Expectation Failed"
    +http-failed-dependency+               424 "Failed Dependency"
    +http-internal-server-error+           500 "Internal Server Error"
    +http-not-implemented+                 501 "Not Implemented"
    +http-bad-gateway+                     502 "Bad Gateway"
    +http-service-unavailable+             503 "Service Unavailable"
    +http-gateway-time-out+                504 "Gateway Time-out"
    +http-version-not-supported+           505 "Version not supported"))