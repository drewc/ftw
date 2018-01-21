;; -*- Gerbil -*-
package: ftw
(export #t)
(import :clan/utils/base :std/net/httpd :std/net/address
        :std/generic)

(defclass ftw-server 
  (address httpd))

(defgeneric ftw-server-handler
  (λ (self)
    (λ (req res)
      (http-response-write
       res 404 '(("Content-Type" . "text/plain"))
       "these aren't the droids you are looking for.\n"))))

(def default-ftw-server-address
 "localhost:8042")

(def (start-ftw-server! ftw-server)

  (unless (ftw-server-address ftw-server)
    (set! (ftw-server-address ftw-server)
      default-ftw-server-address))
  
  (def httpd (start-http-server!
              (ftw-server-address ftw-server)
              mux: (make-default-http-mux
                    (λ (req res)
                      ((ftw-server-handler ftw-server) req res)))))
    (set! (ftw-server-httpd ftw-server) httpd)
    httpd)

(def (stop-ftw-server! server)
  (stop-http-server! (ftw-server-httpd server)))
