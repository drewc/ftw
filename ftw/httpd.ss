;;; -*- Gerbil -*-
;;; (C) drewc
;;; embedded HTTP/1.1 server For The Web!

(import :drewc/ftw/httpd/handler 
        :drewc/ftw/httpd/endpoint
        :drewc/ftw/httpd/cookies
        :drewc/ftw/httpd/parameters
        :std/net/httpd
        #;:std/net/socket
        :std/os/socket)
(export (import: :drewc/ftw/httpd/handler)
        (import: :drewc/ftw/httpd/endpoint)
        (import: :drewc/ftw/httpd/cookies)
        (import: :drewc/ftw/httpd/parameters)
        (import: :std/net/httpd)
         #t)

(def (start-ftw-http-server! mux: (mux default-endpoint-http-mux)
                             backlog: (backlog 10)
                             sockopts: (sockopts [SO_REUSEADDR])
                             . addresses)
  (apply start-http-server! mux: mux backlog: backlog sockopts: sockopts addresses))

(def stop-ftw-http-server! stop-http-server!)
