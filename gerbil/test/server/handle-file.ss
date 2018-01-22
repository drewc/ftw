
package: test/ftw/server
(export #t)
(import :ftw/server :ftw/server/handle-file
        :clan/utils/base :std/net/request :std/generic)

(defclass (test-handle-file-server ftw-server)
  (not-found))

(def test-file-text "This is a test file")

(def test-file-path "/tmp/handle-file.txt")

(def (make-test-file)
  (call-with-output-file test-file-path
    (λ (p) (display test-file-text p))))

(defmethod (ftw-server-handler (test test-handle-file-server))
  (λ (req res)
    (unless (@ test not-found) (make-test-file))
    ((static-file-handler test-file-path
                          buffer-length: 10) req res)
    (delete-file test-file-path)))


(def (test)
  (def server (make-test-handle-file-server
               address: "localhost:8242"))

  (def (test-static-file)
    (let (response (http-get "http://localhost:8242/"))
      (values (and
                (equal? (request-text response)
                        test-file-text)
                (equal? 200 (request-status response)))
              (request-status response)
              (request-text response)
              (request-headers response)
              response)))

  (def (test-not-found)
    (set! (@ server not-found) #t)
    (let (response (http-get "http://localhost:8242/"))
      (values (and              
                (equal? 404 (request-status response)))
              (request-status response)
              (request-text response)
              (request-headers response)
              response)))
    

  
  (start-ftw-server! server)

  (let (response (http-get "http://localhost:8242/"))
    (begin0 [(cons static-file: (values->list (test-static-file)))
             (cons not-found: (values->list (test-not-found)))]
      (stop-ftw-server! server))))
