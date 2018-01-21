;;; -*- Gerbil -*-
package: ftw/server
(export static-file-handler)

(import :ftw/http-status-code :ftw/file
        :clan/utils/base
        :gerbil/gambit/bytes :gerbil/gambit/os
        :std/net/httpd :std/generic :std/sugar :std/pregexp :std/srfi/1)

(def (static-file-handler
      pathname
      content-type: (content-type #f)
      content-disposition: (content-disposition "attachment")
      filename: (filename #t)
      last-modified: (last-modified #f)
      buffer-length: (buffer-length 8192))
  (if (not (file-exists? pathname))
    (λ (req res)
      (http-response-write
       res +http-not-found+ '(("Content-Type" . "text/plain"))
       (http-status-code-message +http-not-found+)))
    (λ (req res)
        
      (unless content-type
        (set! content-type (file-content-type pathname)))

      (unless last-modified
        (set! last-modified (file-modification-rfc-1123-date pathname)))

      (when filename
        (set! content-disposition
            (call-with-output-string
             (string-append content-disposition "; filename=")
             (λ (p) (write (cond ((string? filename) filename)
                                 ((eq? #t filename)
                                  (file-name pathname)))
                           p)))))

      (def size (number->string (file-size pathname)))
        

      (def headers [["Last-Modified" . last-modified]
                    ["Content-Length"  . size]
                    ["Content-Type"  . content-type]
                    ["Content-Disposition"  . content-disposition]
                    ["Accept-Ranges" . "none"]])

      
      (def chunk (make-bytes buffer-length))

      ;; Begin response
      (http-response-begin res +http-ok+ headers)

      ;; Blow some chunks
      (call-with-input-file pathname
        (lambda (p)
          (let loop ()
            (let (len (read-bytes chunk p))           
              (unless (= len buffer-length)
                (bytes-shrink! chunk len))
              (http-response-chunk res chunk)
              (when (= len buffer-length)
                (loop))))))
      ;; End the response
      (http-response-end res))))
