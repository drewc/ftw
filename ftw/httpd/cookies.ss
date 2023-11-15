(import :drewc/ftw/httpd/handler :std/srfi/13)
(export #t)

(def (http-request-cookies req)
  (let* ((hs (http-request-headers* req))
         (cj (assget "Cookie" hs))
         (cookies
          (and cj (map (lambda (c) (match (map string-trim (string-split c #\=))
                                ([a b] [a . b])))
                       (string-split cj #\;)))))

    (or cookies [])))

(def (http-request-cookies* (req #f))
  (http-request-cookies (or req (current-http-request))))
