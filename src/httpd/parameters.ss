(import :std/net/uri :drewc/ftw/httpd/handler)
(export #t)

(def (GET-parameters req) (form-url-decode (http-request-params req)))
(def (GET-parameters* (req #f)) (GET-parameters (or req (current-http-request))))
(def (GET-parameter name req) (assget name (or (GET-parameters req) [])))
(def (GET-parameter* name (req #f))
  (GET-parameter name (or req (current-http-request))))
