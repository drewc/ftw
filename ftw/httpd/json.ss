(export #t)

(import :drewc/ftw/httpd/endpoint :drewc/ftw/httpd/handler
        :std/text/json :std/srfi/13)

(def (json/OPTIONS)
  (let (o (assget "Origin" (http-request-headers*)))
    (http-response-write*
     200 `(("Access-Control-Allow-Origin" . ,(or o "*"))
           ("Access-Control-Allow-Credentials" . "true")
           ("Access-Control-Allow-Methods" . "POST, GET, OPTIONS")
           ("Access-Control-Allow-Headers" . "Content-Type"))
     #f)))

(def (respond/JSON code: (code 200) json)
  (let ((o (assget "Origin" (http-request-headers*)))
        (js (if (not (string? json))
              (json-object->string json)
              json)))
    (http-response-write* code `(("Content-Type" . "application/json")
                                 ("Access-Control-Allow-Origin" . ,(or o "*"))
                                 ("Access-Control-Allow-Credentials" . "true"))
                          js)))

(def (http-request-body-json*)
  (with-input-from-u8vector (http-request-body*) read-json))

(defsyntax (define-json-endpoint stx)
  (syntax-case stx ()
   ((macro name loc)
    (with-syntax ((opt (datum->syntax #'macro (string->symbol
                        (string-append (symbol->string (syntax->datum #'name))
                                       "/OPTIONS")))))
      #'(begin (def opt json/OPTIONS)
               (define-endpoint name loc))))))
