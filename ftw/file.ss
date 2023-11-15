(export #t file-size file-exists?)
(import
  :drewc/ftw/file/mime-type :drewc/ftw/timestamp :std/format :std/sugar
  (only-in :gerbil/gambit
           file-info-last-modification-time
           file-info file-size file-exists?)
  :std/pregexp :std/srfi/1 :std/srfi/13 :std/misc/process)

(def (file-extension pathname)
  (string-trim (path-extension pathname) #\.))

(def (file-mime-type pathname)
  (string-drop-right (run-process ["file" "--mime" "--brief" pathname]) 1))

(def (file-content-type pathname)
  (let* ((ext (file-extension pathname))
         (mtype (if (equal? ext "") #f (extension->mime-type ext)))
         (type #f))
    ;; If it is text or non-existant, use `file --mime --brief` to guess the encoding
    (when (or (not mtype)
              (and (pregexp-match "^text" mtype)
               (not (pregexp-match ";\\s*charset=" mtype))))
      (let* ((type? (try (file-mime-type pathname)
                         (catch (e) #f)))
             (sp? (if type? (string-split type? #\;) #f))
             (charset (if (or (not sp?) (null? (cdr sp?))) #f
                          (cadr sp?)))
             (bin (if charset
                    (let ((cs (string-split charset #\=)))
                      (string=? (cadr cs) "binary"))
                  #f))
             (our-type (if (and charset (not bin) mtype)
                         (format "~a;~a" mtype charset)
                         (or mtype type?))))
      (set! type our-type)))

    (or type mtype "application/octet-stream")))

(def (file-modification-rfc-1123-date pathname)
  (rfc-1123-date<-gambit/os-time
   (file-info-last-modification-time (file-info pathname))))

(def (file-name pathname)
  (path-strip-directory pathname))

(def (file-headers (file "/bin/sh"))
  (def ct (file-content-type file))
  (def cd (if (pregexp-match "^text" ct) []
              [["Content-Disposition"
                (format "attachment; filename=~a" (file-name file)) ...]]))
  (append
   [["Last-Modified" (file-modification-rfc-1123-date file) ...]
   ["Content-Length" (number->string (file-size file)) ...]
   ["Content-Type"  ct ...]
   ["Accept-Ranges" . "none"]]
   cd))
