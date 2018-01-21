;;; -*- Gerbil -*-
package: ftw
(export #t)
(import :ftw/mime-type :ftw/timestamp
        (only-in :gerbil/gambit/os 
                 file-info-last-modification-time 
                 file-info)
        :std/pregexp :std/srfi/1 :std/srfi/13 :std/misc/process)

(def (file-extension pathname)
  (let (match (pregexp-match "\\.([^.]+)$" pathname))
    (when match (second match))))

(def (file-mime-type pathname)
  (string-drop-right (run-process ["file" "--mime" "--brief" pathname]) 1))
  
(def (file-content-type pathname)
  (let* ((ext (file-extension pathname))
         (type (when ext (extension->mime-type ext))))    
    (or type "application/octet-stream")))

(def (file-modification-rfc-1123-date pathname)
  (rfc-1123-date<-gambit/os-time
   (file-info-last-modification-time (file-info pathname))))

(def (file-name pathname)
  (path-strip-directory pathname))
