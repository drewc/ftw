(import :clan/timestamp
        :std/srfi/19
        (rename-in :gerbil/gambit/os (time? time??)))

(export #t (import: :clan/timestamp))

(def rfc-1123-format-string "~a, ~d ~b ~Y ~H:~M:~S GMT")

(def (rfc-1123-date<-date date)
  (date->string date rfc-1123-format-string))

(def (rfc-1123-date<-timestamp timestamp)
  (rfc-1123-date<-date (date<-unix-timestamp timestamp)))

(def (rfc-1123-date<-srfi-19-time time)
  (rfc-1123-date<-timestamp (unix-timestamp<-srfi-19-time time)))

(def (rfc-1123-date<-gambit/os-time time)
  (rfc-1123-date<-srfi-19-time
   (make-time 'time-utc 0
              (inexact->exact (time->seconds time)))))
