  ;;; (c) drewc <me@drewc.ca>

  (import :std/srfi/1 :std/srfi/95 :std/iter :drewc/ftw/httpd/endpoint/struct :std/sugar)
  (export #t)

    ;; [[[[file:~/.gerbil/pkg/github.com/drewc/ftw/ftw/httpd/endpoint.org::*endpoint/queue.ss][endpoint/queue.ss]]][]]
      (defstruct endpoint-queue (hit-counter priorities default-priority) constructor: :init!)
    
      (defmethod {:init! endpoint-queue}
        (cut struct-instance-init! <> (make-hash-table-eq) [] 42))
    ;; ends here
    ;; [[[[file:~/.gerbil/pkg/github.com/drewc/ftw/ftw/httpd/endpoint.org::*endpoint/queue.ss][endpoint/queue.ss]]][]]
      (def (find-queued ep q)
        (let/cc yup!
          (begin0 #f
            (for ((values k v) (in-hash (endpoint-queue-hit-counter q)))
              (when (eq? (endpoint-name k)
                         (endpoint-name ep))
                (yup! k))))))
    ;; ends here
    ;; [[[[file:~/.gerbil/pkg/github.com/drewc/ftw/ftw/httpd/endpoint.org::*endpoint/queue.ss][endpoint/queue.ss]]][]]
      (def (endpoint-queue-endpoint-hits q ep)
        (hash-ref (endpoint-queue-hit-counter q) ep 0))
    
      (def (increment-endpoint-hit-count! q ep)
        (hash-update!
         (endpoint-queue-hit-counter q) ep (lambda (n) (if (number? n) (1+ n) 0))))
    
      (def (reset-endpoint-queue-hit-counter! q)
        (set! (endpoint-queue-hit-counter q)
          (make-hash-table-eq))
        (map-endpoint-queue (lambda (n ep) (increment-endpoint-hit-count! q ep)) q))
    
    
    ;; ends here
    ;; [[[[file:~/.gerbil/pkg/github.com/drewc/ftw/ftw/httpd/endpoint.org::*endpoint/queue.ss][endpoint/queue.ss]]][]]
      (def (sort-endpoint-queue! q)
        (sort! (endpoint-queue-priorities q) < car)
        (for ([_ . eps] (endpoint-queue-priorities q))
          (sort! eps > (cut endpoint-queue-endpoint-hits q <>)))
        (endpoint-queue-priorities-set!
         q
         (filter (lambda (apair) (not (null? (cdr apair)))) (endpoint-queue-priorities q))))
    
      (def (map-endpoint-queue fn q)
        (for ([n . eps] (endpoint-queue-priorities q))
          (for (ep eps) (fn n ep))))
    
      (def (endpoint-queue-endpoint-index q ep)
        (let (n 0)
          (let/cc k
            (map-endpoint-queue
             (lambda (_ ep?) (if (eq? ep? ep) (k n) (set! n (1+ n)))) q)
            (k #f))))
    
    ;; ends here
    ;; [[[[file:~/.gerbil/pkg/github.com/drewc/ftw/ftw/httpd/endpoint.org::*endpoint/queue.ss][endpoint/queue.ss]]][]]
      (def (remove-endpoint-from-queue! e q)
        (awhen (e (find-queued e q))
          (for (pair (endpoint-queue-priorities q))
            (match pair
              ([_ . eps]
               (if (member e eps) (set! (cdr pair) (remove (cut eq? e <>) eps))))))
          (hash-remove! (endpoint-queue-hit-counter q) e)
            (sort-endpoint-queue! q)))
    ;; ends here
    ;; [[[[file:~/.gerbil/pkg/github.com/drewc/ftw/ftw/httpd/endpoint.org::*endpoint/queue.ss][endpoint/queue.ss]]][]]
      (def (add-endpoint-to-queue!
            endpoint queue
            priority: (priority (endpoint-queue-default-priority queue)))
        (def exists? (find-queued endpoint queue))
        (if (not exists?)
          (let (qlist (assoc priority (endpoint-queue-priorities queue)))
            (if qlist (set! (cdr qlist) (cons endpoint (cdr qlist)))
                (let (q (endpoint-queue-priorities queue))
                  (set! (endpoint-queue-priorities queue)
                    [[priority endpoint] . q])))
            (increment-endpoint-hit-count! queue endpoint)
            (sort-endpoint-queue! queue))
          (begin (remove-endpoint-from-queue! endpoint queue)
                 (add-endpoint-to-queue! endpoint queue priority: priority))))
    ;; ends here
    ;; [[[[file:~/.gerbil/pkg/github.com/drewc/ftw/ftw/httpd/endpoint.org::*endpoint/queue.ss][endpoint/queue.ss]]][]]
      (def (endpoint-queue-dispatch-function queue)
        (lambda (req res)
          (def pq (endpoint-queue-priorities queue))
          (let/cc k
            (for ([_ . eps] pq)
              (for (ep eps)
                (let (this-one-dispatched?
                      ((endpoint-dispatcher ep) req res))
                  (when this-one-dispatched?
                    (increment-endpoint-hit-count! queue ep)
                    (k #t)))))
            ;; If here, fail
            (k #f))))
    ;; ends here
