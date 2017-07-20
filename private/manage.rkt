#lang racket/base

;; Used to define a set of functions that should be mutually atomic. The
;; functions should use call-as-managed with a manager thread, ensuring that no
;; two functions in the set are ever run concurrently on the same managed values.

(require racket/contract/base)

(provide
 (contract-out
  [manager? predicate/c]
  [make-manager (-> manager?)]
  [call/manager (-> manager? (-> any) any)]))


(struct manager (thread))

(define (make-manager)
  (manager (thread (λ () (let loop () ((thread-receive)) (loop))))))

(define (call/manager mng thunk)
  (thread-resume (manager-thread mng) (current-thread))
  (define sema (make-semaphore))
  (define result (box #f))
  (thread-send (manager-thread mng)
               (lambda ()
                 (set-box! result (call-with-values thunk vector))
                 (semaphore-post sema)))
  (semaphore-wait sema)
  (vector->values (unbox result)))

