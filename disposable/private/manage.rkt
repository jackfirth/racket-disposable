#lang racket/base

;; Used to define a set of functions that should be mutually atomic. The
;; functions should use call-as-managed with a manager thread, ensuring that no
;; two functions in the set are ever run concurrently on the same managed values.

(require racket/contract/base)

(provide
 (contract-out
  [manager? predicate/c]
  [make-manager (-> manager?)]
  [call/manager (-> manager? (-> any) any)]
  [manager-kill (-> manager? void?)]))

(require racket/function)


(struct manager (thread))

(define (make-manager)
  (define (exec-loop) ((thread-receive)) (exec-loop))
  (manager (thread exec-loop)))

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

(define (kill-current) (kill-thread (current-thread)))

(define (manager-kill mng)
  (define thd (manager-thread mng))
  (thread-send thd kill-current)
  (sync (thread-dead-evt thd))
  (void))
