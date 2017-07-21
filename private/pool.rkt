#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [make-pool (-> (-> any/c)
                 (-> any/c void?)
                 (or/c exact-nonnegative-integer? +inf.0)
                 (or/c exact-nonnegative-integer? +inf.0)
                 pool?)]
  [pool? predicate/c]
  [pool-lease (-> pool? lease?)]
  [pool-return (-> pool? lease? void?)]
  [pool-clear (-> pool? void?)]
  [lease? predicate/c]
  [lease-get (-> lease? any/c)]))

(require racket/function
         racket/list
         "manage.rkt")


;; Pool lease data structure

(struct lease (value-box))

(define (make-lease v) (lease (box v)))

(define (lease-try-get l) (unbox (lease-value-box l)))

(define (lease-get l)
  (define v (lease-try-get l))
  (unless v
    (error 'lease-get "attempted to access value for expired lease"))
  v)


(define (lease-release l)
  (set-box! (lease-value-box l) #f))

;; Pool core structure definition

(struct pool (manager semaphore produce release leases idle max max-idle))

(define (make-pool create delete max max-unused)
  (pool (make-manager)
        (and (not (equal? max +inf.0))
             (make-semaphore max))
        create
        delete
        (box (list))
        (box (list))
        max
        max-unused))

;; Unmanged pool utilities

(define (pool-num-active p) (length (unbox (pool-leases p))))
(define (pool-num-idle p) (length (unbox (pool-idle p))))
(define (pool-total p) (+ (pool-num-active p) (pool-num-idle p)))
(define (pool-has-capacity? p) (< (pool-total p) (pool-max p)))
(define (pool-has-idle? p) (not (zero? (pool-num-idle p))))
(define (pool-has-idle-capacity? p) (< (pool-num-idle p) (pool-max-idle p)))

(define (pool-add-lease! p v)
  (define l (make-lease v))
  (set-box! (pool-leases p) (cons l (unbox (pool-leases p))))
  l)

(define (pool-remove-lease! p l)
  (set-box! (pool-leases p) (remove l (unbox (pool-leases p))))
  (lease-release l))

(define (pool-add-idle! p v)
  (set-box! (pool-idle p) (cons v (unbox (pool-idle p)))))

(define (pool-remove-idle! p)
  (define idles (unbox (pool-idle p)))
  (set-box! (pool-idle p) (rest idles))
  (first idles))

(define (pool-lease-idle p) (pool-add-lease! p (pool-remove-idle! p)))
(define (pool-lease-new p) (pool-add-lease! p ((pool-produce p))))

;; High-level API called atomically in pool manager thread

(define (pool-clear p)
  (define (thnk)
    (define vs
      (append (for/list ([l (in-list (unbox (pool-leases p)))])
                (begin0 (lease-get l) (lease-release l)))
              (unbox (pool-idle p))))
    (set-box! (pool-leases p) '())
    (set-box! (pool-idle p) '())
    (define (release-async v)
      (thread (thunk ((pool-release p) v))))
    (for-each sync (map release-async vs)))
  (call/manager (pool-manager p) thnk))

(define (pool-lease p)
  (define (thnk)
    (cond [(pool-has-idle? p) (pool-lease-idle p)]
          [(pool-has-capacity? p) (pool-lease-new p)]
          [else #f]))
  (define maybe-lease
    (call/manager (pool-manager p) thnk))
  (or maybe-lease
      (let ()
        (sync (pool-semaphore p))
        (pool-lease p))))

(define (pool-return p l)
  (define (thnk)
    (define v (lease-try-get l))
    (when v
      (pool-remove-lease! p l)
      (if (pool-has-idle-capacity? p)
          (pool-add-idle! p v)
          ((pool-release p) v))
      (when (pool-semaphore p)
        (semaphore-post (pool-semaphore p)))))
  (call/manager (pool-manager p) thnk))
