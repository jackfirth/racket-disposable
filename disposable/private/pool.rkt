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
  [lease-value (-> lease? any/c)]))

(require disposable/private/atomic-box
         racket/function
         racket/list
         racket/match
         racket/promise)

(module+ test
  (require rackunit))

(define (for-each/async f vs)
  (define (f/async v) (delay/thread (f v)))
  (for-each force (map f/async vs)))

;; Immutable representation of a pool and leases

(struct lease (value)) ;; used for eq?-ness of opaque structs
(struct stored (leases idle max max-idle) #:transparent)
(define (make-stored max max-idle) (stored '() '() max max-idle))

(define (stored-num-leases s) (length (stored-leases s)))
(define (stored-num-idle s) (length (stored-idle s)))
(define (stored-num-values s) (+ (stored-num-leases s) (stored-num-idle s)))
(define (stored-full? s) (>= (stored-num-values s) (stored-max s)))
(define (stored-idle-full? s) (>= (stored-num-idle s) (stored-max-idle s)))
(define (stored-idle-available? s) (> (stored-num-idle s) 0))

(define (stored-update s #:leases [leases-f values] #:idle [idle-f values])
  (struct-copy stored s
               [leases (leases-f (stored-leases s))]
               [idle (idle-f (stored-idle s))]))

(define (stored-add-new s v)
  (define l (lease v))
  (list (stored-update s #:leases (λ (ls) (cons l ls))) l))

(define (stored-add-idle s v) (stored-update s #:idle (λ (vs) (cons v vs))))

(define (stored-use-idle s)
  (stored-add-new (stored-update s #:idle rest) (first (stored-idle s))))

(define (stored-remove-lease s l)
  (stored-update s #:leases (λ (ls) (remove l ls))))

;; (Stored a) (-> a) -> (Maybe (Stored a, Lease a))
(define/contract (stored-get s produce)
  (-> stored? (-> any/c) (or/c #f (list/c stored? lease?)))
  (cond
    [(stored-idle-available? s) (stored-use-idle s)]
    [(stored-full? s) #f]
    [else (stored-add-new s (produce))]))

;; (Stored a) (Lease a) -> (Either (Stored a) (Stored a, a))
(define/contract (stored-return s l)
  (-> stored? lease? (or/c stored? (list/c stored? any/c)))
  (define new-s (stored-remove-lease s l))
  (define v (lease-value l))
  (if (stored-idle-full? s) (list new-s v) (stored-add-idle new-s v)))

;; (Stored a) -> [a]
(define (stored-list s)
  (append (map lease-value (stored-leases s)) (stored-idle s)))

;; Pools

(struct pool (stored sema produce release))

(define (make-pool create delete max max-unused)
  (define sema (and (not (equal? max +inf.0)) (make-semaphore max)))
  (pool (atomic-box (make-stored max max-unused)) sema create delete))

(define (pool-clear p)
  (define (clear s)
    (for-each/async (pool-release p) (stored-list s)))
  (atomic-box-close (pool-stored p) #:on-close clear))

(define (pool-return p l)
  (define (update s)
    (define new-s
      (match (stored-return s l)
        [(list (? stored? new-s) v) ((pool-release p) v) new-s]
        [(? stored? new-s) new-s]))
    (when (pool-sema p) (semaphore-post (pool-sema p)))
    new-s)
  (atomic-box-update! (pool-stored p) update
                      #:handle-closed void))

(define (pool-lease p)
  (define (apply s)
    (match (stored-get s (pool-produce p))
      [#f (values s #f)]
      [(list (? stored? new-s) (? lease? l)) (values new-s l)]))
  (or (call/atomic-box (pool-stored p) apply)
      (let ()
        (sync (pool-sema p))
        (pool-lease p))))
