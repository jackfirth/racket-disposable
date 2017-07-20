#lang racket/base

;; A disposable value is a value that encompasses external resources that must
;; be released when the value is no longer in use.

(require racket/contract)

(provide
 with-disposable
 (contract-out
  [rename disposable* disposable (-> (-> any/c) (-> any/c void?) disposable?)]
  [make-disposable (-> (-> (values any/c (-> void?))) disposable?)]
  [disposable? predicate/c]
  [disposable/c (-> (or/c chaperone-contract? flat-contract?) contract?)]
  [call/disposable (-> disposable? (-> any/c any) any)]
  [disposable-apply (->* (procedure?) #:rest (listof disposable?) disposable?)]
  [disposable-pure (-> any/c disposable?)]
  [disposable-chain (-> disposable? (-> any/c disposable?) disposable?)]
  [disposable-pool (->* (disposable?)
                        (#:max (or/c exact-nonnegative-integer? +inf.0)
                         #:max-idle (or/c exact-nonnegative-integer? +inf.0)
                         #:sync-release? boolean?)
                        (disposable/c disposable?))]
  [disposable/async-dealloc (-> disposable? disposable?)]
  [acquire (->* (disposable?) (#:dispose-when evt?) any/c)]
  [acquire-global (->* (disposable?) (#:plumber plumber?) any/c)]
  [acquire-virtual (-> disposable? (-> any/c))]))

(module+ private-unsafe
  (provide
   (contract-out
    [acquire! (-> disposable? (values any/c (-> void?)))])))

(require (for-syntax racket/base)
         racket/function
         racket/list
         racket/promise
         syntax/parse/define
         "private/pool.rkt")

(module+ test
  (require rackunit
           syntax/macro-testing))

;; Kernel API

(struct disposable (proc)
  #:constructor-name make-disposable)

(define (acquire! disp) ((disposable-proc disp)))

;; Construction sugar

(define (disposable* alloc dealloc)
  (make-disposable (thunk (define v (alloc)) (values v (thunk (dealloc v))))))

;; Contracts

(define (disposable/c value/c)
  (struct/c disposable (-> (values value/c any/c))))

;; Safe caller interface

(define (call/disposable disp f)
  (define-values (v dispose!) (acquire! disp))
  (dynamic-wind void (thunk (f v)) dispose!))

(begin-for-syntax
  (define-syntax-class bindings
    (pattern ([id:id expr:expr] ...)
             #:fail-when (check-duplicate-identifier (syntax->list #'(id ...)))
             "duplicate identifiers not allowed")))

(define-simple-macro (with-disposable bindings:bindings body:expr ...+)
  (call/disposable (disposable-apply list bindings.expr ...)
                   (λ (vs) (apply (λ (bindings.id ...) body ...) vs))))

(module+ test
  (check-exn #rx"with-disposable: duplicate identifiers not allowed"
             (thunk
              (convert-compile-time-error
               (with-disposable ([a 1] [a 2])
                 (void)))))
  (check-exn #rx"with-disposable"
             (thunk
              (convert-compile-time-error
               (with-disposable ([a 1]))))))

;; Safe monadic compositional interface

(define (map-async f vs)
  (map force (for/list ([v (in-list vs)]) (delay (f v)))))

(define (acquire/list! disp) (call-with-values (disposable-proc disp) list))
(define (acquire-all! disps) (map-async acquire/list! disps))

(define (disposable-pure v) (make-disposable (thunk (values v void))))

(define (disposable-apply f . disps)
  (make-disposable
   (thunk
    (define v+dispose!-pairs (acquire-all! disps))
    (define (dispose-all!)
      (map-async (λ (dispose!) (dispose!)) (map second v+dispose!-pairs))
      (void))
    (values (apply f (map first v+dispose!-pairs)) dispose-all!))))

(define (disposable-chain disp f)
  (make-disposable
   (thunk
    (define-values (v v-dispose!) (acquire! disp))
    (define-values (f-v f-dispose!) (acquire! (f v)))
    (define (dispose-all!) (f-dispose!) (v-dispose!))
    (values f-v dispose-all!))))

;; Disposables tied to an event (by default, the lifetime of the current thread)

(define (current-thread-dead) (thread-dead-evt (current-thread)))

(define (acquire disp #:dispose-when [evt (current-thread-dead)])
  (define-values (v dispose!) (acquire! disp))
  (thread (thunk (sync evt) (dispose!)))
  v)

;; Plumber-enabled globally allocated disposables

(define (acquire-global disp #:plumber [plumber (current-plumber)])
  (define-values (v dispose!) (acquire! disp))
  (define (flush! handle)
    (dispose!)
    (plumber-flush-handle-remove! handle))
  (plumber-add-flush! plumber flush!)
  v)

;; Async deallocation

(define (disposable/async-dealloc disp)
  (make-disposable
   (λ ()
     (define-values (v dispose!) (acquire! disp))
     (define (dispose-async!) (thread dispose!))
     (values v dispose-async!))))

;; Pooled disposables

(define (pool-disposable produce release max max-idle)
  (define (create) (make-pool produce release max max-idle))
  (disposable* create pool-clear))

(define (lease-disposable v+dispose-pool)
  (define (get-lease-value v+dispose-lease)
    (first (lease-get v+dispose-lease)))
  (disposable-apply get-lease-value
                    (disposable* (thunk (pool-lease v+dispose-pool))
                                 (λ (l) (pool-return v+dispose-pool l)))))

(define (disposable-pool item-disp
                         #:max [max +inf.0]
                         #:max-idle [max-idle 10]
                         #:sync-release? [sync-release? #f])
  (define (produce) (acquire/list! item-disp))
  (define (release v-dispose-pair) ((second v-dispose-pair)))
  (define (lease-disposable* pool)
    ;; Leases can be returned asynchronously, but the pool itself is deallocated
    ;; synchronously. This enables a globally allocated pool to safely
    ;; deallocate all values before finishing deallocation while individual
    ;; leases can be returned without blocking the leasing thread on a
    ;; potentially expensive deallocation in the event of a full (of idle
    ;; values) pool.
    (define lease-disp (lease-disposable pool))
    (if sync-release? lease-disp (disposable/async-dealloc lease-disp)))
  (disposable-apply lease-disposable*
                    (pool-disposable produce release max max-idle)))

;; Virtual access to disposables

(define (acquire-virtual disp)
  (define thd-hash (make-weak-hash))
  (thunk (hash-ref! thd-hash (current-thread) (thunk (acquire disp)))))
