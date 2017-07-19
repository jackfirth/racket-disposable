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
  [disposable-bind (->* ((unconstrained-domain-> disposable?))
                        #:rest (listof disposable?)
                        disposable?)]
  [disposable-pool (->* (disposable?)
                        (#:max (or/c exact-nonnegative-integer? +inf.0)
                         #:max-idle (or/c exact-nonnegative-integer? +inf.0))
                        (disposable/c disposable?))]
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
         syntax/parse/define
         "pool.rkt")

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
  (begin0 (f v) (dispose!)))

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
  (check-exn #rx"with-disposable: expected more terms"
             (thunk
              (convert-compile-time-error
               (with-disposable ([a 1]))))))

;; Safe monadic compositional interface

(define (acquire/list! disp) (call-with-values (disposable-proc disp) list))
(define (acquire-all! disps) (map acquire/list! disps))

(define (disposable-pure v) (make-disposable (thunk (values v void))))

(define (disposable-apply f . disps)
  (make-disposable
   (thunk
    (define v+dispose!-pairs (acquire-all! disps))
    (values (apply f (map first v+dispose!-pairs))
            (thunk
             (for ([dispose! (in-list (map second v+dispose!-pairs))])
               (dispose!)))))))

(define (disposable-bind f . disps)
  (define list-disp (apply disposable-apply list disps))
  (make-disposable
   (thunk
    (define-values (vs vs-dispose!) (acquire! list-disp))
    (define-values (f-v f-dispose!) (acquire! (apply f vs)))
    (define (dispose-all!) (f-dispose!) (vs-dispose!))
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

;; Pooled disposables

(define (pool-disposable produce release max max-idle)
  (define (create) (make-pool produce release max max-idle))
  (disposable* create pool-clear))

(define (lease-disposable pool)
  (disposable* (thunk (pool-lease pool))
               (λ (l) (pool-return pool l))))

(define (lease-disposable* pool)
  (disposable-apply lease-get (lease-disposable pool)))

(define (disposable-pool item-disp #:max [max +inf.0] #:max-idle [max-idle 10])
  (define (produce) (acquire/list! item-disp))
  (define (release v-dispose-pair) ((second v-dispose-pair)))
  (define pool (pool-disposable produce release max max-idle))
  (define (lease-item-disposable pool)
    (disposable-apply first (lease-disposable* pool)))
  (disposable-apply lease-item-disposable pool))

;; Virtual access to disposables

(define (acquire-virtual disp)
  (define thd-hash (make-weak-hash))
  (thunk (hash-ref! thd-hash (current-thread) (thunk (acquire disp)))))
