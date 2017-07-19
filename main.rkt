#lang racket/base

;; A disposable value is a value that encompasses external resources that must
;; be released when the value is no longer in use.

(require racket/contract)

(provide
 with-disposable
 (contract-out
  [disposable (-> (-> (values any/c (-> void?))) disposable?)]
  [disposable? predicate/c]
  [disposable/c (-> (or/c chaperone-contract? flat-contract?) contract?)]
  [call/disposable (-> disposable? (-> any/c any) any)]
  [disposable-apply (->* (procedure?) #:rest (listof disposable?) disposable?)]
  [disposable-pure (-> any/c disposable?)]
  [disposable-bind (->* ((unconstrained-domain-> disposable?))
                        #:rest (listof disposable?)
                        disposable?)]
  [disposable/create+delete (-> (unconstrained-domain-> any/c)
                                (unconstrained-domain-> void?)
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


;; Kernel API

(struct disposable (proc))

(define (acquire! disp) ((disposable-proc disp)))

;; Contracts

(define (disposable/c value/c)
  (struct/c disposable (-> (values value/c any/c))))

;; Safe caller interface

(define (call/disposable disp f)
  (define-values (v dispose!) (acquire! disp))
  (begin0 (f v) (dispose!)))

(define-simple-macro (with-disposable ([id:id disp:expr] ...) body:expr ...)
  (call/disposable (disposable-apply list disp ...)
                   (λ (vs) (apply (λ (id ...) body ...) vs))))

;; Safe monadic compositional interface

(define (acquire/list! disp) (call-with-values (disposable-proc disp) list))
(define (acquire-all! disps) (map acquire/list! disps))

(define (disposable-pure v) (disposable (thunk (values v void))))

(define (disposable-apply f . disps)
  (disposable
   (thunk
    (define v+dispose!-pairs (acquire-all! disps))
    (values (apply f (map first v+dispose!-pairs))
            (thunk
             (for ([dispose! (in-list (map second v+dispose!-pairs))])
               (dispose!)))))))

(define (disposable-bind f . disps)
  (define list-disp (apply disposable-apply list disps))
  (disposable
   (thunk
    (define-values (vs vs-dispose!) (acquire! list-disp))
    (define-values (f-v f-dispose!) (acquire! (apply f vs)))
    (define (dispose-all!) (f-dispose!) (vs-dispose!))
    (values f-v dispose-all!))))

;; Construction sugar

(define (disposable/create+delete create delete)
  (disposable (thunk (define v (create)) (values v (thunk (delete v))))))

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
  (disposable/create+delete create pool-clear))

(define (lease-disposable pool)
  (disposable/create+delete
   (thunk (pool-lease pool))
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
