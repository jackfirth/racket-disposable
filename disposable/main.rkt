#lang racket/base

;; A disposable value is a value that encompasses external resources that must
;; be released when the value is no longer in use.

(require racket/contract)

(provide
 with-disposable
 (contract-out
  [disposable (-> (-> any/c) (-> any/c void?) disposable?)]
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
  [acquire (->* (disposable?) (#:dispose-evt evt?) any/c)]
  [acquire-global (->* (disposable?) (#:plumber plumber?) any/c)]
  [acquire-virtual (-> disposable? (-> any/c))]
  [transient? predicate/c]
  [transient/c (-> contract? contract?)]
  [disposable-transient (-> disposable? (disposable/c transient?))]
  [transient-dispose (-> transient? void?)]
  [transient-acquire (-> transient? any/c)]
  [transient-get (-> transient? any/c)]
  [transient-refresh (-> transient? any/c)]))

(module+ private-unsafe
  (provide
   (contract-out
    [acquire! (-> disposable? (values any/c (-> void?)))])))

(require (for-syntax racket/base
                     "private/syntax.rkt")
         racket/function
         racket/list
         racket/promise
         syntax/parse/define
         "private/pool.rkt")

(module+ test
  (require rackunit
           syntax/macro-testing))

;; Kernel API module, used to take care of renaming smart constructors and
;; isolating access to the struct type definition.

(module kernel racket/base

  (provide (rename-out [disposable* disposable])
           acquire!
           disposable?
           disposable/c
           make-disposable)

  (require racket/contract/base
           racket/function)

  (struct disposable (proc)
    #:constructor-name make-disposable)

  (define (acquire! disp) ((disposable-proc disp)))

  (define (disposable* alloc dealloc)
    (define (alloc+dealloc)
      (define v (alloc))
      (values v (thunk (dealloc v))))
    (make-disposable alloc+dealloc))

  (define (disposable/c value/c)
    (struct/c disposable (-> (values value/c any/c)))))

(require 'kernel)

;; Safe caller interface

(define (call/disposable disp f)
  (define v-box (box #f))
  (define dispose!-box (box #f))
  (dynamic-wind (thunk
                 (define-values (v dispose!) (acquire! disp))
                 (set-box! v-box v)
                 (set-box! dispose!-box dispose!))
                (thunk
                 (call-with-continuation-barrier (thunk (f (unbox v-box)))))
                (thunk ((unbox dispose!-box)))))

(define-simple-macro (with-disposable bindings:bindings body:expr ...+)
  (call/disposable (disposable-apply list bindings.expr ...)
                   (λ (vs) (apply (λ (bindings.id ...) body ...) vs))))

;; Safe monadic compositional interface

(define (map-async f vs)
  (map force (for/list ([v (in-list vs)]) (delay/thread (f v)))))

(define (acquire/list! disp) (call-with-values (thunk (acquire! disp)) list))

(define (acquire-all! disps)
  (map-async (λ (disp) (parameterize-break #f (acquire/list! disp))) disps))

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

(define (acquire disp #:dispose-evt [evt (current-thread-dead)])
  (parameterize-break #f
    (define-values (v dispose!) (acquire! disp))
    (thread (thunk (parameterize-break #f (sync evt) (dispose!))))
    v))

;; Plumber-enabled globally allocated disposables

(define (acquire-global disp #:plumber [plumber (current-plumber)])
  (parameterize-break #f
    (define-values (v dispose!) (acquire! disp))
    (define (flush! handle)
      (parameterize-break #f
        (dispose!)
        (plumber-flush-handle-remove! handle)))
    (plumber-add-flush! plumber flush!)
    v))

;; Async deallocation

(define (disposable/async-dealloc disp)
  (make-disposable
   (λ ()
     (define-values (v dispose!) (acquire! disp))
     (define (dispose-async!)
       (thread (thunk (parameterize-break #f (dispose!)))))
     (values v dispose-async!))))

;; Pooled disposables

(define (pool-disposable produce release max max-idle)
  (define (create) (make-pool produce release max max-idle))
  (disposable create pool-clear))

(define (lease-disposable v+dispose-pool)
  (define (get-lease-value v+dispose-lease)
    (first (lease-value v+dispose-lease)))
  (disposable-apply get-lease-value
                    (disposable (thunk (pool-lease v+dispose-pool))
                                (λ (l) (pool-return v+dispose-pool l)))))

(define (disposable-pool item-disp
                         #:max [max +inf.0]
                         #:max-idle [max-idle 10]
                         #:sync-release? [sync-release? #f])
  (define (produce) (parameterize-break #f (acquire/list! item-disp)))
  (define (release v-dispose-pair)
    (parameterize-break #f ((second v-dispose-pair))))
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
  (thunk
   (parameterize-break #f
     (hash-ref! thd-hash (current-thread) (thunk (acquire disp))))))

;; Transients

(struct transient (transition-sema box disp))

(define (transient/c c)
  (define box-in-c (box/c (or/c #f (list/c c procedure?))))
  (and/c transient? (struct/c transient semaphore? box-in-c disposable?)))

(define (disposable-transient disp)
  (define (create)
    (define-values (v dispose!) (acquire! disp))
    (transient (make-semaphore 1) (box (list v dispose!)) disp))
  (disposable create transient-dispose))

(define (transient-dispose t)
  (define (call)
    (define current (unbox (transient-box t)))
    (when current
      ((second current))
      (set-box! (transient-box t) #f)))
  (call-with-semaphore (transient-transition-sema t) call)
  (void))

(define (transient-acquire t)
  (define (call)
    (define current (unbox (transient-box t)))
    (cond [current (first current)]
          [else
           (define-values (v dispose!) (acquire! (transient-disp t)))
           (set-box! (transient-box t) (list v dispose!))
           v]))
  (call-with-semaphore (transient-transition-sema t) call))

(define (transient-get t)
  (define current (unbox (transient-box t)))
  (and current (first current)))

(define (transient-refresh t)
  (transient-dispose t)
  (transient-acquire t))
