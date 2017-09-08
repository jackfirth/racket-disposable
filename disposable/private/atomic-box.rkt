#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [atomic-box (-> any/c atomic-box?)]
  [atomic-box? predicate/c]
  [atomic-box-close! (->* (atomic-box?) (#:on-close (-> any/c void?)) void?)]
  [atomic-box-closed? (-> atomic-box? boolean?)]
  [atomic-box-ref (->* (atomic-box?)
                       (#:handle-closed (or/c (-> any/c) #f))
                       any/c)]
  [atomic-box-set! (->* (atomic-box? any/c)
                        (#:handle-closed (or/c (-> any/c) #f))
                        any/c)]
  [atomic-box-update! (->* (atomic-box? (-> any/c any/c))
                           (#:return (-> any/c any/c)
                            #:handle-closed (or/c (-> any/c) #f))
                           any/c)]
  [call/atomic-box (->* (atomic-box? (-> any/c (values any/c any/c)))
                        (#:handle-closed (or/c (-> any/c) #f))
                        any/c)]))

(require racket/function
         racket/match)

;; An atomic box contains a value that can have an update function applied to it
;; atomically in a separate kill-safe manager thread. The update function may
;; optionally return a value to the box client who requested the update. This
;; allows a kill-safe thread-safe mutable data structure to be constructed from
;; an ordinary immutable data structure.

(struct closed () #:constructor-name make-closed #:omit-define-syntaxes)
(define closed (make-closed))

(struct atomic-box (box thread)
  #:constructor-name make-atomic-box #:omit-define-syntaxes)

(struct write-op (new-v) #:transparent)
(struct close-op (f) #:transparent)
(struct update-op (f resp-channel) #:transparent)
(struct call-op (f resp-channel) #:transparent)

(define (loop b)
  (match (thread-receive)
    [(write-op new-v) (set-box! b new-v) (loop b)]
    [(close-op f) (define v (unbox b)) (set-box! b closed) (f v)]
    [(update-op f ch)
     (define new-v (f (unbox b)))
     (channel-put ch new-v)
     (set-box! b new-v)
     (loop b)]
    [(call-op f ch)
     (define-values (new-v response) (f (unbox b)))
     (channel-put ch response)
     (set-box! b new-v)
     (loop b)]))

(define (atomic-box v)
  (define b (box v))
  (make-atomic-box b (thread (thunk (loop b)))))

(define (atomic-box-resume b)
  (thread-resume (atomic-box-thread b) (current-thread)))

(define (atomic-box-closed? b)
  (thread-dead? (atomic-box-thread b)))

(define (raise-closed-error name b)
  (raise-argument-error name "(not/c atomic-box-closed?)" b))

(define (atomic-box-ref b #:handle-closed [handle #f])
  (atomic-box-resume b)
  (define try-v (unbox (atomic-box-box b)))
  (cond [(not (closed? try-v)) try-v]
        [handle (handle)]
        [else (raise-closed-error 'atomic-box-ref b)]))
  
(define (atomic-box-set! b v #:handle-closed [handle #f])
  (atomic-box-resume b)
  (define handle*
    (or handle (thunk (raise-closed-error 'atomic-box-set! b))))
  (thread-send (atomic-box-thread b) (write-op v) handle*))

(define (atomic-box-close! b #:on-close [on-close void])
  (atomic-box-resume b)
  (thread-send (atomic-box-thread b) (close-op on-close) void)
  (sync (thread-dead-evt (atomic-box-thread b)))
  (void))

(define (atomic-box-update! b f #:return [return void]
                            #:handle-closed [handle #f])
  (atomic-box-resume b)
  (define ch (make-channel))
  (define op (update-op f ch))
  (cond [(thread-send (atomic-box-thread b) op #f)
         (return (channel-get ch))]
        [handle (handle)]
        [else (raise-closed-error 'atomic-box-update! b)]))

(define (call/atomic-box b f #:handle-closed [handle #f])
  (atomic-box-resume b)
  (define ch (make-channel))
  (define op (call-op f ch))
  (cond [(thread-send (atomic-box-thread b) op #f)
         (channel-get ch)]
        [handle (handle)]
        [else (raise-closed-error 'call/atomic-box b)]))

(define b (atomic-box 0))
(define (count!)
  (cond [(atomic-box-update! b add1 #:handle-closed (thunk #f))
         (sleep 1)
         (count!)]
        [else
         (displayln "Counter loop shutting down")]))
(define counter (thread count!))
