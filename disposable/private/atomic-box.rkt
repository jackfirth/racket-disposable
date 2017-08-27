#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [atomic-box (-> any/c atomic-box?)]
  [atomic-box? predicate/c]
  [call/atomic-box (->* (atomic-box? (-> any/c (values any/c any/c)))
                        (#:handle-closed (or/c (-> any) #f 'default))
                        any/c)]
  [atomic-box-close (->* (atomic-box?) (#:on-close (-> any/c any/c)) void?)]
  [atomic-box-ref (->* (atomic-box?)
                       (#:handle-closed (or/c (-> any) #f 'default))
                       any/c)]
  [atomic-box-update! (->* (atomic-box? (-> any/c any/c))
                           (#:return (-> any/c any/c)
                            #:handle-closed (or/c (-> any) #f 'default))
                           any/c)]))

(require racket/function
         racket/match)

;; An atomic box contains a value that can have an update function applied to it
;; atomically in a separate kill-safe manager thread. The update function may
;; optionally return a value to the box client who requested the update. This
;; allows a kill-safe thread-safe mutable data structure to be constructed from
;; an ordinary immutable data structure.

(struct atomic-box (thread)
  #:constructor-name make-atomic-box #:omit-define-syntaxes)

(define (call+send v f ch)
  (define-values (new-v return) (f v))
  (channel-put ch return)
  new-v)

(define (loop v)
  (match (thread-receive)
    [(? procedure? f) (f v)]
    [(list f ch) (loop (call+send v f ch))]))

(define (atomic-box v)
  (make-atomic-box (thread (thunk (loop v)))))

(define (atomic-box-resume b)
  (thread-resume (atomic-box-thread b) (current-thread)))

(define (call/atomic-box b f #:handle-closed [handle 'default])
  (atomic-box-resume b)
  (define ch (make-channel))
  (if (equal? handle 'default)
      (thread-send (atomic-box-thread b) (list f ch))
      (thread-send (atomic-box-thread b) (list f ch) handle))
  (channel-get ch))

(define (atomic-box-close b #:on-close [on-close void])
  (atomic-box-resume b)
  (define th (atomic-box-thread b))
  (when (thread-send th on-close #f)
    (sync (thread-dead-evt th))
    (void)))

(define (atomic-box-ref b #:handle-closed [handle 'default])
  (call/atomic-box b (λ (v) (values v v)) #:handle-closed handle))

(define (atomic-box-update! b f
                            #:return [return void]
                            #:handle-closed [handle 'default])
  (define (call v)
    (define fv (f v))
    (values fv (return fv)))
  (call/atomic-box b call #:handle-closed handle))
