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
                                disposable?)]))

(module+ private-unsafe
  (provide
   (contract-out
    [disposable-alloc! (-> disposable? (values any/c (-> void?)))])))

(require (for-syntax racket/base)
         racket/function
         racket/list
         syntax/parse/define)


;; Kernel API

(struct disposable (proc))

(define (disposable-alloc! disp) ((disposable-proc disp)))

;; Contracts

(define (disposable/c value/c)
  (struct/c disposable (-> (values value/c any/c))))

;; Safe caller interface

(define (call/disposable disp f)
  (define-values (v dispose!) (disposable-alloc! disp))
  (begin0 (f v) (dispose!)))

(define-simple-macro (with-disposable ([id:id disp:expr] ...) body:expr ...)
  (call/disposable (disposable-apply list disp ...)
                   (λ (vs) (apply (λ (id ...) body ...) vs))))

;; Safe monadic compositional interface

(define (disposable-alloc-all! disps)
  (define (disposable-alloc/list! disp)
    (call-with-values (disposable-proc disp) list))
  (map disposable-alloc/list! disps))

(define (disposable-pure v) (disposable (thunk (values v void))))

(define (disposable-apply f . disps)
  (disposable
   (thunk
    (define v+dispose!-pairs (disposable-alloc-all! disps))
    (values (apply f (map first v+dispose!-pairs))
            (thunk
             (for ([dispose! (in-list (map second v+dispose!-pairs))])
               (dispose!)))))))

(define (disposable-bind f . disps)
  (define list-disp (apply disposable-apply list disps))
  (disposable
   (thunk
    (define-values (vs vs-dispose!) (disposable-alloc! list-disp))
    (define-values (f-v f-dispose!) (disposable-alloc! (apply f vs)))
    (define (dispose-all!) (f-dispose!) (vs-dispose!))
    (values f-v dispose-all!))))

;; Construction sugar

(define (disposable/create+delete create delete)
  (disposable (thunk (define v (create)) (values v (thunk (delete v))))))
