#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [disposable/sequence (-> sequence? disposable?)]
  [disposable/event-log
   (-> disposable? (values disposable? (-> (listof disposable-event/c))))]))

(require "private/manage.rkt"
         disposable
         disposable/unsafe
         racket/function)

(module+ test
  (require rackunit))


(define disposable-event/c (list/c (or/c 'alloc 'dealloc) any/c))

(define (box-transform! b f) (set-box! b (f (unbox b))))
(define (snoc v vs) (append vs (list v)))
(define (box-snoc! b v) (box-transform! b (λ (vs) (snoc v vs))))

(define (disposable/sequence seq)
  (define mgr (make-manager))
  (define-values (_ next!) (sequence-generate seq))
  (define (next-managed!) (call/manager mgr next!))
  (disposable next-managed! void))

(define (disposable/event-log disp)
  (define mgr (make-manager))
  (define elog (box (list)))
  (define (log-event! type v)
    (call/manager mgr (thunk (box-snoc! elog (list type v)))))
  (define disp/log
    (make-disposable
     (λ ()
       (define-values (v dispose!) (acquire! disp))
       (log-event! 'alloc v)
       (values v (thunk (log-event! 'dealloc v) (dispose!))))))
  (values disp/log (thunk (unbox elog))))

(module+ test
  (test-case "disposable/sequence and disposable/event-log"
    (define-values (abc-disp elog)
      (disposable/event-log (disposable/sequence (list 'a 'b 'c))))
    (check-equal? (elog) (list))
    (with-disposable ([v abc-disp])
      (check-equal? v 'a)
      (check-equal? (elog) '((alloc a))))
    (check-equal? (elog) '((alloc a) (dealloc a)))
    (with-disposable ([v1 abc-disp])
      (with-disposable ([v2 abc-disp])
        (check-equal? v1 'b)
        (check-equal? v2 'c)
        (check-equal? (elog) '((alloc a) (dealloc a) (alloc b) (alloc c))))
      (check-equal? (elog)
                    '((alloc a) (dealloc a) (alloc b) (alloc c) (dealloc c))))))
