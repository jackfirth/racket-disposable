#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [sequence->disposable (-> sequence? disposable?)]
  [disposable/event-log
   (-> disposable?
       (disposable/c (list/c disposable? (-> (listof disposable-event/c)))))]))

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

(define (sequence->disposable seq)
  (define mgr (make-manager))
  (define-values (_ next!) (sequence-generate seq))
  (define (next-managed!) (call/manager mgr next!))
  (disposable next-managed! void))

(struct event-log (mgr evts))
(define (make-event-log) (event-log (make-manager) (box (list))))
(define (event-log-events elog) (unbox (event-log-evts elog)))

(define (kill-event-log! elog) (manager-kill (event-log-mgr elog)))

(define (log-event! elog type v)
  (define (append-event!) (box-snoc! (event-log-evts elog) (list type v)))
  (call/manager (event-log-mgr elog) append-event!))

(define (wrap-disposable/event-log disp elog)
  (make-disposable
   (thunk
     (define-values (v dispose!) (acquire! disp))
     (log-event! elog 'alloc v)
     (values v (thunk (log-event! elog 'dealloc v) (dispose!))))))

(define (disposable/event-log disp)
  (make-disposable
   (thunk
    (define elog (make-event-log))
    (define disp/log (wrap-disposable/event-log disp elog))
    (define (get-events) (event-log-events elog))
    (define (kill) (kill-event-log! elog))
    (values (list disp/log get-events) kill))))
