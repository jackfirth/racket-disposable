#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [sequence->disposable (-> sequence? disposable?)]
  [disposable/event-log
   (-> disposable? (disposable/c (list/c disposable? event-log?)))]
  [event-log? predicate/c]
  [event-log-events (-> event-log? (listof disp-event?))]))

(define disp-event? (list/c (or/c 'alloc 'dealloc) any/c))

(require disposable
         disposable/private/atomic-box
         disposable/unsafe
         racket/function)

(module+ test
  (require rackunit))


(define disposable-event/c (list/c (or/c 'alloc 'dealloc) any/c))

(define (snoc v vs) (append vs (list v)))

(define (sequence->disposable seq)
  (define-values (_ next!) (sequence-generate seq))
  (define b (atomic-box #f))
  (define (next!/atomic) (call/atomic-box b (λ (v) (values v (next!)))))
  (disposable next!/atomic void))

(struct event-log (box))
(define (make-event-log) (event-log (atomic-box (list))))
(define (event-log-events elog) (atomic-box-ref (event-log-box elog)))

(define (kill-event-log! elog) (atomic-box-close (event-log-box elog)))

(define (log-event! elog type v)
  (define log (list type v))
  (atomic-box-update! (event-log-box elog) (λ (logs) (snoc log logs))))

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
    (define (kill) (kill-event-log! elog))
    (values (list disp/log elog) kill))))
