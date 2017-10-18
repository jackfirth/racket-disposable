#lang racket/base

(module+ test
  (require disposable
           disposable/testing
           fixture
           racket/list
           rackunit)

  (define-fixture abc-disp+log
    (disposable/event-log (sequence->disposable (list 'a 'b 'c))))

  (define (abc-disp) (first (current-abc-disp+log)))
  (define (events) (event-log-events (second (current-abc-disp+log))))

  (test-case/fixture "disposable/sequence and disposable/event-log"
    #:fixture abc-disp+log
    (check-equal? (events) (list))
    (with-disposable ([v (abc-disp)])
      (check-equal? v 'a)
      (check-equal? (events) '((alloc a))))
    (check-equal? (events) '((alloc a) (dealloc a)))
    (with-disposable ([v1 (abc-disp)])
      (with-disposable ([v2 (abc-disp)])
        (check-equal? v1 'b)
        (check-equal? v2 'c)
        (check-equal? (events) '((alloc a) (dealloc a) (alloc b) (alloc c))))
      (define final-expected-elog
        '((alloc a) (dealloc a) (alloc b) (alloc c) (dealloc c)))
      (check-equal? (events) final-expected-elog))))
