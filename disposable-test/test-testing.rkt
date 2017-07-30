#lang racket/base

(module+ test
  (require disposable
           disposable/testing
           rackunit)
  
  (test-case "disposable/sequence and disposable/event-log"
    (define-values (abc-disp elog)
      (disposable/event-log (sequence->disposable (list 'a 'b 'c))))
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
