#lang racket/base

(module+ test
  (require doc-coverage
           disposable
           disposable/file
           disposable/example
           disposable/testing
           disposable/unsafe
           rackunit)

  (test-case "acquire!"
    (define-values (disp get-log)
      (disposable/event-log (disposable-pure 'foo)))
    (define-values (v dispose!) (acquire! disp))
    (check-equal? v 'foo)
    (check-equal? (get-log) '((alloc foo)))
    (dispose!)
    (check-equal? (get-log) '((alloc foo) (dealloc foo))))

  (test-case "documentation coverage of public modules"
    (check-all-documented 'disposable)
    (check-all-documented 'disposable/file)
    (check-all-documented 'disposable/example)
    (check-all-documented 'disposable/testing)
    (check-all-documented 'disposable/unsafe)))
