#lang racket/base

(module+ test
  (require (for-syntax racket/base)
           doc-coverage
           disposable
           disposable/file
           disposable/example
           disposable/testing
           disposable/unsafe
           racket/function
           racket/stxparam
           rackunit
           syntax/parse/define)

  (define (make-foo-disp+log) (disposable/event-log (disposable-pure 'foo)))
  (define-syntax-parameter foo-disp #'#f)
  (define-syntax-parameter foo-log #'#f)

  (define-simple-macro (with-foo-disp body:expr ...+)
    (let-values ([(disp log) (make-foo-disp+log)])
      (syntax-parameterize ([foo-disp (make-rename-transformer #'disp)]
                            [foo-log (make-rename-transformer #'log)])
        body ...)))

  (test-case "acquire!"
    (with-foo-disp
      (define-values (v dispose!) (acquire! foo-disp))
      (check-equal? v 'foo)
      (check-equal? (foo-log) '((alloc foo)))
      (dispose!)
      (check-equal? (foo-log) '((alloc foo) (dealloc foo)))))

  (test-case "acquire"
    (define (make-evt+trigger)
      (define sema (make-semaphore))
      (values sema (thunk (semaphore-post sema) (sleep 0.1))))
    (define-values (evt trigger) (make-evt+trigger))
    (with-foo-disp
      (define foo-disp/resume foo-disp)
      (check-equal? (acquire foo-disp/resume #:dispose-when evt) 'foo)
      (check-equal? (foo-log) '((alloc foo)))
      (trigger)
      (check-equal? (foo-log) '((alloc foo) (dealloc foo)))))

  (test-case "acquire-global"
    (with-foo-disp
      (define plumber (make-plumber))
      (check-equal? (acquire-global foo-disp #:plumber plumber) 'foo)
      (check-equal? (foo-log) '((alloc foo)))
      (plumber-flush-all plumber)
      (check-equal? (foo-log) '((alloc foo) (dealloc foo)))))

  (test-case "documentation coverage of public modules"
    (check-all-documented 'disposable)
    (check-all-documented 'disposable/file)
    (check-all-documented 'disposable/example)
    (check-all-documented 'disposable/testing)
    (check-all-documented 'disposable/unsafe)))
