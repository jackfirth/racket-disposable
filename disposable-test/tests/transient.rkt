#lang racket/base

(require disposable
         fixture/rackunit
         racket/contract/region
         racket/function
         rackunit
         "util.rkt")


(test-case/fixture "transient-values"
  #:fixture foo/log

  (test-case "disposable-transient"
    (with-foo-transient _
      (check-equal? (foo-evts) '((alloc foo))))
    (check-equal? (foo-evts) '((alloc foo) (dealloc foo))))

  (test-case "transient-dispose"
    (with-foo-transient t
      (transient-dispose t)
      (check-equal? (foo-evts) '((alloc foo) (dealloc foo))))
    (check-equal? (foo-evts) '((alloc foo) (dealloc foo))))

  (test-case "transient-get"
    (with-foo-transient t
      (check-equal? (transient-get t) 'foo)
      (transient-dispose t)
      (check-false (transient-get t))))

  (test-case "transient-acquire"
    (with-foo-transient t
      (check-equal? (transient-acquire t) 'foo)
      (transient-dispose t)
      (check-equal? (transient-acquire t) 'foo)
      (check-equal? (foo-evts) '((alloc foo) (dealloc foo) (alloc foo)))))
  
  (test-case "transient-refresh"
    (with-foo-transient t
      (check-equal? (transient-refresh t) 'foo)
      (check-equal? (foo-evts) '((alloc foo) (dealloc foo) (alloc foo)))))

  (test-case "transient/c"
    (test-case "pass"
      (with-foo-transient t
        (define t/good (invariant-assertion (transient/c symbol?) t))
        (check-not-exn (thunk (transient-refresh t/good)))))
    
    (test-case "fail"
      (with-foo-transient t
        (define t/bad (invariant-assertion (transient/c string?) t))
        (check-exn exn:fail:contract? (thunk (transient-refresh t/bad)))))))
