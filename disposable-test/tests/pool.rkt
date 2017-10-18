#lang racket/base

(require disposable
         fixture/rackunit
         racket/function
         rackunit
         "util.rkt")


(test-case/fixture "disposable-pool"
  #:fixture foo/log

  (test-begin
    ;; Release leases synchronously to ensure predictable test results
    (define pool (disposable-pool (foo-disp) #:max-idle 1 #:sync-release? #t))
    (check-equal? (foo-evts) '())
    (with-disposable ([lease-disp pool])
      (check-equal? (foo-evts) '())
      ;; First lease creates a value without deallocating
      (with-disposable ([v lease-disp]) (check-equal? v 'foo))
      (check-equal? (foo-evts) '((alloc foo)))
      ;; Second lease resuses that same value
      (call/disposable lease-disp void)
      (check-equal? (foo-evts) '((alloc foo)))
      (with-disposable ([v1 lease-disp] [v2 lease-disp])
        ;; Leasing more values than there are in the pool creates more values
        (check-equal? (foo-evts) '((alloc foo) (alloc foo))))
      ;; Returning more values than #:max-idle deallocates the extra values
      (check-equal? (foo-evts) '((alloc foo) (alloc foo) (dealloc foo))))
    ;; Deallocating the pool deallocates all values
    (check-equal? (foo-evts)
                  '((alloc foo) (alloc foo) (dealloc foo) (dealloc foo))))

  (test-case "async release"
    (define-values (foo/block unblock-foo)
      (disposable/block-dealloc (foo-disp)))
    (define pool (disposable-pool foo/block #:max-idle 0))
    (with-disposable ([lease-disp pool])
      (call/disposable lease-disp void)
      (check-equal? (foo-evts) '((alloc foo)))
      (unblock-foo)
      (check-equal? (foo-evts) '((alloc foo) (dealloc foo)))))

  (test-case "release after pool deallocated"
    (define foo-pool (disposable-pool (foo-disp) #:sync-release? #t))
    (define sema (make-semaphore))
    (define leased-foo
      (with-disposable ([lease foo-pool])
        (acquire lease #:dispose-evt sema)))
    ;; By now, the pool is deallocated but the lease hasn't been deallocated
    ;; because it's blocked on the semaphore. We should have access to the foo
    ;; value even though its backing disposable has deallocated. Additionally,
    ;; unblocking the lease deallocation should not cause an error because
    ;; attempting to return the leased value to the deallocated pool should
    ;; safely do nothing.
    (check-equal? leased-foo 'foo)
    (check-equal? (foo-evts) '((alloc foo) (dealloc foo)))
    ;; The error here is thrown asynchronously on a background thread so it's
    ;; pretty tricky to catch here. If this test fails, a gross error message
    ;; is printed asynchronously and that's probably the best we can do for
    ;; now.
    (semaphore-post sema))

  (test-case "leasing when full blocks"
    (define foo-pool
      (disposable-pool (foo-disp) #:max 1 #:max-idle 0 #:sync-release? #t))
    (with-disposable ([lease foo-pool])
      (define sema (make-semaphore))
      (define consumed-sema (make-semaphore))
      (define consumer
        (thread
         (thunk
          (with-disposable ([_ lease])
            (semaphore-post consumed-sema)
            (sync sema)))))
      (sync consumed-sema)
      (check-equal? (foo-evts) '((alloc foo)))
      (define blocked (thread (thunk (call/disposable lease void))))
      (sync/timeout 0.1 blocked)
      (check-equal? (foo-evts) '((alloc foo)))
      (check-pred thread-running? blocked)
      (semaphore-post sema)
      (sync consumer)
      (check-not-false (sync/timeout 1 blocked))
      (check-equal? (foo-evts)
                    '((alloc foo) (dealloc foo) (alloc foo) (dealloc foo))))))