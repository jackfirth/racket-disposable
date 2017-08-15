#lang racket/base

(module+ test
  (require (for-syntax racket/base)
           doc-coverage
           disposable
           disposable/example
           disposable/file
           disposable/testing
           disposable/unsafe
           fixture
           racket/control
           racket/list
           racket/function
           racket/stxparam
           rackunit
           syntax/macro-testing
           syntax/parse/define)

  ;; Utility to control the timing of deallocation of a disposable. Allows the
  ;; caller to decide when deallocation starts as well as ensure deallocation
  ;; finishes.
  (define (disposable/block-dealloc disp)
    (define block-sema (make-semaphore))
    (define wait-sema (make-semaphore))
    (values (make-disposable
             (thunk
              (define-values (v dispose!) (acquire! disp))
              (define (dispose/block!)
                (sync block-sema)
                (dispose!)
                (semaphore-post wait-sema))
              (values v dispose/block!)))
            (thunk (semaphore-post block-sema) (sync wait-sema))))

  ;; Utilities to set up a standard disposable for testing that returns foo and
  ;; has a log.

  (define-fixture foo/log (disposable/event-log (disposable-pure 'foo)))
  (define (foo-disp) (first (foo/log)))
  (define (foo-evts) (event-log-events (second (foo/log))))

  (test-case/fixture "base tests"
    #:fixture foo/log

    (test-case "acquire!"
      (define-values (v dispose!) (acquire! (foo-disp)))
      (check-equal? v 'foo)
      (check-equal? (foo-evts) '((alloc foo)))
      (dispose!)
      (check-equal? (foo-evts) '((alloc foo) (dealloc foo))))

    (define-fixture trigger-evt (disposable make-semaphore void))
    (define (trigger!) (semaphore-post (trigger-evt)) (sleep 0.1))

    (test-case/fixture "acquire"
      #:fixture trigger-evt
      (check-equal? (acquire (foo-disp) #:dispose-evt (trigger-evt)) 'foo)
      (check-equal? (foo-evts) '((alloc foo)))
      (trigger!)
      (check-equal? (foo-evts) '((alloc foo) (dealloc foo))))

    (test-case "acquire-global"
      (define plumber (make-plumber))
      (parameterize ([current-plumber plumber])
        (check-equal? (acquire-global (foo-disp)) 'foo))
      (check-equal? (foo-evts) '((alloc foo)))
      (plumber-flush-all plumber)
      (check-equal? (foo-evts) '((alloc foo) (dealloc foo))))

    (test-case "with-disposable"
      (with-disposable ([v (foo-disp)])
        (check-equal? v 'foo)
        (check-equal? (foo-evts) '((alloc foo))))
      (check-equal? (foo-evts) '((alloc foo) (dealloc foo))))

    (test-case "call/disposable"
      (define (check-call/disposable v)
        (check-equal? v 'foo)
        (check-equal? (foo-evts) '((alloc foo))))
      (call/disposable (foo-disp) check-call/disposable)
      (check-equal? (foo-evts) '((alloc foo) (dealloc foo))))

    (test-case "call/disposable error"
      (check-exn values (thunk (call/disposable (foo-disp) raise)))
      (check-equal? (foo-evts) '((alloc foo) (dealloc foo))))

    ;; Continuation utilities
    
    (define (store-cc! a-box)
      (call-with-composable-continuation
       (λ (k) (set-box! a-box k))))
    
    (define (call/capture proc)
      ;; Yes, this is magic
      (define k (box #f))
      (call/prompt (thunk (proc (thunk (store-cc! k)))))
      (thunk ((unbox k) (void))))

    (test-case "call/disposable continuation barrier"
      (define (capture-in-disposable!)
        (call/capture
         (λ (capture!)
           (with-disposable ([_ (foo-disp)])
             (capture!)))))

      ;; Attempting to re-enter a continuation captured inside of
      ;; "with-disposable" should never succeed, because the disposable value
      ;; used has already been deallocated. A new value could be allocated, but
      ;; expressions evaluated before the captured continuation but still inside
      ;; with-disposable will reference the old value that existed at the time
      ;; of capture. Creating a new value upon re-entry would result in the pre-
      ;; capture expressions referencing a different allocated value than the
      ;; post-capture expressions. Thus, a continuation barrier is required.
      (check-exn exn:fail:contract:continuation? capture-in-disposable!)
      (check-equal? (foo-evts) '((alloc foo) (dealloc foo))))

    (define-fixture seq-disp/log
      (disposable/event-log (sequence->disposable '(1 2 3))))
    (define (seq-disp) (first (seq-disp/log)))
    (define (seq-evts) (event-log-events (second (seq-disp/log))))

    (test-case/fixture "acquire-virtual"
      #:fixture seq-disp/log
      (define get-virtual (acquire-virtual (seq-disp)))

      ;; In order to test acquire-virtual, we must call the thunk it returns in
      ;; different threads and ensure we get differnet values. We also have to
      ;; test that within the same thread, we get the same value. And finally,
      ;; we have to test that when a thread dies its corresponding virtual value
      ;; is deallocated. This utility function spawns threads that observe the
      ;; virtual value for the purposes of testing, as well as giving the caller
      ;; a thunk that kills the observer thread.
      (define (spawn-observation-thread expected)
        (define observation-box (box #f))
        (define box-sema (make-semaphore))
        (define thread-death-sema (make-semaphore))
        (define thd
          (thread
           (thunk
            ;; This failure has a horrible stack trace but I don't know how to
            ;; fix that without removing it entirely.
            (check-equal? (get-virtual) (get-virtual))
            (set-box! observation-box (get-virtual))
            (semaphore-post box-sema)
            (sync thread-death-sema))))
        (sync box-sema)
        (check-equal? (unbox observation-box) expected)
        (thunk (semaphore-post thread-death-sema)
               (sync thd)
               ;; This extra sleep ensures the background thread kicked off by
               ;; acquire-virtual has time to dispose of the virtual instance
               ;; used by the observer thread.
               (sleep 0.1)))
      
      (define kill1 (spawn-observation-thread 1))
      (check-equal? (seq-evts) '((alloc 1)))
      (kill1)
      (check-equal? (seq-evts) '((alloc 1) (dealloc 1)))
      (define kill2 (spawn-observation-thread 2))
      (define kill3 (spawn-observation-thread 3))
      (kill3)
      (kill2)
      (define expected-final-log
        '((alloc 1) (dealloc 1) (alloc 2) (alloc 3) (dealloc 3) (dealloc 2)))
      (check-equal? (seq-evts) expected-final-log))

    (test-case "disposable-pool"
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

    (test-case "disposable-pool async release"
      (define-values (foo/block unblock-foo)
        (disposable/block-dealloc (foo-disp)))
      (define pool (disposable-pool foo/block #:max-idle 0))
      (with-disposable ([lease-disp pool])
        (call/disposable lease-disp void)
        (check-equal? (foo-evts) '((alloc foo)))
        (unblock-foo)
        (check-equal? (foo-evts) '((alloc foo) (dealloc foo)))))

    (test-case "disposable-pool release after pool deallocated"
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

    (test-case "disposable-pool leasing when full blocks"
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
                      '((alloc foo) (dealloc foo) (alloc foo) (dealloc foo)))))

    (test-case "disposable/async-dealloc"
      (define-values (foo/block unblock-foo)
        (disposable/block-dealloc (foo-disp)))
      (define foo/async (disposable/async-dealloc foo/block))
      (with-disposable ([v foo/async]) (check-equal? v 'foo))
      (check-equal? (foo-evts) '((alloc foo)))
      (unblock-foo)
      (check-equal? (foo-evts) '((alloc foo) (dealloc foo))))

    (test-case "disposable/chain"
      (define (pair-disp item-disp)
        (disposable-chain item-disp (λ (v) (disposable-pure (list v v)))))
      (define foo-pair (pair-disp (foo-disp)))
      (with-disposable ([v+v foo-pair])
        (check-equal? v+v '(foo foo)))
      (check-equal? (foo-evts) '((alloc foo) (dealloc foo)))))

  (test-case "documentation coverage of public modules"
    (check-all-documented 'disposable)
    (check-all-documented 'disposable/file)
    (check-all-documented 'disposable/example)
    (check-all-documented 'disposable/testing)
    (check-all-documented 'disposable/unsafe))

  (test-case "with-disposable syntax"
    (check-exn #rx"with-disposable: duplicate identifiers not allowed"
               (thunk
                (convert-compile-time-error
                 (with-disposable ([a 1] [a 2])
                   (void)))))
    (check-exn #rx"with-disposable"
               (thunk
                (convert-compile-time-error
                 (with-disposable ([a 1])))))))
