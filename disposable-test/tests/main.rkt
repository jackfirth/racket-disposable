#lang racket/base

(require (for-syntax racket/base)
         disposable
         disposable/testing
         disposable/unsafe
         doc-coverage
         fixture
         racket/control
         racket/function
         racket/list
         rackunit
         syntax/macro-testing
         "util.rkt")


(test-case/fixture "consuming-values"
  #:fixture foo/log

  (test-case "acquire!"
    (define-values (v dispose!) (acquire! (foo-disp)))
    (check-equal? v 'foo)
    (check-equal? (foo-evts) '((alloc foo)))
    (dispose!)
    (check-equal? (foo-evts) '((alloc foo) (dealloc foo))))

  (define-fixture trigger-evt (disposable make-semaphore void))
  (define (trigger!) (semaphore-post (current-trigger-evt)) (sleep 0.1))

  (test-case/fixture "acquire"
    #:fixture trigger-evt
    (check-equal? (acquire (foo-disp) #:dispose-evt (current-trigger-evt))
                  'foo)
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

  (test-case "call/capture"
    (define result (box #f))
    (define counter (box 0))
    (define captured
      (call/capture
       (λ (capture!)
         (set-box! result (capture!))
         (set-box! counter (add1 (unbox counter))))))
    (check-equal? (unbox result) (void))
    (check-equal? (unbox counter) 1)
    (captured)
    (check-equal? (unbox result) (void))
    (check-equal? (unbox counter) 2))

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
  (define (seq-disp) (first (current-seq-disp/log)))
  (define (seq-evts) (event-log-events (second (current-seq-disp/log))))

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
    (check-equal? (seq-evts) expected-final-log)))

(test-case/fixture "other"
  #:fixture foo/log
  
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

(test-case "disposable/custodian"
  (define parent-cust (current-custodian))
  (define (managed [c (current-custodian)])
    (custodian-managed-list c parent-cust))
  
  (define (make-cust-resources) (thread (λ () (sync never-evt))) (void))
  (define disp/sys-resource
    (disposable make-cust-resources
                (λ (_) (make-cust-resources))))

  (with-fresh-custodian
    (with-disposable ([_ disp/sys-resource]) (void))
    (check-equal? (length (managed)) 2))

  (define cust (make-custodian))
  (with-fresh-custodian
    (with-disposable ([_ (disposable/custodian disp/sys-resource cust)]) (void))
    (check-equal? (length (managed)) 0)
    (check-equal? (length (managed cust)) 2))
  (custodian-shutdown-all cust))

(test-case/fixture "disposable/memoize"
  #:fixture foo/log
  (define (foo-allocs n) (make-list n '(alloc foo)))
  (define (foo-deallocs n) (make-list n '(dealloc foo)))
  (define (func arg [opt-arg #f] #:kw kw-arg #:opt-kw [opt-kw-arg #f])
    (foo-disp))
  (with-disposable ([func/memo (disposable/memoize func)])
    (check-equal? (foo-evts) '())
    (check-equal? (func/memo 1 #:kw 2) 'foo)
    (check-equal? (foo-evts) (foo-allocs 1))
    (check-equal? (func/memo 1 #:kw 2) 'foo)
    (check-equal? (foo-evts) (foo-allocs 1))
    (check-equal? (func/memo 2 #:kw 2) 'foo)
    (check-equal? (foo-evts) (foo-allocs 2))
    (check-equal? (func/memo 2 #:kw 2) 'foo)
    (check-equal? (foo-evts) (foo-allocs 2))
    (check-equal? (func/memo 1 #f #:kw 2 #:opt-kw #f) 'foo)
    (check-equal? (foo-evts) (foo-allocs 3))
    (check-equal? (func/memo 1 #t #:kw 2) 'foo)
    (check-equal? (foo-evts) (foo-allocs 4))
    (check-equal? (func/memo 1 #:kw 2 #:opt-kw #t) 'foo)
    (check-equal? (foo-evts) (foo-allocs 5)))
  (check-equal? (foo-evts) (append (foo-allocs 5) (foo-deallocs 5))))

(test-case "documentation coverage of public modules"
  (local-require disposable/file disposable/example)
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
               (with-disposable ([a 1]))))))
