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

  (test-case "with-disposable"
    (with-foo-disp
      (with-disposable ([v foo-disp])
        (check-equal? v 'foo)
        (check-equal? (foo-log) '((alloc foo))))
      (check-equal? (foo-log) '((alloc foo) (dealloc foo)))))

  (test-case "call/disposable"
    (with-foo-disp
      (define (check-call/disposable v)
        (check-equal? v 'foo)
        (check-equal? (foo-log) '((alloc foo))))
      (call/disposable foo-disp check-call/disposable)
      (check-equal? (foo-log) '((alloc foo) (dealloc foo)))))

  (test-case "acquire-virtual"
    (define-values (seq-disp seq-log)
      (disposable/event-log (sequence->disposable '(1 2 3))))
    (define get-virtual (acquire-virtual seq-disp))

    ;; In order to test acquire-virtual, we must call the thunk it returns in
    ;; different threads and ensure we get differnet values. We also have to
    ;; test that within the same thread, we get the same value. And finally, we
    ;; have to test that when a thread dies its corresponding virtual value is
    ;; deallocated. This utility function spawns threads that obsere the virtual
    ;; value for the purposes of testing, as well as giving the caller a thunk
    ;; that kills the observer thread.
    (define (spawn-observation-thread expected)
      (define observation-box (box #f))
      (define box-sema (make-semaphore))
      (define thread-death-sema (make-semaphore))
      (define thd
        (thread
         (thunk
          ;; This failure has a horrible stack trace but I don't know how to fix
          ;; that without removing it entirely.
          (check-equal? (get-virtual) (get-virtual))
          (set-box! observation-box (get-virtual))
          (semaphore-post box-sema)
          (sync thread-death-sema))))
      (sync box-sema)
      (check-equal? (unbox observation-box) expected)
      (thunk (semaphore-post thread-death-sema)
             (sync thd)
             (sleep 0.1)))

    (define kill1 (spawn-observation-thread 1))
    (check-equal? (seq-log) '((alloc 1)))
    (kill1)
    (check-equal? (seq-log) '((alloc 1) (dealloc 1)))
    (define kill2 (spawn-observation-thread 2))
    (define kill3 (spawn-observation-thread 3))
    (kill3)
    (kill2)
    (define expected-final-log
      '((alloc 1) (dealloc 1) (alloc 2) (alloc 3) (dealloc 3) (dealloc 2)))
    (check-equal? (seq-log) expected-final-log))
  
  (test-case "documentation coverage of public modules"
    (check-all-documented 'disposable)
    (check-all-documented 'disposable/file)
    (check-all-documented 'disposable/example)
    (check-all-documented 'disposable/testing)
    (check-all-documented 'disposable/unsafe)))
