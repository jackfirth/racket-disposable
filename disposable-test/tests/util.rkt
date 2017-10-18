#lang racket/base

(provide disposable/block-dealloc
         foo/log
         foo-disp
         foo-evts)

(require disposable
         disposable/testing
         disposable/unsafe
         fixture
         racket/function
         racket/list)


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
(define (foo-disp) (first (current-foo/log)))
(define (foo-evts) (event-log-events (second (current-foo/log))))
