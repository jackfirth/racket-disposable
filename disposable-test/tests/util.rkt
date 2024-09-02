#lang racket/base

(provide disposable/block-dealloc
         foo/log
         foo-disp
         foo-evts
         with-foo-transient
         with-fresh-custodian)

(require disposable
         disposable/testing
         disposable/unsafe
         fixture
         racket/function
         racket/list
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
(define (foo-disp) (first (current-foo/log)))
(define (foo-evts) (event-log-events (second (current-foo/log))))

(define-syntax-parse-rule (with-foo-transient id:id body:expr ...)
  (with-disposable ([id (disposable-transient (foo-disp))]) body ...))

(define (call/fresh-custodian proc)
  (define c (make-custodian))
  (begin0
    (parameterize ([current-custodian c]) (proc))
    (custodian-shutdown-all c)))

(define-syntax-parse-rule (with-fresh-custodian body:expr ...)
  (call/fresh-custodian (thunk body ...)))
