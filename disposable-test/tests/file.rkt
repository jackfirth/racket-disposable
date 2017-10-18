#lang racket/base

(module+ test
  (require disposable
           disposable/file
           disposable/unsafe
           racket/file
           racket/function
           rackunit)
  
  (define (double-dispose! disp)
    (define-values (v dispose!) (acquire! disp))
    (dispose!)
    (dispose!)
    v)

  (test-case "disposable-file"
    (define file*
      (with-disposable ([file (disposable-file)])
        (check-not-exn (thunk (file->string file)))
        file))
    (check-exn exn:fail:filesystem? (thunk (file->string file*))))
  (test-case "disposable-file #:contents"
    (with-disposable ([file (disposable-file #:contents "stuff")])
      (check-equal? (file->string file) "stuff")))
  (test-case "disposable-file idempotent"
    (check-not-exn (thunk (double-dispose! (disposable-file)))))
  (test-case "disposable-file logging"
    (define receiver (make-log-receiver disposable-file-logger 'info))
    (define file (double-dispose! (disposable-file)))
    (define received (sync receiver))
    (check-pred vector? received)
    (check-equal? (vector-ref received 0) 'info)
    (define msg
      (format "disposable-file: attempted to delete nonexistent file: ~a"
              (path->string file)))
    (check-equal? (vector-ref received 1) msg)
    (check-pred continuation-mark-set? (vector-ref received 2))
    (check-equal? (vector-ref received 3) 'disposable-file))
  (test-case "disposable-directory"
    (define dir*
      (with-disposable ([dir (disposable-directory)])
        (check-equal? (directory-list dir) '())
        dir))
    (check-exn exn:fail:filesystem? (thunk (directory-list dir*))))
  (test-case "disposable-directory #:parent-dir"
    (with-disposable ([dir (disposable-directory)])
      (with-disposable ([nested-dir (disposable-directory #:parent-dir dir)])
        (check-equal? (directory-list dir #:build? #t) (list nested-dir)))
      (check-equal? (directory-list dir) '())))
  (test-case "disposable-file #:parent-dir"
    (with-disposable ([dir (disposable-directory)])
      (with-disposable ([file (disposable-file #:parent-dir dir)])
        (check-equal? (directory-list dir #:build? #t) (list file)))
      (check-equal? (directory-list dir) '())))
  (test-case "disposable-directory idempotent"
    (define-values (v dispose!) (acquire! (disposable-directory)))
    (dispose!)
    (check-not-exn dispose!))
  (test-case "disposable-directory logging"
    (define receiver (make-log-receiver disposable-directory-logger 'info))
    (define dir (double-dispose! (disposable-directory)))
    (define received (sync receiver))
    (check-pred vector? received)
    (check-equal? (vector-ref received 0) 'info)
    (define msg-format
      "disposable-directory: attempted to delete nonexistent directory: ~a")
    (check-equal? (vector-ref received 1)
                  (format msg-format  (path->string dir)))
    (check-pred continuation-mark-set? (vector-ref received 2))
    (check-equal? (vector-ref received 3) 'disposable-directory)))
