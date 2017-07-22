#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [disposable-file (->* ()
                        (#:contents string? #:parent-dir (or/c #f path-string?))
                        (disposable/c path?))]
  [disposable-directory (->* () (#:parent-dir (or/c #f path-string?))
                             (disposable/c path?))]))

(require disposable
         disposable/unsafe
         racket/file
         racket/function)

(module+ test
  (require rackunit))


(define-logger disposable-file)
(define-logger disposable-directory)

(define (disposable-file #:contents [contents ""] #:parent-dir [parent-dir #f])
  (define (create)
    (define path (make-temporary-file "rkttmp~a" #f parent-dir))
    (display-to-file contents path #:mode 'text #:exists 'truncate)
    path)
  (define (delete file)
    (define (exn:no-such-file? exn)
      (and (exn:fail:filesystem:errno? exn)
           (member (exn:fail:filesystem:errno-errno exn)
                   '((2 . posix) (2 . windows)))
           #t))
    (define (log-no-such-file _)
      (define msg-format "attempted to delete nonexistent file: ~a")
      (log-disposable-file-info msg-format file))
    (with-handlers ([exn:no-such-file? log-no-such-file]) (delete-file file)))
  (disposable create delete))

(module+ test
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
    (define-values (v dispose!) (acquire! (disposable-file)))
    (dispose!)
    (check-not-exn dispose!)))
    
(define (disposable-directory #:parent-dir [parent-dir #f])
  (define (create-dir) (make-temporary-file "rkttmp~a" 'directory parent-dir))
  (define (delete dir)
    (define (exn:no-such-dir? exn)
      (and (exn:fail:filesystem:errno? exn)
           (member (exn:fail:filesystem:errno-errno exn)
                   '((2 . posix) (3 . windows)))
           #t))
    (define (log-no-such-dir _)
      (define msg-format "attempted to delete nonexistent directory: ~a")
      (log-disposable-directory-info msg-format dir))
    (with-handlers ([exn:no-such-dir? log-no-such-dir]) (delete-directory dir)))
  (disposable create-dir delete))

(module+ test
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
    (check-not-exn dispose!)))
