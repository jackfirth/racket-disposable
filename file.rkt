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
         racket/file
         racket/function)

(module+ test
  (require rackunit))


(define (disposable-file #:contents [contents ""] #:parent-dir [parent-dir #f])
  (define (create-file)
    (define path (make-temporary-file "rkttmp~a" #f parent-dir))
    (display-to-file contents path #:mode 'text #:exists 'truncate)
    path)
  (disposable create-file delete-file))

(module+ test
  (test-case "disposable-file"
    (define file*
      (with-disposable ([file (disposable-file)])
        (check-not-exn (thunk (file->string file)))
        file))
    (check-exn exn:fail:filesystem? (thunk (file->string file*))))
  (test-case "disposable-file #:contents"
    (with-disposable ([file (disposable-file #:contents "stuff")])
      (check-equal? (file->string file) "stuff"))))
    
(define (disposable-directory #:parent-dir [parent-dir #f])
  (define (create-dir) (make-temporary-file "rkttmp~a" 'directory parent-dir))
  (disposable create-dir delete-directory))

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
      (check-equal? (directory-list dir) '()))))
