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


(define (disposable-file #:contents [contents ""] #:parent-dir [parent-dir #f])
  (define (create-file)
    (define path (make-temporary-file "rkttmp~a" #f parent-dir))
    (display-to-file contents path #:mode 'text #:exists 'truncate)
    path)
  (disposable/create+delete create-file delete-file))

(define (disposable-directory #:parent-dir [parent-dir #f])
  (define (create-dir) (make-temporary-file "rkttmp~a" 'directory parent-dir))
  (disposable/create+delete create-dir delete-directory))
