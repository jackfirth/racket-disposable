#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [disposable-file (->* () (#:contents string? #:in-dir (or/c #f path-string?))
                        (disposable/c path?))]
  [disposable-directory (->* () (#:in-dir (or/c #f path-string?))
                             (disposable/c path?))]))

(require disposable
         racket/file
         racket/function)


(define (disposable-file #:contents [contents ""] #:in-dir [dir #f])
  (define (create-file)
    (define path (make-temporary-file "rkttmp~a" #f dir))
    (display-to-file contents path #:mode 'text #:exists 'truncate)
    path)
  (disposable/create+delete create-file delete-file))

(define (disposable-directory #:in-dir [parent-dir #f])
  (define (create-dir) (make-temporary-file "rkttmp~a" 'directory parent-dir))
  (disposable/create+delete create-dir delete-directory))
