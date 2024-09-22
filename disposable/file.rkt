#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [disposable-file (->* ()
                        (#:contents string? #:parent-dir (or/c #f path-string?))
                        (disposable/c path?))]
  [disposable-file-logger logger?]
  [disposable-directory (->* () (#:parent-dir (or/c #f path-string?))
                             (disposable/c path?))]
  [disposable-directory-logger logger?]))

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

(define (disposable-directory #:parent-dir [parent-dir #f])
  (define (create-dir) (make-temporary-directory "rkttmp~a" #:base-dir parent-dir))
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
