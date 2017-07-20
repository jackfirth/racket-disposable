#lang racket/base

(provide (for-label (all-from-out disposable
                                  disposable/example
                                  disposable/file
                                  disposable/testing
                                  disposable/unsafe
                                  racket/base
                                  racket/contract
                                  racket/file))
         disposable-examples
         source-code-link)

(require (for-label disposable
                    disposable/example
                    disposable/file
                    disposable/testing
                    disposable/unsafe
                    racket/base
                    racket/contract
                    racket/file)
         scribble/example
         scribble/manual
         scribble/text
         syntax/parse/define
         "util.rkt")


(define (source-code-link github-str)
  (begin/text "Source code for this library is avaible "
              (hyperlink github-str "on Github")))

(define (make-disposable-eval)
  (make-base-eval #:lang 'racket/base
                  '(require disposable
                            disposable/example
                            disposable/file
                            disposable/testing
                            disposable/unsafe
                            racket/file)))

(define-simple-macro (disposable-examples example:expr ...)
  (examples #:eval (make-disposable-eval) example ...))

(define-tech-helpers
  disposable-tech "disposable"
  virtual-tech "virtual instance"
  pool-tech "pool")
