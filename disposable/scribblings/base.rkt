#lang racket/base

(provide (for-label (all-from-out disposable
                                  disposable/example
                                  disposable/file
                                  disposable/testing
                                  disposable/unsafe
                                  racket/base
                                  racket/contract
                                  racket/file))
         disposable-examples)

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
  pool-tech "pool"
  logger-tech "logger" scribblings/reference/reference)
