#lang racket/base

(provide (for-label (all-from-out disposable
                                  disposable/example
                                  disposable/file
                                  disposable/testing
                                  disposable/unsafe
                                  racket/base
                                  racket/contract
                                  racket/dict
                                  racket/file
                                  racket/list))
         disposable-examples)

(require (for-label disposable
                    disposable/example
                    disposable/file
                    disposable/testing
                    disposable/unsafe
                    racket/base
                    racket/contract
                    racket/dict
                    racket/file
                    racket/list)
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
                            racket/file
                            racket/list)))

(define-syntax-parse-rule (disposable-examples example:expr ...)
  (examples #:eval (make-disposable-eval) example ...))

(define-tech-helpers
  disposable-tech "disposable"
  disp-value-tech "disposable value"
  ext-res-tech "external resource"
  sys-res-tech "system resource"
  virtual-tech "virtual instance"
  pool-tech "pool"
  transient-tech "transient"
  logger-tech "logger" scribblings/reference/reference
  event-log-tech "event log"
  cont-tech "continuation" scribblings/guide/guide
  cont-barrier-tech "continuation barrier" scribblings/reference/reference
  exn-tech "exception" scribblings/guide/guide
  plumber-tech "plumber" scribblings/reference/reference
  flush-cb-tech "flush callbacks" scribblings/reference/reference
  custodian-tech "custodian" scribblings/reference/reference
  current-cust-tech "current custodian" scribblings/reference/reference
  sync-ready-tech "ready for synchronization" scribblings/reference/reference
  break-tech "break" scribblings/reference/reference
  atomic-mode-tech "atomic mode" scribblings/foreign/foreign
  finalizer-tech "finalizers" scribblings/reference/reference
  port-tech "port" scribblings/guide/guide)
