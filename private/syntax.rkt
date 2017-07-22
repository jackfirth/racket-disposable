#lang racket/base

(provide bindings)

(require syntax/parse)


(define-syntax-class bindings
  (pattern ([id:id expr:expr] ...)
           #:fail-when (check-duplicate-identifier (syntax->list #'(id ...)))
           "duplicate identifiers not allowed"))
