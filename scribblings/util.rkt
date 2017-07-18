#lang racket/base

(provide define-tech-helpers)

(require scribble/manual
         syntax/parse/define)


(define ((tech-helper key) #:definition? [definition? #f] . pre-flow)
  (apply (if definition? deftech tech) #:key key pre-flow))

(define-simple-macro (define-tech-helpers (~seq id:id key:str) ...)
  (begin (begin (define id (tech-helper key)) (provide id)) ...))
