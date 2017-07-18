#lang info
(define collection "disposable")
(define scribblings
  '(("scribblings/main.scrbl" () (library) "disposable")))
(define deps
  '("base"
    "reprovide-lang"))
(define build-deps
  '("racket-doc"
    "scribble-lib"
    "scribble-text-lib"))
