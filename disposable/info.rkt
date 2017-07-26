#lang info
(define collection "disposable")
(define scribblings
  '(("scribblings/main.scrbl" () (library) "disposable")))
(define version "0.1")
(define deps
  '("base"
    "reprovide-lang"))
(define build-deps
  '("rackunit-lib"
    "racket-doc"
    "scribble-lib"
    "scribble-text-lib"
    "doc-coverage"))
(define cover-omit-paths
  '("example.rkt")) ;; already tested by docs
