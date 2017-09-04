#lang info
(define collection "disposable")
(define scribblings
  '(("scribblings/main.scrbl" () ("Resource Management") "disposable")))
(define version "0.2")
(define deps
  '("base"
    "reprovide-lang"))
(define build-deps
  '("rackunit-lib"
    "racket-doc"
    ("scribble-lib" #:version "1.16")
    "scribble-text-lib"))
(define cover-omit-paths
  '("example.rkt")) ;; already tested by docs
