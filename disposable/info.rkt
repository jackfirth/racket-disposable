#lang info
(define collection "disposable")
(define scribblings
  '(("scribblings/main.scrbl" (multi-page) ("Resource Management") "disposable")))
(define version "0.5")
(define deps
  '(("arguments" #:version "1.2.1")
    "base"
    "reprovide-lang"))
(define build-deps
  '("rackunit-lib"
    "racket-doc"
    ("scribble-lib" #:version "1.16")
    "scribble-text-lib"))
(define cover-omit-paths
  '("example.rkt")) ;; already tested by docs
