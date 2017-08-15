#lang info
(define collection "disposable")
(define version "0.1")
(define deps
  '("base"))
(define build-deps
  '(("disposable" #:version "0.2")
    "doc-coverage"
    "fixture"
    "rackunit-lib"))
(define cover-omit-paths
  '("example.rkt")) ;; already tested by docs
