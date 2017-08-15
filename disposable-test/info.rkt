#lang info
(define collection "disposable")
(define version "0.1")
(define deps
  '("base"
    ("disposable" #:version "0.2")))
(define build-deps
  '("doc-coverage"
    "fixture"
    "rackunit-lib"))
(define cover-omit-paths
  '("example.rkt")) ;; already tested by docs
(define implies
  '("disposable"))
