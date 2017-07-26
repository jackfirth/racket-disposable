#lang info
(define collection "disposable")
(define version "0.1")
(define deps
  '("base"))
(define build-deps
  '("doc-coverage"
    "rackunit-lib"
    "disposable"))
(define cover-omit-paths
  '("example.rkt")) ;; already tested by docs
