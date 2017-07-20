#lang racket/base

(module+ test
  (require doc-coverage
           disposable
           disposable/file
           disposable/example
           disposable/testing
           disposable/unsafe)
  (check-all-documented 'disposable)
  (check-all-documented 'disposable/file)
  (check-all-documented 'disposable/example)
  (check-all-documented 'disposable/testing)
  (check-all-documented 'disposable/unsafe))
