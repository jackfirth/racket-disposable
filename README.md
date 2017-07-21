# racket-disposable [![Build Status](https://travis-ci.org/jackfirth/racket-disposable.svg?branch=master)](https://travis-ci.org/jackfirth/racket-disposable) [![codecov](https://codecov.io/gh/jackfirth/racket-disposable/branch/master/graph/badge.svg)](https://codecov.io/gh/jackfirth/racket-disposable) [![Documentation](https://img.shields.io/badge/read-documentation-blue.svg)](http://docs.racket-lang.org/disposable/)
An experimental Racket library providing an abstraction for values associated with external resources that allows automatic resource pooling, per-thread virtual construction, and monadic composition.

```racket
(require disposable)

(define (connect!) (make-connection ...))
(define (disconnect! conn) (close-connection conn ...))
(define disposable-connection (disposable connect! disconnect!))

(with-disposable ([conn disposable-connection])
  ... use conn ...)
```
