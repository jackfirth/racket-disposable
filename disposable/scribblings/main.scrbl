#lang scribble/manual
@(require "base.rkt")

@(define github-url "https://github.com/jackfirth/racket-disposable")
@(define license-url
   "https://github.com/jackfirth/racket-disposable/blob/master/LICENSE")

@title{Disposable Values}
@defmodule[disposable #:packages ("disposable")]
@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]]

This library defines @disposable-tech{disposables}, composable first-class
representations of values with external resources that must be allocated and
deallocated. Several safe abstractions are provided for accessing values while
ensuring their associated resources are deallocated. Disposables are monadically
composable and several combinators exist for automatic resouce pooling,
asynchronous concurrent allocation and deallocation, and debugging or runtime
introspection.

@(racketblock
  (define (connect!) (make-connection ...))
  (define (disconnect! conn) (close-connection conn ...))
  (define disposable-connection (disposable connect! disconnect!))

  (with-disposable ([conn disposable-connection])
    ... use conn ...))

Source code for this library is avaible @hyperlink[github-url]{on Github} and is
provided under the terms of the @hyperlink[license-url]{Apache License 2.0}.

@bold{Warning!} This library is @emph{experimental}; it may change in backwards
incompatible ways without notice. As such, now is the best time for feedback and
suggestions so feel free to open a repository issue or reach out to me directly.

This package provides several modules, all in the @racketmodname[disposable]
collection:

@itemlist[
 @item{@racketmodname[disposable] - A safe high-level interface to disposable
  values, along with combinators for extending and composing them.}
 @item{@racketmodname[disposable/unsafe] - Unsafe low-level interface to
  disposable values.}
 @item{@racketmodname[disposable/file] - Constructors for filesystem related
  disposables.}
 @item{@racketmodname[disposable/testing] - Utilities for testing disposables
  and disposable-related code.}
 @item{@racketmodname[disposable/example] - Utilities for documenting
  disposables and disposable-related code.}]

@local-table-of-contents[]
@include-section["lite.scrbl"]
@include-section["pool.scrbl"]
@include-section["transient.scrbl"]
@include-section["misc.scrbl"]
