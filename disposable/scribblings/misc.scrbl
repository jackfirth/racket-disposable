#lang scribble/manual
@(require "base.rkt")

@title{Batteries-Included Disposables}

The @racketmodname[disposable] library includes a few extra modules that provide
specialized @disposable-tech{disposables} for various purposes.

@local-table-of-contents[]

@section{Filesystem Disposables}
@defmodule[disposable/file #:packages ("disposable")]

@defproc[(disposable-file [#:contents contents string? ""]
                          [#:parent-dir parent-dir path-string?
                           (find-system-dir 'temp-dir)])
         (disposable/c path-string?)]{
 Returns a @disposable-tech{disposable} that allocates a temporary file in
 @racket[parent-dir] containing @racket[contents] and deletes the file upon
 deallocation. If the file is deleted before disposal occurs, or the same file
 is disposed twice, no error occurs but an event is logged to
 @racket[disposable-file-logger].

 @(disposable-examples
   (with-disposable ([tmpfile (disposable-file #:contents "foo")])
     (printf "Created temporary file ~a\n" tmpfile)
     (printf "Contents = ~a\n" (file->string tmpfile))))}

@defthing[disposable-file-logger logger?]{
 A @logger-tech{logger} used by @racket[disposable-file] whenever an allocated
 file is already deleted at the time of disposal. Events are logged at the
 @racket['info] level with the topic @racket['disposable-file].}

@defproc[(disposable-directory [#:parent-dir parent-dir path-string?
                                (find-system-dir 'temp-dir)])
         (disposable/c path-string?)]{
 Retuns a @disposable-tech{disposable} that allocates a temporary directory in
 @racket[parent-dir] and deletes the directory upon deallocation. If the
 directory is deleted before disposal occurs, or the same directory is disposed
 twice, no error occurs but an event is logged to
 @racket[disposable-directory-logger].

 @(disposable-examples
   (with-disposable ([tmpdir (disposable-directory)])
     (with-disposable ([child (disposable-file #:parent-dir tmpdir)])
       (printf "File = ~a\n" child)
       (printf "Directory children = ~a\n" (directory-list tmpdir)))))}

@defthing[disposable-directory-logger logger?]{
 A @logger-tech{logger} used by @racket[disposable-directory] whenever an
 allocated directory is already deleted at the time of disposal. Events are
 logged at the @racket['info] level with the topic
 @racket['disposable-directory].}

@section{Utilities for Testing Disposables}
@defmodule[disposable/testing #:packages ("disposable")]

This module provides utilities for testing operations involving disposables. The
bindings provided here are designed for testing purposes only, and are not
intended for use in production environments.

@defproc[(sequence->disposable [seq sequence?]) disposable?]{
 Starts traversing @racket[seq] with @racket[sequence-generate], then returns a
 disposable that allocates values by returning the next value from the traversal
 of @racket[seq]. Deallocation does nothing. This is intended for testing code
 that manipulates a disposable.

 @(disposable-examples
   (define abc-disp (sequence->disposable '(a b c)))
   (with-disposable ([item abc-disp])
     (printf "Acquired ~v\n" item))
   (with-disposable ([item abc-disp])
     (printf "Acquired ~v\n" item)))}

@defproc[(disposable/event-log [disp disposable?])
         (disposable/c (list/c disposable? event-log?))]{

 Returns a disposable that allocates an
 @event-log-tech[#:definition? #t]{event log} and a wrapper around @racket[disp]
 that records allocations and deallocations in the allocated event log. An event
 log contains a list of events, where each event is a list whose first item is
 either @racket['alloc] or @racket['dealloc] and whose second item is the
 allocated or deallocated value.

 @(disposable-examples
   (define ex/log (disposable/event-log example-disposable))
   (with-disposable ([ex+log-list ex/log])
     (define ex (first ex+log-list))
     (define elog (second ex+log-list))
     (with-disposable ([n ex])
       (printf "Acquired ~v\n" n))
     (with-disposable ([n ex])
       (printf "Acquired ~v\n" n))
     (event-log-events elog)))}

@defproc[(event-log? [v any/c]) boolean?]{
 Predicate identifying @event-log-tech{event log} values. See
 @racket[disposable/event-log].}

@defproc[(event-log-events [elog event-log?])
         (listof (list/c (or/c 'alloc 'dealloc) any/c))]{
 Returns a list of events that are currently in @racket[elog]. Events are
 returned in order of oldest to newest. See @racket[disposable/event-log].}

@section{Example Disposables}
@defmodule[disposable/example #:packages ("disposable")]

Resource allocation can be tricky to showcase in documentation, so the
@racketmodname[disposable/example] module provides a simple
@racket[example-disposable] that "allocates" a random number while printing a
message to @racket[current-output-port] during allocation and deallocation. This
is used to help users visualize the timeline of disposable values when reading
documentation. It is not considered a stable part of @racketmodname[disposable],
and is only documented to provide hyperlinks at use sites.

@defthing[example-disposable (disposable/c (integer-in 0 99))]{
 A @disposable-tech{disposable} used in documentation that displays a message
 when called for allocation and deallocation. Returns a small random number when
 asked to allocate a value.}
