#lang scribble/manual
@(require "base.rkt")

@title{Disposable Values}
@defmodule[disposable #:packages ("disposable")]
@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]]

This library defines @disposable-tech{disposables}, composable first-class
representations of values with external resources that must be allocated and
deallocated. Several safe abstractions are provided for accessing values while
ensuring their associated resources are deallocated. Additionally, disposables
are monadically composable.

@source-code-link{https://github.com/jackfirth/racket-disposable}

@section{Data Model}

Conceptually, a @disposable-tech[#:definition? #t]{disposable} is a producer of
values that allocates external resources and provides a way to deallocate those
resources. Concretely, a disposable is implemented as a thunk that when called
allocates a new value and returns it paired with a thunk that deallocates the
value. Disposables can be accessed in low-level ways and high-level ways, see
@secref{consume-disp} for details.

@defproc[(disposable [alloc (-> any/c)] [dealloc (-> any/c void?)])
         disposable?]{
 Returns a @disposable-tech{disposable} that allocates values by calling
 @racket[alloc] and deallocates values by calling @racket[dealloc] on the
 allocated values. For a more flexible but complex interface, see
 @racket[make-disposable].}

@defproc[(make-disposable [proc (-> (values any/c (-> void?)))]) disposable?]{
 Returns a @disposable-tech{disposable} that is implemented with @racket[proc].
 See @racket[with-disposable] and @racket[acquire!] for details about how
 @racket[proc] is called. For the common case where deallocation can be
 implemented with a function that takes the allocated value as input, see
 @racket[disposable] for a simpler interface.}

@defproc[(disposable? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a @disposable-tech{disposable}, returns
 @racket[#f] otherwise.}

@defproc[(disposable/c [c contract?]) contract?]{
 Returns a contract for a @disposable-tech{disposable} that allocates values
 matching @racket[c].}

@section[#:tag "consume-disp"]{Consuming Disposable Values}

Disposable values can be consumed in a variety of ways, ranging from unsafe low
level access with @racket[acquire!] to automated per-thread allocation with
@racket[acquire-virtual].

@defform[(with-disposable ([id:id disp-expr] ...) body ...+)
         #:contracts ([disp-expr disposable?])]{
 Allocates a value with each @racket[disp-expr] and binds it to the
 corresponding @racket[id] in the @racket[body] expressions. The values are
 deallocated after evaluating the @racket[body] expressions, or if control
 leaves the @racket[body] expressions due to an exception or a continuation
 jump. The @racket[body] expressions are not @emph{reentrant safe}, that is if
 control jumps out of @racket[body ...] and then jumps back into
 @racket[body ...] (e.g. due to a continuation), the allocated value is not
 reallocated. This is intended to be used when a disposable value is needed only
 for a series of expressions, such as when using a temporary file for scratch
 space.

 @(disposable-examples
   (with-disposable ([x example-disposable]
                     [y example-disposable])
     (- x y))
   (eval:error
    (with-disposable ([n example-disposable])
      (error "uh oh!"))))}

@defproc[(call/disposable [disp disposable?] [proc (-> any/c any)]) any]{
 Allocates a value with @racket[disp], passes that value as input to
 @racket[proc], then deallocates the value. Returns the result of calling
 @racket[proc] with the allocated value. The dynamic version of
 @racket[with-disposable]. Like @racket[with-disposable], the value is
 deallocated if control leaves @racket[proc] due to an exception or a
 continuation jump.

 @(disposable-examples
   (call/disposable example-disposable (λ (n) (* n n)))
   (eval:error (call/disposable example-disposable (λ (_) (error "uh oh!")))))}

@defproc[(acquire [disp disposable?]
                  [#:dispose-when evt evt? (thread-dead-evt (current-thread))])
         any/c]{
 Returns a newly-allocated value with @racket[disp] and launches a background
 thread that deallocates the value when @racket[evt] is ready for
 synchronization. The default for @racket[evt] causes the value to be
 deallocated when the calling thread dies. This is for when a disposable value
 is inherently tied to the lifteime of the thread using it, such as a connection
 used while handling a web server request in a servlet model where a new thread
 is spawned for each request. Other uses include @racket[alarm-evt] to return a
 value that is deallocated after a timeout, or using a @racket[subprocess?]
 value to deallocate after a subprocess terminates.

 @(disposable-examples
   (sync
    (thread
     (λ ()
       (define n (acquire example-disposable))
       (printf "Acquired ~v\n" n)))))}

@defproc[(acquire-global [disp disposable?]
                         [#:plumber plumber plumber? (current-plumber)])
         any/c]{
 Returns a newly-allocated value with @racket[disp] and attaches a flush
 callback to @racket[plumber] that deallocates the value. This is intended for
 when a disposable value is to be used throughout the lifetime of a program,
 such as a global database connection pool. Additionally, globally-allocated
 values can be safely provided by modules for use in other modules just like
 normal values, allowing the safe modular definition of global program
 resources.

 @(disposable-examples
   (define plumb (make-plumber))
   (define n (acquire-global example-disposable #:plumber plumb))
   (add1 n)
   (plumber-flush-all plumb))}

@defproc[(acquire-virtual [disp disposable?]) (-> any/c)]{
 Returns a thunk that returns a
 @virtual-tech[#:definition? #t]{virtual instance} of @racket[disp], meaning an
 instance is allocated per-thread the first time a thread calls the thunk and
 returned in subsequent calls by the same thread. The returned thunk maintains a
 weak mapping of threads to allocated instances of @racket[disp], with instances
 deallocated whenever their associated threads die (in the same manner as
 @racket[acquire-thread]). This may be expensive in high-concurrency scenarios
 with short lived threads. To use with high concurrency, consider combining with
 @racket[disposable-pool] to reuse instances between threads.

 @(disposable-examples
   (define virtual-example (acquire-virtual example-disposable))
   (define (spawn)
     (thread (λ () (printf "Acquired ~v\n" (virtual-example)))))
   (sync (spawn) (spawn) (spawn)))}

@section{Unsafe Allocation of Disposables}
@defmodule[disposable/unsafe #:packages ("disposable")]

The @racketmodname[disposable/unsafe] module provides a single export,
@racket[acquire!], which provides an unsafe building block upon which safe
allocation abstractions can be built.

@defproc[(acquire! [disp disposable?]) (values any/c (-> void?))]{
 Returns a newly-allocated value with @racket[disp] as well as a thunk that
 deallocates the value when called. This is @emph{unsafe}, as the caller is
 responsible for ensuring that the deallocation thunk is called.

 @(disposable-examples
   (define-values (n dispose!) (acquire! example-disposable))
   (printf "Acquired ~v unsafely\n" n)
   (dispose!))}

@section{Monadic Composition of Disposables}

@defproc[(disposable-pure [v any/c]) disposable?]{
 Returns a pure @disposable-tech{disposable}. The disposable always returns
 @racket[v] when asked to allocate a value and takes no action upon
 deallocation. Useful with @racket[disposable-apply] for combining a mix of
 disposable and plain values.

 @(disposable-examples
   (with-disposable ([n (disposable-pure 42)])
     n))}

@defproc[(disposable-apply [f procedure?] [disp disposable?] ...) disposable?]{
 Returns a @disposable-tech{disposable} value that allocates a value from each
 @racket[disp], calls @racket[f] with the allocated values, then returns the
 result of calling @racket[f] as the allocated value. Deallocation of the value
 is performed by deallocating each of the source values produced.

 @(disposable-examples
   (struct posn (x y) #:transparent)
   (define disposable-posn
     (disposable-apply posn example-disposable example-disposable))
   (with-disposable ([p disposable-posn])
     (printf "Acquired ~v\n" p)))}

@defproc[(disposable-bind [f (->* () #:rest list? disposable?)]
                          [disp disposable?] ...)
         disposable?]{
 Returns a @disposable-tech{disposable} that allocates a value from each
 @racket[disp], calls @racket[f] with the allocated value producing a new
 disposable, then returns a newly allocated value with the disposable returned
 by @racket[f]. Deallocation of the value is performed by first deallocating
 using the disposable returned by @racket[f], then deallocating each of the
 source values produced. Note that the disposable returned by @racket[f] is
 @emph{not} responsible for deallocating the values used by @racket[f] to
 construct it.

 @(disposable-examples
   (define (construct x y)
     (printf "Constructing disposable with ~a and ~a\n" x y)
     (disposable-apply + example-disposable
                       (disposable-pure x)
                       (disposable-pure y)))
   (define bound
     (disposable-bind construct example-disposable example-disposable))
   (with-disposable ([v bound])
     (printf "Acquired ~v\n" v)))}

@section{Reusing Disposables with Pools}

Disposable values are often expensive to allocate and deallocate. Certain access
and isolation patterns such as the per-thread instances created by
@racket[acquire-virtual] result in a large amount of allocation and deallocation
of short-lived instances of disposables. To help support these use patterns, the
@racketmodname[disposable] library provides
@pool-tech[#:definition? #t]{disposable pools}. Pools encapsulate a collection
of instances of a disposable that are @emph{leased} out when allocated and
returned to the pool after use without being deallocated. Because this can
result in permanently storing idle values in the pool without deallocating them,
the pool @emph{itself} is a disposable which allocates a nested disposable. The
nested disposable allocated by the pool leases values from the pool when
allocating. Various parameters are provided for tweaking the configuration of
the pool's size and tolerance of unused values.

@defproc[(disposable-pool
          [disp disposable?]
          [#:max max (or/c exact-nonnegative-integer? +inf.0) +inf.0]
          [#:max-unused max-unused (or/c exact-nonnegative-integer? +inf.0) 10])
         (disposable/c disposable?)]{
 Returns a @disposable-tech{disposable} that allocates @pool-tech{pools} of
 values using @racket[disp]. The returned disposable allocates a new pool and
 returns a @emph{lease} disposable that leases values from the pool. Upon
 deallocation, the lease disposable returns the values to the pool as
 @emph{unused values}. Unused values are reused for future leases. Returning a
 leased value to the pool will deallocate the value instead if the number of
 unused values is greater than @racket[max-unused]. Leasing a new value from the
 pool will raise an error if no values are available and more than @racket[max]
 values are already in the pool. Future versions of this library may block until
 a value is available instead of raising an error. When the pool disposable is
 deallocated, all values in the pool are deallocated and removed from the pool.

 @(disposable-examples
   (define ex-pool (disposable-pool example-disposable))
   (with-disposable ([ex ex-pool])
     (displayln "Pool initialized")
     (with-disposable ([n ex])
       (printf "Acquired ~v from the pool\n" n))
     (with-disposable ([x ex] [y ex])
       (printf "Acquired ~v and ~v from the pool\n" x y))
     (displayln "Pool shutdown commencing")))}

@section{Filesystem Disposables}
@defmodule[disposable/file #:packages ("disposable")]

@defproc[(disposable-file [#:contents contents string? ""]
                          [#:parent-dir parent-dir path-string?
                           (find-system-dir 'temp-dir)])
         (disposable/c path-string?)]{
 Returns a @disposable-tech{disposable} that allocates a temporary file in
 @racket[parent-dir] containing @racket[contents] and deletes the file upon
 deallocation.

 @(disposable-examples
   (with-disposable ([tmpfile (disposable-file #:contents "foo")])
     (printf "Created temporary file ~a\n" tmpfile)
     (printf "Contents = ~a\n" (file->string tmpfile))))}

@defproc[(disposable-directory [#:parent-dir parent-dir path-string?
                                (find-system-dir 'temp-dir)])
         (disposable/c path-string?)]{
 Retuns a @disposable-tech{disposable} that allocates a temporary directory in
 @racket[parent-dir] and deletes the directory upon deallocation.}

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
         (values disposable?
                 (-> (listof (list (or/c 'alloc 'dealloc) any/c))))]{
 Creates an @emph{event log} that records allocations and dealloctions made with
 @racket[disp], then returns two values:

 @itemlist[
 @item{A wrapped version of @racket[disp] that adds allocation and deallocation
   events to the log. Each event is a list whose first element describes whether
   it was an allocation or a dealloction and whose second element is the value
   allocated or deallocated by the disposable.}
 @item{A thunk that, when called, returns a list of all events currently in the
   event log. The events are ordered from oldest to newest.}]

 @(disposable-examples
   (define-values (ex/log get-log)
     (disposable/event-log example-disposable))
   (with-disposable ([n ex/log])
     (printf "Acquired ~v\n" n))
   (with-disposable ([n ex/log])
     (printf "Acquired ~v\n" n))
   (get-log))}

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
