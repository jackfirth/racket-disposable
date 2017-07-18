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

A @disposable-tech[#:definition? #t]{disposable} is a producer of values that
allocates external resources and provides a way to deallocate those resources.
Conceptually, a disposable is a thunk that when called allocates a new value
and returns it paired with a thunk that deallocates the value. Disposables can
be accessed in a low-level way and a high-level way, see @secref{consume-disp}
for details.

@defproc[(disposable [alloc (-> (values any/c (-> void?)))]) disposable?]{
 Returns a @disposable-tech{disposable} that is implemented with @racket[alloc].
 See @racket[with-disposable] and @racket[acquire!] for details about how
 @racket[alloc] is called.}

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
 deallocated upon exiting the @racket[body] expressions. This is intended to be
 used when a disposable value is needed only for a series of expressions, such
 as when using a temporary file for scratch space.

 @(disposable-examples
   (with-disposable ([x example-disposable]
                     [y example-disposable])
     (- x y)))}

@defproc[(call/disposable [disp disposable?] [proc (-> any/c any)]) any]{
 Allocates a value with @racket[disp], passes that value as input to
 @racket[proc], then deallocates the value. Returns the result of calling
 @racket[proc] with the allocated value. The dynamic version of
 @racket[with-disposable].

 @(disposable-examples
   (call/disposable example-disposable (λ (n) (* n n))))}

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

@defproc[(acquire-thread [disp disposable?]) any/c]{
 Returns a newly-allocated value with @racket[disp] and launches a background
 thread that deallocates the value when the calling thread dies. This is
 for when a disposable value is inherently tied to the lifteime of the thread
 using it, such as a connection used while handling a web server request in a
 servlet model where a new thread is spawned for each request. The background
 thread only weakly holds a reference to the calling thread, allowing the
 calling thread to be garbage collected before the value is disposed.

 @(disposable-examples
   (sync
    (thread
     (λ ()
       (define n (acquire-thread example-disposable))
       (printf "Acquired ~v\n" n)))))}

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
