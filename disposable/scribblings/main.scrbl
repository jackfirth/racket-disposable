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

@section{Overview of Collections and Modules}

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
 allocated values. Both procedures are called with breaks disabled. For a more
 flexible but complex interface, see @racket[make-disposable].}

@defproc[(make-disposable [proc (-> (values any/c (-> void?)))]) disposable?]{
 Returns a @disposable-tech{disposable} that is implemented with @racket[proc].
 The given @racket[proc] should return two values: a newly allocated value for
 use by consumers of the disposable, and a thunk that can be used to deallocate
 any resources created during allocation of the value. Both @racket[proc] and
 the disposal thunk it returns are called with breaks disabled. For the common
 case where deallocation can be implemented with a function that takes the
 allocated value as input, see @racket[disposable] for a simpler interface.}

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
 deallocated upon exiting the @racket[body] expressions, either normally or due
 to an @exn-tech{exception} or a @cont-tech{continuation jump}. Additionally,
 the @racket[body] expressions are called with a @cont-barrier-tech{continuation
  barrier} to prevent jumping back into the expressions after deallocation.
 Deallocation of the @racket[id] values occurs concurrently; deallocation of one
 value does not block on deallocation of any other values.

 @(disposable-examples
   (with-disposable ([x example-disposable]
                     [y example-disposable])
     (- x y))
   (eval:error
    (with-disposable ([n example-disposable])
      (error "uh oh!"))))}

@defproc[(call/disposable [disp disposable?] [proc (-> any/c any)]) any]{
 Allocates a value with @racket[disp], passes that value as input to
 @racket[proc], deallocates the value, then returns the result of calling
 @racket[proc]. The dynamic version of @racket[with-disposable]. Like
 @racket[with-disposable], the value is deallocated if control leaves
 @racket[proc] due to an @exn-tech{exception} or a
 @cont-tech{continuation jump}, and a @cont-barrier-tech{continuation barrier}
 prevents jumping back into @racket[proc]. To use with multiple disposables that
 are deallocated concurrently, use @racket[disposable-apply] with @racket[list]
 to transform the disposables into a single disposable containing a list of
 values.

 @(disposable-examples
   (call/disposable example-disposable (λ (n) (* n n)))
   (eval:error (call/disposable example-disposable (λ (_) (error "uh oh!")))))}

@defproc[(acquire [disp disposable?]
                  [#:dispose-evt evt evt? (thread-dead-evt (current-thread))])
         any/c]{
 Allocates and returns a value with @racket[disp] and launches a background
 thread that deallocates the value when @racket[evt] is @sync-ready-tech{ready
  for synchronization}. Using the default for @racket[evt] causes the value to
 be deallocated when the calling thread dies. This is for when a disposable
 value is inherently tied to the lifteime of the thread using it, such as a
 connection used while handling a web server request in a servlet model where a
 new thread is spawned for each request. Other uses include @racket[alarm-evt]
 to return a value that is deallocated after a timeout, or using a
 @racket[subprocess?] value to deallocate after a subprocess terminates.

 @(disposable-examples
   (sync
    (thread
     (λ ()
       (define n (acquire example-disposable))
       (printf "Acquired ~v\n" n)))))}

@defproc[(acquire-global [disp disposable?]
                         [#:plumber plumber plumber? (current-plumber)])
         any/c]{
 Allocates and returns a value with @racket[disp] and attaches a
 @flush-cb-tech{flush callback} to @racket[plumber] that deallocates the value.
 This is intended for when a disposable value is used throughout the lifetime of
 a program, such as a global database connection pool. Globally allocated values
 can be safely provided by modules for use in other modules just like normal
 values, allowing the modular definition of global program resources. Note that
 @plumber-tech{plumbers} expect that after all flush callbacks return the
 program may exit and kill any remaining threads, so a disposable that is
 acquired globally should not deallocate values in any asynchronous manner (such
 as in a background thread). In particular, do not use @racket[acquire-global]
 with @racket[disposable/async-dealloc].

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
 @racket[acquire]). This may be expensive in high-concurrency scenarios with
 short lived threads. To use with high concurrency, consider combining with
 @racket[disposable-pool] to reuse instances between threads. In particular, see
 @secref{gpvl} for a common and convenient access pattern.

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
 responsible for ensuring that the deallocation thunk is called. Both the
 @racket[acquire!] procedure and the disposal thunk it returns should be called
 with breaks disabled.

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
 is performed by deallocating each of the source values produced. Deallocation
 occurs concurrently.

 @(disposable-examples
   (struct posn (x y) #:transparent)
   (define disposable-posn
     (disposable-apply posn example-disposable example-disposable))
   (with-disposable ([p disposable-posn])
     (printf "Acquired ~v\n" p)))}

@defproc[(disposable-chain [disp disposable?] [f (-> any/c disposable?)])
         disposable?]{
 Returns a @disposable-tech{disposable} that allocates a value from
 @racket[disp], calls @racket[f] with the allocated value producing a new
 disposable, then returns a newly allocated value with the disposable returned
 by @racket[f]. Deallocation of the value is performed by first deallocating
 using the disposable returned by @racket[f], then deallocating the source value
 produced by @racket[disp] after finishing deallocating with the disposable
 produced by @racket[f].

 @(disposable-examples
   (define (add-example x)
     (disposable-apply (λ (y) (+ x y)) example-disposable))
   (define sum-disp (disposable-chain example-disposable add-example))
   (with-disposable ([v sum-disp])
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
          [#:max-idle max-idle (or/c exact-nonnegative-integer? +inf.0) 10]
          [#:sync-release? sync-release? boolean? #f])
         (disposable/c disposable?)]{
 Returns a @disposable-tech{disposable} that allocates @pool-tech{pools} of
 values using @racket[disp]. The returned disposable allocates a new pool and
 returns a @emph{lease} disposable that leases values from the pool. Upon
 deallocation, the lease disposable returns the values to the pool as
 @emph{unused values}. Unused values are reused for future leases. Returning a
 leased value to the pool will deallocate the value instead if the number of
 unused values is greater than @racket[max-idle].

 Leasing a new value from the pool will raise an error if no values are
 available and more than @racket[max] values are already in the pool. When the
 pool disposable is deallocated, all values in the pool are deallocated and
 removed from the pool. Allocation of values by the pool is not concurrent; the
 pool will allocate multiple values serially if multiple clients request values
 concurrently.

 Allocation and deallocation by the pool sets @racket[current-custodian] to the
 custodian that was current when when the @emph{pool} was allocated, not when
 a @emph{lease} for that pool is allocated. As a result, different clients of a
 pool with different custodians may use values from the pool that are not
 managed by their custodian. Because a lease disposable is only obtainable by
 allocating a pool it's expected that the leasing threads have custodians that
 are subordinate to the pool's custodian, ensuring that a custodian shutdown of
 either the pool's custodian or any lease's custodian does not result in a lease
 returning allocated values whose custodian-managed resources (e.g. threads,
 ports, etc.) have already been reclaimed.

 If @racket[sync-release?] if @racket[#f] (the default) leased values are
 returned to the pool asynchronously, preventing the leasing thread from
 potentially blocking on an expensive deallocation. This is not always
 desireable; in testing it's useful to have predictable deallocation, and when
 using @racket[acquire] or @racket[acquire-virtual] where the lease is already
 disposed asynchronously it's unnecessary.

 @(disposable-examples
   (define ex-pool (disposable-pool example-disposable))
   (with-disposable ([ex ex-pool])
     (displayln "Pool initialized")
     (with-disposable ([n ex])
       (printf "Acquired ~v from the pool\n" n))
     (with-disposable ([x ex] [y ex])
       (printf "Acquired ~v and ~v from the pool\n" x y))
     (displayln "Pool shutdown commencing")))}

@section[#:tag "gpvl"]{Global Pools with Virtual Leases}

@virtual-tech{Virtual instances} of disposables are exceptionally convenient,
essentially providing thread-isolated resources "for free". However, for
contexts where short lived threads are created very frequently (such as web
servers), resource allocation and deallocation may be much more expensive than
the cost of the work the thread performs with that resource. By combining
@racket[acquire-virtual] with a @pool-tech{pooled disposable} constructed by
@racket[disposable-pool], we can get the best of both worlds: individual threads
have isolated access to resources, but resources are automatically reused by
threads to minimize expensive allocation and deallocation. Furthermore, by
defining and acquiring the pool globally in a module with
@racket[acquire-global], we can hide the use of disposables from client modules
completely:

@(racketblock
  (define pool-lease
    (acquire-global (disposable-pool example-disposable
                                     #:sync-release? #t)))
  (define get-resource (acquire-virtual pool-lease))
  (provide get-resource))

Clients need only call @racket[(get-resource)] to obtain a thread-specific
allocated value. The pool is completely disposed only when the program is about
to exit (more specifically, when the associated @plumber-tech{plumber} is
flushed). This is called the @emph{global pool with virtual lease} pattern, and
is appropriate for reusable resources accessed in isolation by short-lived tasks
in a long-running program.

@section{Extending Disposables}

@defproc[(disposable/async-dealloc [disp disposable?]) disposable?]{
 Returns a disposable that is like @racket[disp], but deallocation happens
 asynchronously in a background thread spawned when deallocation would normally
 occur. This is intended for disposables where immediately releasing the
 resource is not required. Note that this is not recommended for use with
 @racket[acquire-global], as it's important that the @plumber-tech{plumber} used
 by @racket[acquire-global] does not finish flushing until all resources have
 been deallocated.

 @(disposable-examples
   (with-disposable ([n (disposable/async-dealloc example-disposable)])
     (printf "Acquired ~v\n" n))
   (sleep 0.1))}

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
