#lang scribble/manual
@(require "base.rkt")

@title{Basic Disposable API and Concepts}

A @disposable-tech[#:definition? #t]{disposable} is a producer of values that
allocates @ext-res-tech{external resources} when producing a value and provides
a way to deallocate those resources when a produced value is no longer needed.
A value produced by a disposable is called a @disp-value-tech[#:definition? #t]{
 disposable value}.

Disposables are closely related to @custodian-tech{custodians}, but differ in
what sort of resources they manage and how those resources are reclaimed.
Broadly speaking, resources requiring management can be divided into two
categories:

@itemlist[
 @item{@sys-res-tech[#:definition? #t]{System resources} --- resources acquired
  from a program's execution environment such as memory, file descriptors,
  operating system threads, handles to other local processes, etc.}
 @item{@ext-res-tech[#:definition? #t]{External resources} --- resources managed
  by arbitrary external systems that a program communicates with, such as test
  data in a database, network connections with graceful termination protocols,
  machines used to execute distributed data processing, temporary access
  credentials acquired from a remote key management service, etc.}]

These two kinds of resources have very different implications for programs that
use them. Due to the distributed nature of external resources and the inherent
unreliability of network communication, @bold{it is impossible to guarantee that
a given external resource is released}. However, system resources are very
rarely impossible to release. @custodian-tech{Custodians} are designed to
mangage @sys-res-tech{system resources} and assume reliable reclamation is
possible, placing several restrictions on how programs can use custodians as a
result:

@itemlist[
 @item{It must be possible to @emph{forcibly} reclaim system resources during
  program termination as occurs when @racket[custodian-shutdown-all] is called.
  This prevents reclaiming resources by communicating over a network because
  it's  impossible to guarantee successful distributed communication.}
 @item{Placing a user-defined resource under the management of a custodian
  requires using the @racketmodname[ffi/unsafe/custodian] module. This is an
  unsafe API because custodian shutdown actions are executed in
  @atomic-mode-tech{atomic mode}, resulting in possible deadlocks if shutdown
  actions perform asynchronous IO.}
 @item{Custodian shutdown callbacks normally should not strongly reference the
  values they're meant to clean up, as these shutdown callbacks frequently
  double as @finalizer-tech{finalizers} that run when the garbage collector
  determines the value to finalize is no longer reachable. This defeats certain
  composition patterns for shutdown callbacks, particularly composition that
  uses closures to reference both composed callbacks and managed values.}]

These restrictions make managing @ext-res-tech{external resources} with
custodians inappropriate. Instead, an external resource should be produced by a
@disposable-tech{disposable} resulting in a @disp-value-tech{disposable value}
that can be safely managed in a distributed system.

All of the bindings documented in this section are provided by @racketmodname[
 disposable] with the exception of @racket[acquire!], which is provided by
@racketmodname[disposable/unsafe].

@local-table-of-contents[]

@section{Data Definition}

Concretely, a @disposable-tech{disposable} is implemented as a thunk that when
called allocates a new @disp-value-tech{disposable value} and returns that value
alongside another thunk that deallocates the @ext-res-tech{external resources}
associated with that particular disposable value.

@defproc[(disposable [alloc (-> any/c)] [dealloc (-> any/c void?)])
         disposable?]{
 Returns a @disposable-tech{disposable} that allocates a @disp-value-tech{
  disposable value} by calling @racket[alloc] and deallocates @ext-res-tech{
  external resources} associated with that value by calling @racket[dealloc] on
 the allocated value. Both procedures are called with @break-tech{breaks}
 disabled. For a more flexible but less convenient interface, see @racket[
 make-disposable].}

@defproc[(make-disposable [proc (-> (values any/c (-> void?)))]) disposable?]{
 Returns a @disposable-tech{disposable} that is implemented with @racket[proc].
 The given @racket[proc] should return two values: a newly allocated value for
 use by consumers of the disposable, and a thunk that can be used to deallocate
 any @ext-res-tech{external resources} created during allocation of the value.
 Both @racket[proc] and the disposal thunk it returns are called with breaks
 disabled. For the common case where deallocation can be implemented with a
 function that takes the allocated value as input, prefer the @racket[
 disposable] procedure.}

@defproc[(disposable? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a @disposable-tech{disposable}, returns
 @racket[#f] otherwise.}

@defproc[(disposable/c [c contract?]) contract?]{
 Returns a contract for a @disposable-tech{disposable} that allocates values
 matching @racket[c].}

@section{Consuming Disposable Values}

@disp-value-tech{Disposable values} can be acquired in a variety of ways,
ranging from unsafe low level access with @racket[acquire!] to automated
per-thread allocation with @racket[acquire-virtual].

@defform[(with-disposable ([id disp-expr] ...) body ...+)
         #:contracts ([disp-expr disposable?])]{
 Allocates a @disp-value-tech{disposable value} with each @racket[disp-expr] and
 binds each value to its corresponding @racket[id] within the scope of the
 @racket[body] expressions. The values are deallocated upon exiting the @racket[
 body] expressions, even if exiting occurs with an @exn-tech{exception} or a
 @cont-tech{continuation jump}. Additionally, the @racket[body] expressions are
 called with a @cont-barrier-tech{continuation barrier} to prevent jumping back
 into the expressions after deallocation. Deallocation of the @racket[id] values
 occurs concurrently; deallocation of one value does not block on deallocation
 of any other values.

 @(disposable-examples
   (with-disposable ([x example-disposable]
                     [y example-disposable])
     (- x y))
   (eval:error
    (with-disposable ([n example-disposable])
      (error "uh oh!"))))}

@defproc[(call/disposable [disp disposable?] [proc (-> any/c any)]) any]{
 Allocates a @disp-value-tech{disposable value} with @racket[disp], calls
 @racket[proc] with the value as an argument, deallocates the value, then
 returns the result of calling @racket[proc]. This is essentially a non-macro
 form of @racket[with-disposable]. Like @racket[with-disposable], the value is
 deallocated if control leaves @racket[proc] due to an @exn-tech{exception} or a
 @cont-tech{continuation jump}, and a @cont-barrier-tech{continuation barrier}
 prevents jumping back into @racket[proc]. To use with multiple disposables that
 are deallocated concurrently, use @racket[disposable-apply] with @racket[list]
 to transform multiple disposables into a single disposable that allocates a
 list of values.

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

@section{Asynchronous Cleanup}

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
