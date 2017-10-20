#lang scribble/manual
@(require "base.rkt")

@title{Basic Disposable API}

Conceptually, a @disposable-tech[#:definition? #t]{disposable} is a producer of
values that allocates external resources and provides a way to deallocate those
resources. Concretely, a disposable is implemented as a thunk that when called
allocates a new value and returns it paired with a thunk that deallocates the
value. Disposables can be accessed in low-level ways and high-level ways, see
@secref{consume-disp} for details.

All of the bindings documented in this section are provided by @racketmodname[
 disposable] with the exception of @racket[acquire!], which is provided by
@racketmodname[disposable/unsafe].

@local-table-of-contents[]

@section{Data Definition}

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

@defform[(with-disposable ([id disp-expr] ...) body ...+)
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
