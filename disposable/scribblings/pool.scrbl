#lang scribble/manual
@(require "base.rkt")

@title{Reusing Disposables with Pools}

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

All of the bindings documented in this section are provided by
@racketmodname[disposable].

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

 If no idle values are available and more than @racket[max] values are already
 in the pool, attempting to lease a value will block until a pooled value
 becomes available. When the pool disposable is deallocated, all values in the
 pool are deallocated and removed from the pool. Allocation of values by the
 pool is not concurrent; the pool will allocate multiple values serially if
 multiple clients request values concurrently.

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
