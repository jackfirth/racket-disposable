#lang scribble/manual
@(require "base.rkt")

@title{Transient Values}

A @transient-tech[#:definition? #t]{transient} value is a wrapper around a value
allocated by a @disposable-tech{disposable} that allows users of the value to
have some limited control over its lifecycle. In particular, users of a
transient may:

@itemlist[
 @item{Eagerly dispose the value with @racket[transient-dispose]}
 @item{Replace the value with a @emph{fresh} value allocated from the same
  disposable with @racket[transient-refresh]}
 @item{Directly access the value with @racket[transient-acquire], which may
  allocate a fresh value if the transient's current value has already been
  disposed}
 @item{Access the value only if it hasn't been disposed with @racket[
 transient-get]}]

Any disposable may be converted to a disposable of a transient value by using
@racket[disposable-transient]. Transients are intended for lightweight control
of disposable allocation and deallocation in a way that cooperates with other
disposable wrappers and combinators. For example, combining transients with
@racket[disposable-pool] allows values to be reused @emph{and} allows clients to
explicitly create and destroy pooled values if needed.

Transients are thread safe, but not thread isolated. A transient either contains
one allocated value or contains no values, and multiple threads may call any of
the above functions on transients without violating this consistency. However,
the above functions will block if a new value needs to be allocated or
deallocated. To acquire disposables in a thread isolated manner where each
thread has unfettered access to its own private instance of a disposable, see
@racket[acquire-virtual].

All of the bindings documented in this section are provided by
@racketmodname[disposable].

@section{Definition and Construction}

@defproc[(transient? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a @transient-tech{transient}, returns
 @racket[#f] otherwise.}

@defproc[(transient/c [c contract?]) contract?]{
 Returns a contract that recognizes @transient-tech{transients} that own values
 that satisfy @racket[c]. The returned contract monitors all future values that
 a transient might allocate in addition to checking its current value.}

@defproc[(disposable-transient [disp disposable?]) (disposable/c transient?)]{
 Returns a @disposable-tech{disposable} that wraps values allocated by
 @racket[disp] in a @transient-tech{transient}. Disposal of the transient
 disposes its underlying value using @racket[transient-dispose].

 @(disposable-examples
   (with-disposable ([t (disposable-transient example-disposable)])
     (printf "Acquired transient: ~a\n" (transient-acquire t))
     (transient-refresh t)
     (printf "Refreshed transient: ~a\n" (transient-acquire t))))}

@section{Manipulating Transients}

@defproc[(transient-dispose [t transient?]) void?]{
 Disposes the currently allocated value of @racket[t] if one exists. A value
 owned by a @transient-tech{transient} will never be deallocated twice ---
 repeated calls to @racket[transient-dispose] on the same transient have no
 effect. The @disposable-tech{disposable} returend by @racket[
 disposable-transient] calls @racket[transient-dispose] upon deallocation.

 After disposal of the value owned by @racket[t], calling @racket[
 transient-acquire] will block on allocating a fresh value while @racket[
 transient-get] will return @racket[#f].

 @(disposable-examples
   (with-disposable ([t (disposable-transient example-disposable)])
     (transient-dispose t)))}

@defproc[(transient-acquire [t transient?]) any/c]{
 Returns the allocated value of @racket[t]. If @racket[t] does not currently own
 an allocated value (due to a previous use of @racket[transient-dispose]) a
 fresh value is created inside @racket[t] using the @disposable-tech{disposable}
 that @racket[t] was constructed with. See also @racket[transient-get] which
 returns @racket[#f] instead of allocating a new value.

 @(disposable-examples
   (with-disposable ([t (disposable-transient example-disposable)])
     (transient-dispose t)
     (printf "Acquired transient: ~a\n" (transient-acquire t))))}

@defproc[(transient-get [t transient?]) any/c]{
 Returns the allocated value of @racket[t] or @racket[#f] if @racket[t] does not
 currently own an allocated value (due to a previous use of @racket[
 transient-dispose]). In general, prefer using @racket[transient-acquire] unless
 there is a specific reason to avoid allocating a fresh value.

 @(disposable-examples
   (with-disposable ([t (disposable-transient example-disposable)])
     (transient-dispose t)
     (printf "Transient value: ~a\n" (transient-get t))))}

@defproc[(transient-refresh [t transient?]) any/c]{
 Disposes the allocated value owned by @racket[t] if one exists, then allocates
 a fresh value inside @racket[t] and returns that value. Equivalent to calling
 @racket[(transient-dispose t)] followed by @racket[(transient-acquire t)].

 @(disposable-examples
   (with-disposable ([t (disposable-transient example-disposable)])
     (printf "Refreshed value: ~a\n" (transient-refresh t))))}
