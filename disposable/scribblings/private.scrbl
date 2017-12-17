#lang scribble/manual
@(require "base.rkt")

@title{Disposable Implementation Details}
@defmodule[disposable/private]

@bold{This module is not for public consumption} --- the bindings documented
here are used internally by the @racketmodname[disposable] library. No attempt
is made to retain any sort of backwards compatibility. Documentation for the
@racketmodname[disposable/private] module is provided solely to make the
implementation of @racketmodname[disposable] easier to understand and navigate.

@local-table-of-contents[]

@section{Synchronizable Events}

This library contains additions and extensions to Racket @sync-evt-tech{
 synchronizable events}, with the goal of making common concurrency patterns
easier to understand, implement, and compose.

@subsection{Terminology}

@subsection{Composition}

@defproc[(pair-evt) evt?]{}

@defproc[(apply-evt [f procedure?] [evt (evt/c any?)] ...) evt?]{
 Returns a @sync-evt-tech{synchronizable event} that calls @racket[f] with the
 @racket[evt] values. The returned event is @sync-ready-tech{ready for
  synchronization} when @emph{all} of the given @racket[evt] values are ready
 for synchronization, and its @sync-result-tech{synchronization result} is the
 result of applying @racket[f] to the list of @racket[evt] synchronization
 results. @break-tech{Breaks} are explicitly disabled during the call to
 @racket[f].

 @bold{The returned event is not atomic.}} @;; TODO: explain more here

@defproc[(commit-evt [evt evt?] [f (unconstrained-domain-> evt?)]) evt?]{
 Returns a @sync-evt-tech{synchronizable event} that is @sync-ready-tech{ready
  for synchronization} when @racket[evt] is ready for synchronization and whose
 @sync-result-tech{synchornization result} is the synchronization result of the
 event returned when @racket[f] is applied to the result of @racket[evt].

 This function may appear similar to @racket[replace-evt], but it behaves closer
 to @racket[wrap-evt]. Specifically, @racket[f] is only called if @racket[evt]
 is @sync-chosen-tech{chosen for synchornization} and the event returned by
 @racket[f] is @emph{always and immediately} chosen for synchronization.

 This is roughly equivalent to @racket[(wrap-evt evt (compose sync f))]. Note
 that @break-tech{breaks} are explicitly disabled while synchronizing on the
 event returned by @racket[f] --- allowing breaks would defeat uses of @racket[
 commit-evt] that clean up transactional shared state. Consider returning an
 event from @racket[f] that periodically checks for @break-tech{breaks} at safe
 points using @racket[break-checking-evt] or one that has a timeout set using
 @racket[timeout-evt].

 Use of @racket[commit-evt] is appropriate when constructing an event that needs
 to perform some sort of @emph{essential} follow-up transaction if and only if
 the event is chosen for synchronization. @bold{This is dangerous for follow-up
  events that wait indefinitely}, because once @racket[evt] is chosen the
 follow-up event @emph{must} be chosen @emph{no matter what}. This means events
 returned by @racket[commit-evt] can't have their follow-up events influenced by
 timeouts or alternative choices, breaking typical assumptions about event
 composition and scheduling. Consider carefully whether @racket[replace-evt] is
 more appropriate before deciding to use @racket[commit-evt].}

@defproc[(seq-evt [evt evt?] ... [#:commit? commit? #f]) evt?]{
 Returns a @sync-evt-tech{synchronizable event} that is @sync-ready-tech{ready
  for synchronization} when each @racket[evt] is ready for synchronization, and
 whose @sync-result-tech{synchronization result} is the synchronization result of
 the final @racket[evt].

 Unlike @racket[apply-evt], a @thread-tech{thread} attempting to @sync-tech{
  synchronize} on the returned event first synchronizes on the @racket[evt]
 arguments @emph{serially} --- each @racket[evt] is not synchronized on until
 the preceeding @racket[evt] is ready for synchronization.

 If @racket[commit?] is true, then no @racket[evt] is @sync-chosen-tech{chosen
  for synchronization} unless the entire sequence is chosen for synchronizatio
 and only the first @racket[evt] argument can be chosen in a @racket[
 choice-evt]. This is equivalent to chaining the events together with @racket[
 commit-evt] and ignoring the synchronization results of every event but the
 last. See @racket[commit-evt] for more details, particularly about the @bold{
  dangers of indefinite transactional cleanup}.

 If @racket[commit?] is false, then any @racket[evt] may be chosen for
 synchronization as long as all preceeding events in the sequence were chosen,
 even if the following event (or the returned sequence event, in the case of the
 last @racket[evt] argument) isn't chosen. This is equivalent to chaining the
 events together with @racket[replace-evt] and ignoring the synchronization
 results of every event but the last.}

@defproc[(share-evt [local-evt evt?]) evt?]{}

@subsection{Polling, Timeouts, and Breaks}

@defproc[(timeout-evt [wait time-period?]) (evt/c #f)]{
 Returns a @sync-evt-tech{synchronizable event} that is actually an
 @sync-maker-tech{event maker}. When a @thread-tech{thread} attempts to
 @sync-tech{synchronize} on the returned event, the current time is noted with
 @racket[(current-inexact-milliseconds)] and the returned event becomes
 @sync-ready-tech{ready for synchronization} after @racket[wait] time has
 passed.

 This is essentially a combination of @racket[alarm-evt] and @racket[guard-evt].
 Events returned by @racket[timeout-evt] are @sync-local-tech{local events} ---
 multiple synchronizing threads on the same timeout event are isolated and
 independent.

 It is intended that @racket[(sync/timeout (->seconds wait #t) evt ...)] is
 always equivalent to @racket[(sync (timeout-evt wait) evt ...)]. A similar
 equivalence is expected for @racket[sync/timeout/enable-break]. For the event
 form of calling @racket[sync/timeout] with a thunk, see @racket[unready-evt].}

@defproc[(unready-evt) evt?]{}
@defproc[(break-checking-evt [evt evt?]) evt?]{}

@subsection{Asynchronous Event Loops}

@defproc[(fold-evt) evt?]{}
@defproc[(while-evt) evt?]{}
@defproc[(forever-evt) evt?]{}

@subsection{Observability}

@defproc[(logging-evt) evt?]{}
@defproc[(debug-evt) evt?]{}

@subsection{Miscellaneous}

@defproc[(call-evt [thunk (-> any)]
                   [#:custodian cust custodian? (current-custodian)])
         evt?]{
 Returns a @sync-evt-tech{synchronizable event} that is actually an
 @sync-maker-tech{event maker}. The maker creates events encapsulating a
 call to @racket[thunk].

 When a @thread-tech{thread} attempts to @sync-tech{synchronize} on the returned
 event, a child thread is spawned under the management of @racket[cust] that
 calls @racket[thunk]. The event is @sync-ready-tech{ready for synchronization}
 for the synchronizing thread when any of the following are true:

 @itemlist[
 @item{The call to @racket[thunk] completes successfully. In that case, the
   return values of the call become the @sync-result-tech{synchronization
    result} of the event.}
 @item{An exception is raised in the child thread, resulting in the exception
   being re-raised in the synchronizing thread.}
 @item{The child thread dies or is killed before the call to @racket[thunk]
   returns, also resulting in a re-raised exception in the synchronizing
   thread.}]

 If a @break-tech{break} is detected in the synchronizing thread while breaking
 is enabled, the break is transferred to the child thread instead of raised
 normally. The call to @racket[thunk] in the child thread is made with breaks
 enabled initially but @racket[thunk] may disable breaks as it sees fit. If the
 returned event is not @sync-chosen-tech{chosen for synchronization}, a break is
 sent to the child thread.

 Multiple threads can attempt to synchronize on the returned event concurrently.
 In that case, a separate call in an isolated child thread is created for each
 synchronizing thread. See @racket[share-evt] if this behavior is undesired.
 This behavior makes @racket[call-evt] a @sync-local-tech{local event}.

 This is similar in spirit to @racket[call-in-nested-thread]. It is intended
 that @racket[(call-in-nested-thread thunk cust)] is always equivalent to
 @racket[(sync (call-evt thunk cust))]. If this is not the case, please file a
 bug report.}

@subsection{Threads}

@defthing[spawn/c contract? #:value (-> (-> any) thread?)]
@defparam[current-spawn spawn spawn/c #:value thread]{}
@defproc[(spawn/manager-custodian [thunk (-> any)]
                                  [cust custodian?]
                                  [#:spawn base-spawn spawn/c (current-spawn)])
         thread?]{}
@defproc[(spawn/thread-group [thunk (-> any)]
                             [group thread-group?]
                             [#:spawn base-spawn spawn/c (current-spawn)])
         thread?]{}
@defproc[(spawn/logger [thunk (-> any)] [logger logger?]
                       [#:spawn base-spawn spawn/c (current-spawn)])
         thread?]{}
@defstruct*[death-watch
            ([thread thread?] [values-evt evt?] [raise-evt (evt/c any/c)])
            #:transparent #:omit-constructor]{}
@defproc[(spawn/death-watch [thunk (-> any)]
                            [#:spawn base-spawn spawn/c (current-spawn)])
         (list/c thread? evt? (evt/c any/c))]{}
@defproc[(break-thread/transfer [thread thread?] [break exn:break?]) void?]{}
