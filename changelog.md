## Changes in 2.2.5

 - #117: Document that empty for Concurrently waits forever
 - #120: Add ConcurrentlyE.
 - #123: Fix failing concurrentlyE tests in older GHCs.
 - #124: Allow hashable 1.4
 - #126: Semigroup and Monoid instances for ConcurrentlyE
 - #120: Add ConcurrentlyE
 - #138: expose internals as Control.Concurrent.Async.Internal
 - #131: Fix typos in docs
 - #132: waitAny(Catch): clarify non-empty input list requirement
 - #142: Add cancelMany
 - #135, #145, #150: Support for GHC 9.4, 9.6, 9.8
 - Document that empty for Concurrently waits forever.

## Changes in 2.2.4:

 - Support for GHC 9.2

## Changes in 2.2.3:

 - Documentation fixes

## Changes in 2.2.2:

 - Builds with GHC 8.6.x
 - linkOnly and link2Only are now exported
 - wait now has the same behaviour with BlockedIndefinitelyOnSTM as waitCatch
 - Documentation fixes

## Changes in 2.2.1:

 - Add a Hashable instance for Async
 - Bump upper bounds
 - Documentation updates

## Changes in 2.2:
 - cancel now throws AsyncCancelled instead of ThreadKilled
 - link and link2 now wrap exceptions in ExceptionInLinkedThread when
   throwing to the linked thread. ExceptionInLinkedThread is a child
   of AsyncException in the exception hierarchy, so this maintains the
   invariant that exceptions thrown asynchronously should be
   AsyncExceptions.
 - link and link2 do not propagate AsyncCancelled, so it's now
   possible to cancel a linked thread without cancelling yourself.
 - Added linkOnly and link2Only to specify which exceptions should be
   propagated,if you want something other than the default behaviour
   of ignoring AsyncCancelled.
 - new utility function compareAsyncs for comparing Asyncs of
   different types.
 - Add a `Hashable` instance for `Async a`

## Changes in 2.1.1.1:
 - Make 'cancelWith' wait for the cancelled thread to terminate, like 'cancel'
 - Updates to dependency bounds for GHC 8.2

## Changes in 2.1.1:

 - Add `concurrently_`
 - Add `replicateConcurrently`
 - Add `replicateConcurrently_`
 - Fix incorrect argument order in `forConcurrently_`
 - Generalize `mapConcurrently_` and `forConcurrently_` to `Foldable`
 - `withAsync` now reliably kills the thread, by using an
   uninterruptible cancel
 - Make `cancel` wait for the thread to finish, and adjust
   'concurrently' to match

## Changes in 2.1.0:

 - Bump base dependency to allow 4.10
 - Remove invalid Monad instance for `Concurrently`
 - Add `Monoid` and `Semigroup` instances for `Concurrently`
 - Add `forConcurrently` (flipped version of `mapConcurrently`)
 - Add STM version of all applicable IO functions:
   `waitAnySTM`, `waitAnyCatchSTM`, `waitEitherSTM`,
   `waitEitherCatchSTM`, `waitEitherSTM_`, and `waitBothSTM`.

## Changes in 2.0.2:

 - Add a Monad instance for `Concurrently`
 - Bump base dependency to allow 4.9

## Changes in 2.0.1.6:

 - Add workaround to waitCatch for #14

## Changes in 2.0.1.5:

 - Bump `base` dependencies for GHC 7.8

## Changes in 2.0.1.4:

 - Bump `base` dependency of test suite

## Changes in 2.0.1.3:

 - Bump `base` dependency to allow 4.6

## Changes in 2.0.1.2:

 - Bump `stm` dependency to 2.4

## Changes in 2.0.1.1:

 - Safe Haskell support: `Control.Concurrent.Async` is now `Trustworthy`

## Changes in 2.0.1.0:

 - Added a `Functor` instance for `Async`
 - Added `asyncBound`, `asyncOn`, `asyncWithUnmask`, `asyncOnWithUnmask`, `withAsyncBound`, `withAsyncOn`, `withAsyncWithUnmask`, `withAsyncOnWithUnmask`.
 - Added `mapConcurrently`
 - Added `Concurrently` (with `Applicative` and `Alternative` instances)
