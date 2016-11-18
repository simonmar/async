## Changes in 2.1.1:

 - Add `concurrently_`
 - Add `replicateConcurrently`
 - Add `replicateConcurrently_`

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
