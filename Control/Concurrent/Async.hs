-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.Async
-- Copyright   :  (c) Simon Marlow 2012
-- License     :  BSD3 (see the file LICENSE)
--
-- Maintainer  :  Simon Marlow <marlowsd@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable (requires concurrency)
--
-- This module provides a set of operations for running IO operations
-- asynchronously and waiting for their results.  It is a thin layer
-- over the basic concurrency operations provided by
-- "Control.Concurrent".  The main additional functionality it
-- provides is the ability to wait for the return value of a thread,
-- but the interface also provides some additional safety and
-- robustness over using 'forkIO' threads and @MVar@ directly.
--
-- == High-level API
--
-- @async@'s high-level API spawns /lexically scoped/ threads,
-- ensuring the following key poperties that make it safer to use
-- than using plain 'forkIO':
--
-- 1. No exception is swallowed (waiting for results propagates exceptions).
-- 2. No thread is leaked (left running unintentionally).
--
-- (This is done using the 'Control.Exception.bracket' pattern to work in presence
-- of synchronous and asynchronous exceptions.)
--
-- __Most practical/production code should only use the high-level API__.
--
-- The basic type is @'Async' a@, which represents an asynchronous
-- @IO@ action that will return a value of type @a@, or die with an
-- exception.  An 'Async' is a wrapper around a low-level 'forkIO' thread.
--
-- The fundamental function to spawn threads with the high-level API is
-- 'withAsync'.
--
-- For example, to fetch two web pages at the same time, we could do
-- this (assuming a suitable @getURL@ function):
--
-- > withAsync (getURL url1) $ \a1 -> do
-- >   withAsync (getURL url2) $ \a2 -> do
-- >     page1 <- wait a1
-- >     page2 <- wait a2
-- >     ...
--
-- where 'withAsync' starts the operation in a separate thread, and
-- 'wait' waits for and returns the result.
--
-- * If the operation throws an exception, then that exception is re-thrown
--   by 'wait'. This ensures property (1): No exception is swallowed.
-- * If an exception bubbles up through a 'withAsync', then the 'Async'
--   it spawned is 'cancel'ed. This ensures property (2): No thread is leaked.
--
-- Often we do not care to work manually with 'Async' handles like
-- @a1@ and @a2@. Instead, we want to express high-level objectives like
-- performing two or more tasks concurrently, and waiting for one or all
-- of them to finish.
--
-- For example, the pattern of performing two IO actions concurrently and
-- waiting for both their results is packaged up in a combinator 'concurrently',
-- so we can further shorten the above example to:
--
-- > (page1, page2) <- concurrently (getURL url1) (getURL url2)
-- > ...
--
-- The section __/High-level utilities/__ covers the most
-- common high-level objectives, including:
--
-- * Waiting for 2 results ('concurrently').
-- * Waiting for many results ('mapConcurrently' / 'forConcurrently').
-- * Waiting for the first of 2 results ('race').
-- * Waiting for arbitrary nestings of "all of /N/" and "the first of /N/"
--   results with the 'Concurrently' newtype and its 'Applicative' and
--   'Alternative' instances.
--
-- Click here to scroll to that section:
-- "Control.Concurrent.Async#high-level-utilities".
--
-- == Low-level API
--
-- Some use cases require parallelism that is not lexically scoped.
--
-- For those, the low-level function 'async' can be used as a direct
-- equivalent of 'forkIO':
--
-- > -- Do NOT use this code in production, it has a flaw (explained below).
-- > do
-- >   a1 <- async (getURL url1)
-- >   a2 <- async (getURL url2)
-- >   page1 <- wait a1
-- >   page2 <- wait a2
-- >   ...
--
-- In contrast to 'withAsync', this code has a problem.
--
-- It still fulfills property (1) in that an exception arising from
-- @getUrl@ will be re-thrown by 'wait', but it does not fulfill
-- property (2).
-- Consider the case when the first 'wait' throws an exception; then the
-- second 'wait' will not happen, and the second 'async' may be left
-- running in the background, possibly indefinitely.
--
-- 'withAsync' is like 'async', except that the 'Async' is
-- automatically killed (using 'uninterruptibleCancel') if the
-- enclosing IO operation returns before it has completed.
-- Furthermore, 'withAsync' allows a tree of threads to be built, such
-- that children are automatically killed if their parents die for any
-- reason.
--
-- If you need to use the low-level API, ensure that you guarantee
-- property (2) by other means, such as 'link'ing asyncs that need
-- to die together, and protecting against asynchronous exceptions
-- using 'Control.Exception.bracket', 'Control.Exception.mask',
-- or other functions from "Control.Exception".
--
-- == Miscellaneous
--
-- The 'Functor' instance can be used to change the result of an
-- 'Async'.  For example:
--
-- > ghci> withAsync (return 3) (\a -> wait (fmap (+1) a))
-- > 4
--
-- === Resource exhaustion
--
-- As with all concurrent programming, keep in mind that while
-- Haskell's cooperative ("green") multithreading carries low overhead,
-- spawning too many of them at the same time may lead to resource exhaustion
-- (of memory, file descriptors, or other limited resources), given that the
-- actions running in the threads consume these resources.

-----------------------------------------------------------------------------

module Control.Concurrent.Async (

    -- * Asynchronous actions
    Async,

    -- * High-level API

    -- ** Spawning with automatic 'cancel'ation
    withAsync, withAsyncBound, withAsyncOn, withAsyncWithUnmask,
    withAsyncOnWithUnmask,

    -- ** Querying 'Async's
    wait, poll, waitCatch, asyncThreadId,
    cancel, cancelMany, uninterruptibleCancel, cancelWith, AsyncCancelled(..),

    -- ** #high-level-utilities# High-level utilities
    race, race_,
    concurrently, concurrently_,
    mapConcurrently, forConcurrently,
    mapConcurrently_, forConcurrently_,
    replicateConcurrently, replicateConcurrently_,
    Concurrently(..),
    concurrentlyE,
    ConcurrentlyE(..),
    compareAsyncs,

    -- ** Specialised operations

    -- *** STM operations
    waitSTM, pollSTM, waitCatchSTM,

    -- *** Waiting for multiple 'Async's
    waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel,
    waitEither, waitEitherCatch, waitEitherCancel, waitEitherCatchCancel,
    waitEither_,
    waitBoth,

    -- *** Waiting for multiple 'Async's in STM
    waitAnySTM, waitAnyCatchSTM,
    waitEitherSTM, waitEitherCatchSTM,
    waitEitherSTM_,
    waitBothSTM,

    -- * Low-level API

    -- ** Spawning (low-level API)
    async, asyncBound, asyncOn, asyncWithUnmask, asyncOnWithUnmask,

    -- ** Linking
    link, linkOnly, link2, link2Only, ExceptionInLinkedThread(..),

  ) where

import Control.Concurrent.Async.Internal
