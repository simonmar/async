{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
{-# OPTIONS -Wall #-}

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
-- robustness over using threads and @MVar@ directly.
--
-- The basic type is @'Async' a@, which represents an asynchronous
-- @IO@ action that will return a value of type @a@, or die with an
-- exception.  An @Async@ corresponds to a thread, and its 'ThreadId'
-- can be obtained with 'asyncThreadId', although that should rarely
-- be necessary.
--
-- For example, to fetch two web pages at the same time, we could do
-- this (assuming a suitable @getURL@ function):
--
-- >    do a1 <- async (getURL url1)
-- >       a2 <- async (getURL url2)
-- >       page1 <- waitThrow a1
-- >       page2 <- waitThrow a2
-- >       ...
--
-- where 'async' starts the operation in a separate thread, and
-- 'waitThrow' waits for and returns the result.  If the operation
-- throws an exception, then that exception is re-thrown by
-- 'waitThrow'.  This is one of the ways in which this library
-- provides some additional safety: it is harder to accidentally
-- forget about exceptions thrown in child threads.
--
-- A slight improvement over the previous example is this:
--
-- >       withAsync (getURL url1) $ \a1 -> do
-- >       withAsync (getURL url2) $ \a2 -> do
-- >       page1 <- waitThrow a1
-- >       page2 <- waitThrow a2
-- >       ...
--
-- 'withAsync' is like 'async', except that the 'Async' is
-- automatically killed (using 'cancel') if the enclosing IO operation
-- returns before it has completed.  Consider the case when the first
-- 'waitThrow' throws an exception; then the second 'Async' will be
-- automatically killed rather than being left to run in the
-- background, possibly indefinitely.  This is the second way that the
-- library provides additional safety: using 'withAsync' means we can
-- avoid accidentally leaving threads running.
--
-- Furthermore, 'withAsync' allows a tree of threads to be built, such
-- that children are automatically killed if their parents die for any
-- reason.
--
-----------------------------------------------------------------------------

module Control.Concurrent.Async (

    -- * Async
    Async, async, withAsync, asyncThreadId,
    wait, tryWait, waitThrow, cancel, cancelWith,

    -- ** STM operations
    waitSTM, tryWaitSTM, waitThrowSTM,

    -- ** Waiting for multiple asyncs
    waitAny, waitAnyCancel, waitAnyThrow, waitAnyThrowCancel,
    waitEither, waitEitherCancel, waitEitherThrow,
    waitEitherThrow_, waitEitherThrowCancel,
    waitBothThrow,

    -- ** Linking
    link, link2,

    -- * Convenient utilities
    race, race_, concurrently,
  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Concurrent
import Prelude hiding (catch)
import Control.Monad
import Control.Applicative

import GHC.Exts
import GHC.IO hiding (finally, onException)
import GHC.Conc

-- -----------------------------------------------------------------------------
-- STM Async API


-- | An asynchronous action spawned by 'async' or 'withAsync'.
-- Asynchronous actions are executed in a separate thread, and
-- operations are provided for waiting for asynchronous actions to
-- complete and obtaining their results (see e.g. 'wait').
--
data Async a = Async { asyncThreadId :: {-# UNPACK #-} !ThreadId
                     , _asyncVar     :: {-# UNPACK #-} !(TMVar (Either SomeException a)) }

instance Eq (Async a) where
  Async a _ == Async b _  =  a == b

instance Ord (Async a) where
  Async a _ `compare` Async b _  =  a `compare` b

-- | Spawn an asynchronous action in a separate thread.
async :: IO a -> IO (Async a)
async action = do
   var <- newEmptyTMVarIO
   -- t <- forkFinally action (\r -> atomically $ putTMVar var r)
   -- slightly faster:
   t <- mask $ \restore ->
          rawForkIO $ do r <- try (restore action); atomically $ putTMVar var r
   return (Async t var)

-- | Spawn an asynchronous action in a separate thread, and pass its
-- @Async@ handle to the supplied function.  When the function returns
-- or throws an exception, 'cancel' is called on the @Async@.
--
-- > withAsync action inner = bracket (async action) cancel inner
--
-- This is a useful variant of 'async' that ensures an @Async@ is
-- never left running unintentionally.
--
withAsync :: IO a -> (Async a -> IO b) -> IO b
-- The bracket version works, but is slow.  We can do better by
-- hand-coding it:
withAsync action inner = do
  var <- newEmptyTMVarIO
  mask $ \restore -> do
    t <- rawForkIO $ try (restore action) >>= \r -> atomically $ putTMVar var r
    let a = Async t var
    r <- restore (inner a) `catchAll` \e -> do cancel a; throwIO e
    cancel a
    return r

-- | Wait for an asynchronous action to complete, and return either
-- @Left e@ if the action raised an exception @e@, or @Right a@ if it
-- returned a value @a@.
--
-- > wait = atomically . waitSTM
--
{-# INLINE wait #-}
wait :: Async a -> IO (Either SomeException a)
wait = atomically . waitSTM

-- | Check whether an 'Async' has completed yet.  If it has not
-- completed yet, then the result is @Nothing@, otherwise the result
-- is @Just e@ where @e@ is @Left x@ if the @Async@ raised an
-- exception @x@, or @Right a@ if it returned a value @a@.
--
-- > wait = atomically . tryWaitSTM
--
{-# INLINE tryWait #-}
tryWait :: Async a -> IO (Maybe (Either SomeException a))
tryWait = atomically . tryWaitSTM

-- | A version of 'wait' that throws the exception if the asynchronous
-- action raised one, or otherwise returns its result.
--
-- > waitThrow = atomically . waitThrowSTM
--
{-# INLINE waitThrow #-}
waitThrow :: Async a -> IO a
waitThrow = atomically . waitThrowSTM

-- | A version of 'wait' that can be used inside an STM transaction.
--
{-# INLINE waitSTM #-}
waitSTM :: Async a -> STM (Either SomeException a)
waitSTM (Async _ var) = readTMVar var

-- | A version of 'tryWait' that can be used inside an STM transaction.
--
{-# INLINE tryWaitSTM #-}
tryWaitSTM :: Async a -> STM (Maybe (Either SomeException a))
tryWaitSTM (Async _ var) = tryReadTMVar var

-- | A version of 'waitThrow' that can be used inside an STM transaction.
--
waitThrowSTM :: Async a -> STM a
waitThrowSTM (Async _ var) = do
   r <- readTMVar var
   either throwSTM return r

-- | Cancel an asynchronous action by throwing the @ThreadKilled@
-- exception to it.  Has no effect if the 'Async' has already
-- completed.
{-# INLINE cancel #-}
cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled

-- | Cancel an asynchronous action by throwing the supplied exception
-- to it.
--
cancelWith :: Exception e => Async a -> e -> IO ()
cancelWith (Async t _) e = throwTo t e

-- | Wait for any of the supplied asynchronous operations to complete.
-- The value returned is a pair of the 'Async' that completed, and the
-- result that would be returned by 'wait' on that 'Async'.
--
-- If multiple 'Async's complete or have completed, then the value
-- returned corresponds to the first completed 'Async' in the list.
--
waitAny :: [Async a] -> IO (Async a, Either SomeException a)
waitAny asyncs =
  atomically $
    foldr orElse retry $
      map (\a -> do r <- waitSTM a; return (a, r)) asyncs

-- | Like 'waitAny', but also cancels the other asynchronous
-- operations as soon as one has completed.
--
waitAnyCancel :: [Async a] -> IO (Async a, Either SomeException a)
waitAnyCancel asyncs =
  waitAny asyncs `finally` mapM_ cancel asyncs

-- | Wait for any of the supplied @Async@s to complete.  If the first
-- to complete throw an exception, then that exception is re-thrown
-- by 'waitAnyThrow'.
--
waitAnyThrow :: [Async a] -> IO (Async a, a)
waitAnyThrow asyncs =
  atomically $
    foldr orElse retry $
      map (\a -> do r <- waitThrowSTM a; return (a, r)) asyncs

-- | Like 'waitAnyThrow', but also cancels the other asynchronous
-- operations as soon as one has completed.
--
waitAnyThrowCancel :: [Async a] -> IO (Async a, a)
waitAnyThrowCancel asyncs =
  waitAnyThrow asyncs `finally` mapM_ cancel asyncs

-- | Wait for the first of two @Async@s to finish.
waitEither :: Async a -> Async b
           -> IO (Either (Either SomeException a)
                         (Either SomeException b))
waitEither left right =
  atomically $
    (Left  <$> waitSTM left)
      `orElse`
    (Right <$> waitSTM right)

-- | Like 'waitEither', but also 'cancel's both @Async@s before
-- returning.
--
waitEitherCancel :: Async a -> Async b
                 -> IO (Either (Either SomeException a)
                               (Either SomeException b))
waitEitherCancel left right =
  waitEither left right `finally` (cancel left >> cancel right)

-- | Wait for the first of two @Async@s to finish.  If the @Async@
-- that finished first raised an exception, then the exception is
-- re-thrown by 'waitEitherThrow'.
--
waitEitherThrow :: Async a -> Async b -> IO (Either a b)
waitEitherThrow left right =
  atomically $
    (Left  <$> waitThrowSTM left)
      `orElse`
    (Right <$> waitThrowSTM right)

-- | Like 'waitEitherThrow', but the results are ignored.
--
waitEitherThrow_ :: Async a -> Async b -> IO ()
waitEitherThrow_ left right =
  atomically $
    (void $ waitThrowSTM left)
      `orElse`
    (void $ waitThrowSTM right)

-- | Like 'waitEitherThow', but also 'cancel's both @Async@s before
-- returning.
--
waitEitherThrowCancel :: Async a -> Async b -> IO (Either a b)
waitEitherThrowCancel left right =
  waitEitherThrow left right `finally` (cancel left >> cancel right)

-- | Waits for both @Async@s to finish, but if either of them throws
-- an exception before they have both finished, then the exception is
-- re-thrown by 'waitBothThrow'.
--
waitBothThrow :: Async a -> Async b -> IO (a,b)
waitBothThrow left right =
  atomically $ do
    a <- waitThrowSTM left
           `orElse`
         (waitThrowSTM right >> retry)
    b <- waitThrowSTM right
    return (a,b)


-- | Link the given @Async@ to the current thread, such that if the
-- @Async@ raises an exception, that exception will be re-thrown in
-- the current thread.
--
link :: Async a -> IO ()
link (Async _ var) = do
  me <- myThreadId
  void $ forkRepeat $ do
     r <- atomically $ readTMVar var
     case r of
       Left e -> throwTo me e
       _ -> return ()

-- | Link two @Async@s together, such that if either raises an
-- exception, the same exception is re-thrown in the other @Async@.
--
link2 :: Async a -> Async b -> IO ()
link2 left@(Async tl _)  right@(Async tr _) =
  void $ forkRepeat $ do
    r <- waitEither left right
    case r of
      Left  (Left e) -> throwTo tr e
      Right (Left e) -> throwTo tl e
      _ -> return ()



-- -----------------------------------------------------------------------------

-- | Run two @IO@ actions concurrently, and return the first to
-- finish.  The loser of the race is 'cancel'led.
--
-- > race left right =
-- >   withAsync left $ \a ->
-- >   withAsync right $ \b ->
-- >   waitEitherThrow a b
--
race :: IO a -> IO b -> IO (Either a b)

-- | Like 'race', but the result is ignored.
--
race_ :: IO a -> IO b -> IO ()

-- | Run two @IO@ actions concurrently, and return both results.  If
-- either action throws an exception at any time, then the other
-- action is 'cancel'led, and the exception is re-thrown by
-- 'concurrently'.
--
-- > concurrently left right =
-- >   withAsync left $ \a ->
-- >   withAsync right $ \b ->
-- >   waitBothThrow a b
concurrently :: IO a -> IO b -> IO (a,b)

#define USE_ASYNC_VERSIONS 0

#if USE_ASYNC_VERSIONS

race left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitEitherThrow a b

race_ left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitEitherThrow_ a b

concurrently left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitBothThrow a b

#else

-- MVar versions of race/concurrently
-- More ugly than the Async versions, but quite a bit faster.

-- race :: IO a -> IO b -> IO (Either a b)
race left right = concurrently' left right collect
  where
    collect m = do
        e <- takeMVar m
        case e of
            Left ex -> throwIO ex
            Right r -> return r

-- race_ :: IO a -> IO b -> IO ()
race_ left right = void $ race left right

-- concurrently :: IO a -> IO b -> IO (a,b)
concurrently left right = concurrently' left right (collect [])
  where
    collect [Left a, Right b] _ = return (a,b)
    collect [Right b, Left a] _ = return (a,b)
    collect xs m = do
        e <- takeMVar m
        case e of
            Left ex -> throwIO ex
            Right r -> collect (r:xs) m

concurrently' :: IO a -> IO b
             -> (MVar (Either SomeException (Either a b)) -> IO r)
             -> IO r
concurrently' left right collect = do
    done <- newEmptyMVar
    mask $ \restore -> do
        lid <- forkIO $ restore (left >>= putMVar done . Right . Left)
                             `catchAll` (putMVar done . Left)
        rid <- forkIO $ restore (right >>= putMVar done . Right . Right)
                             `catchAll` (putMVar done . Left)
        let stop = killThread lid >> killThread rid
        r <- restore (collect done) `onException` stop
        stop
        return r

#endif

-- ----------------------------------------------------------------------------

-- | Fork a thread that runs the supplied action, and if it raises an
-- exception, re-runs the action.  The thread terminates only when the
-- action runs to completion without raising an exception.
forkRepeat :: IO a -> IO ThreadId
forkRepeat action =
  mask $ \restore ->
    let go = do r <- tryAll (restore action)
                case r of
                  Left _ -> go
                  _      -> return ()
    in forkIO go

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch

tryAll :: IO a -> IO (Either SomeException a)
tryAll = try

-- A version of forkIO that does not include the outer exception
-- handler: saves a bit of time when we will be installing our own
-- exception handler.
{-# INLINE rawForkIO #-}
rawForkIO :: IO () -> IO ThreadId
rawForkIO action = IO $ \ s ->
   case (fork# action s) of (# s1, tid #) -> (# s1, ThreadId tid #)
