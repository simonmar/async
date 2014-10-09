{-# LANGUAGE CPP, MagicHash, UnboxedTuples, RankNTypes,
             ScopedTypeVariables, DeriveDataTypeable,
             ExistentialQuantification #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
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
-- >       page1 <- wait a1
-- >       page2 <- wait a2
-- >       ...
--
-- where 'async' starts the operation in a separate thread, and
-- 'wait' waits for and returns the result.  If the operation
-- throws an exception, then that exception is re-thrown by
-- 'wait'.  This is one of the ways in which this library
-- provides some additional safety: it is harder to accidentally
-- forget about exceptions thrown in child threads.
--
-- A slight improvement over the previous example is this:
--
-- >       withAsync (getURL url1) $ \a1 -> do
-- >       withAsync (getURL url2) $ \a2 -> do
-- >       page1 <- wait a1
-- >       page2 <- wait a2
-- >       ...
--
-- 'withAsync' is like 'async', except that the 'Async' is
-- automatically killed (using 'cancel') if the enclosing IO operation
-- returns before it has completed.  Consider the case when the first
-- 'wait' throws an exception; then the second 'Async' will be
-- automatically killed rather than being left to run in the
-- background, possibly indefinitely.  This is the second way that the
-- library provides additional safety: using 'withAsync' means we can
-- avoid accidentally leaving threads running.  Furthermore,
-- 'withAsync' allows a tree of threads to be built, such that
-- children are automatically killed if their parents die for any
-- reason.
--
-- The pattern of performing two IO actions concurrently and waiting
-- for their results is packaged up in a combinator 'concurrently', so
-- we can further shorten the above example to:
--
-- >       (page1, page2) <- concurrently (getURL url1) (getURL url2)
-- >       ...
--
-- The 'Functor' instance can be used to change the result of an
-- 'Async'.  For example:
--
-- > ghci> a <- async (return 3)
-- > ghci> wait a
-- > 3
-- > ghci> wait (fmap (+1) a)
-- > 4

-----------------------------------------------------------------------------

module Control.Concurrent.Async (

    -- * Asynchronous actions
    Async,
    -- ** Spawning
    async, asyncBound, asyncOn, asyncWithUnmask, asyncOnWithUnmask,

    -- ** Spawning with automatic 'cancel'ation
    withAsync, withAsyncBound, withAsyncOn, withAsyncWithUnmask, withAsyncOnWithUnmask,

    -- ** Quering 'Async's
    wait, poll, waitCatch, cancel, cancelWith,
    asyncThreadId,

    -- ** STM operations
    waitSTM, pollSTM, waitCatchSTM,

    -- ** Waiting for multiple 'Async's
    waitAny, waitAnyCatch, waitAnyCancel, waitAnyCatchCancel,
    waitEither, waitEitherCatch, waitEitherCancel, waitEitherCatchCancel,
    waitEither_,
    waitBoth,

    -- ** Linking
    link, link2,

    -- * Convenient utilities
    race, race_, concurrently, mapConcurrently,
    Concurrently(..),

  ) where

import Control.Concurrent.STM
import Control.Exception
import Control.Concurrent
#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif
import Control.Monad
import Control.Applicative
import Data.Traversable
import Data.Typeable
import Data.Unique
import Unsafe.Coerce

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
                       -- ^ Returns the 'ThreadId' of the thread running the given 'Async'.
                     , _asyncWait    :: STM (Either SomeException a) }

instance Eq (Async a) where
  Async a _ == Async b _  =  a == b

instance Ord (Async a) where
  Async a _ `compare` Async b _  =  a `compare` b

instance Functor Async where
  fmap f (Async a w) = Async a (fmap (fmap f) w)

-- | Spawn an asynchronous action in a separate thread.
async :: IO a -> IO (Async a)
async = inline asyncUsing rawForkIO

-- | Like 'async' but using 'forkOS' internally.
asyncBound :: IO a -> IO (Async a)
asyncBound = asyncUsing forkOS

-- | Like 'async' but using 'forkOn' internally.
asyncOn :: Int -> IO a -> IO (Async a)
asyncOn = asyncUsing . rawForkOn

-- | Like 'async' but using 'forkIOWithUnmask' internally.
-- The child thread is passed a function that can be used to unmask asynchronous exceptions.
asyncWithUnmask :: ((forall b . IO b -> IO b) -> IO a) -> IO (Async a)
asyncWithUnmask actionWith = asyncUsing rawForkIO (actionWith unsafeUnmask)

-- | Like 'asyncOn' but using 'forkOnWithUnmask' internally.
-- The child thread is passed a function that can be used to unmask asynchronous exceptions.
asyncOnWithUnmask :: Int -> ((forall b . IO b -> IO b) -> IO a) -> IO (Async a)
asyncOnWithUnmask cpu actionWith = asyncUsing (rawForkOn cpu) (actionWith unsafeUnmask)

asyncUsing :: (IO () -> IO ThreadId)
           -> IO a -> IO (Async a)
asyncUsing doFork = \action -> do
   var <- newEmptyTMVarIO
   -- t <- forkFinally action (\r -> atomically $ putTMVar var r)
   -- slightly faster:
   t <- mask $ \restore ->
          doFork $ try (restore action) >>= atomically . putTMVar var
   return (Async t (readTMVar var))

-- | Spawn an asynchronous action in a separate thread, and pass its
-- @Async@ handle to the supplied function.  When the function returns
-- or throws a /synchronous/ exception, 'cancel' is called on the
-- @Async@.  When the function throws an /asynchronous/ exception, the
-- exception is rethrown to the other thread.
--
-- This is a useful variant of 'async' that ensures an @Async@ is
-- never left running unintentionally.
--
-- Since 'cancel' may block, 'withAsync' may also block; see 'cancel'
-- for details.
--
withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync = inline withAsyncUsing rawForkIO

-- | Like 'withAsync' but uses 'forkOS' internally.
withAsyncBound :: IO a -> (Async a -> IO b) -> IO b
withAsyncBound = withAsyncUsing forkOS

-- | Like 'withAsync' but uses 'forkOn' internally.
withAsyncOn :: Int -> IO a -> (Async a -> IO b) -> IO b
withAsyncOn = withAsyncUsing . rawForkOn

-- | Like 'withAsync' but uses 'forkIOWithUnmask' internally.
-- The child thread is passed a function that can be used to unmask asynchronous exceptions.
withAsyncWithUnmask :: ((forall c. IO c -> IO c) -> IO a) -> (Async a -> IO b) -> IO b
withAsyncWithUnmask actionWith = withAsyncUsing rawForkIO (actionWith unsafeUnmask)

-- | Like 'withAsyncOn' but uses 'forkOnWithUnmask' internally.
-- The child thread is passed a function that can be used to unmask asynchronous exceptions
withAsyncOnWithUnmask :: Int -> ((forall c. IO c -> IO c) -> IO a) -> (Async a -> IO b) -> IO b
withAsyncOnWithUnmask cpu actionWith = withAsyncUsing (rawForkOn cpu) (actionWith unsafeUnmask)

withAsyncUsing :: (IO () -> IO ThreadId)
               -> IO a -> (Async a -> IO b) -> IO b
-- The bracket version works, but is slow.  We can do better by
-- hand-coding it:
withAsyncUsing doFork = \action inner -> do
  var <- newEmptyTMVarIO
  mask $ \restore -> do
    t <- doFork $ try (restore action) >>= atomically . putTMVar var
    let a = Async t (readTMVar var)
    r <- restore (inner a) `alsoThrowingTo` t
    cancel a
    return r

-- | If the given action throws an asynchronous exception then also
-- throw it to the specified thread. If it throws a synchronous
-- exception then kill the specified thread.
alsoThrowingTo :: IO a -> ThreadId -> IO a
m `alsoThrowingTo` tid = m `catch` handler
  where
    handler e = do
      case fromException e of
#       if MIN_VERSION_base(4,7,0)
          Just (_ :: SomeAsyncException) -> throwTo tid e
#       else
          Just (_ :: AsyncException)     -> throwTo tid e
#       endif
          Nothing                        -> throwTo tid ThreadKilled
      throwIO e

-- | Wait for an asynchronous action to complete, and return its
-- value.  If the asynchronous action threw an exception, then the
-- exception is re-thrown by 'wait'.
--
-- > wait = atomically . waitSTM
--
{-# INLINE wait #-}
wait :: Async a -> IO a
wait = atomically . waitSTM

-- | Wait for an asynchronous action to complete, and return either
-- @Left e@ if the action raised an exception @e@, or @Right a@ if it
-- returned a value @a@.
--
-- > waitCatch = atomically . waitCatchSTM
--
{-# INLINE waitCatch #-}
waitCatch :: Async a -> IO (Either SomeException a)
waitCatch = tryAgain . atomically . waitCatchSTM
  where
    -- See: https://github.com/simonmar/async/issues/14
    tryAgain f = f `catch` \BlockedIndefinitelyOnSTM -> f

-- | Check whether an 'Async' has completed yet.  If it has not
-- completed yet, then the result is @Nothing@, otherwise the result
-- is @Just e@ where @e@ is @Left x@ if the @Async@ raised an
-- exception @x@, or @Right a@ if it returned a value @a@.
--
-- > poll = atomically . pollSTM
--
{-# INLINE poll #-}
poll :: Async a -> IO (Maybe (Either SomeException a))
poll = atomically . pollSTM

-- | A version of 'wait' that can be used inside an STM transaction.
--
waitSTM :: Async a -> STM a
waitSTM a = do
   r <- waitCatchSTM a
   either throwSTM return r

-- | A version of 'waitCatch' that can be used inside an STM transaction.
--
{-# INLINE waitCatchSTM #-}
waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ w) = w

-- | A version of 'poll' that can be used inside an STM transaction.
--
{-# INLINE pollSTM #-}
pollSTM :: Async a -> STM (Maybe (Either SomeException a))
pollSTM (Async _ w) = (Just <$> w) `orElse` return Nothing

-- | Cancel an asynchronous action by throwing the @ThreadKilled@
-- exception to it.  Has no effect if the 'Async' has already
-- completed.
--
-- > cancel a = throwTo (asyncThreadId a) ThreadKilled
--
-- Note that 'cancel' is synchronous in the same sense as 'throwTo'.
-- It does not return until the exception has been thrown in the
-- target thread, or the target thread has completed.  In particular,
-- if the target thread is making a foreign call, the exception will
-- not be thrown until the foreign call returns, and in this case
-- 'cancel' may block indefinitely.  An asynchronous 'cancel' can
-- of course be obtained by wrapping 'cancel' itself in 'async'.
--
{-# INLINE cancel #-}
cancel :: Async a -> IO ()
cancel (Async t _) = throwTo t ThreadKilled

-- | Cancel an asynchronous action by throwing the supplied exception
-- to it.
--
-- > cancelWith a x = throwTo (asyncThreadId a) x
--
-- The notes about the synchronous nature of 'cancel' also apply to
-- 'cancelWith'.
cancelWith :: Exception e => Async a -> e -> IO ()
cancelWith (Async t _) e = throwTo t e

-- | Wait for any of the supplied asynchronous operations to complete.
-- The value returned is a pair of the 'Async' that completed, and the
-- result that would be returned by 'wait' on that 'Async'.
--
-- If multiple 'Async's complete or have completed, then the value
-- returned corresponds to the first completed 'Async' in the list.
--
waitAnyCatch :: [Async a] -> IO (Async a, Either SomeException a)
waitAnyCatch asyncs =
  atomically $
    foldr orElse retry $
      map (\a -> do r <- waitCatchSTM a; return (a, r)) asyncs

-- | Like 'waitAnyCatch', but also cancels the other asynchronous
-- operations as soon as one has completed.
--
waitAnyCatchCancel :: [Async a] -> IO (Async a, Either SomeException a)
waitAnyCatchCancel asyncs =
  waitAnyCatch asyncs `finally` mapM_ cancel asyncs

-- | Wait for any of the supplied @Async@s to complete.  If the first
-- to complete throws an exception, then that exception is re-thrown
-- by 'waitAny'.
--
-- If multiple 'Async's complete or have completed, then the value
-- returned corresponds to the first completed 'Async' in the list.
--
waitAny :: [Async a] -> IO (Async a, a)
waitAny asyncs =
  atomically $
    foldr orElse retry $
      map (\a -> do r <- waitSTM a; return (a, r)) asyncs

-- | Like 'waitAny', but also cancels the other asynchronous
-- operations as soon as one has completed.
--
waitAnyCancel :: [Async a] -> IO (Async a, a)
waitAnyCancel asyncs =
  waitAny asyncs `finally` mapM_ cancel asyncs

-- | Wait for the first of two @Async@s to finish.
waitEitherCatch :: Async a -> Async b
                -> IO (Either (Either SomeException a)
                              (Either SomeException b))
waitEitherCatch left right =
  atomically $
    (Left  <$> waitCatchSTM left)
      `orElse`
    (Right <$> waitCatchSTM right)

-- | Like 'waitEitherCatch', but also 'cancel's both @Async@s before
-- returning.
--
waitEitherCatchCancel :: Async a -> Async b
                      -> IO (Either (Either SomeException a)
                                    (Either SomeException b))
waitEitherCatchCancel left right =
  waitEitherCatch left right `finally` (cancel left >> cancel right)

-- | Wait for the first of two @Async@s to finish.  If the @Async@
-- that finished first raised an exception, then the exception is
-- re-thrown by 'waitEither'.
--
waitEither :: Async a -> Async b -> IO (Either a b)
waitEither left right =
  atomically $
    (Left  <$> waitSTM left)
      `orElse`
    (Right <$> waitSTM right)

-- | Like 'waitEither', but the result is ignored.
--
waitEither_ :: Async a -> Async b -> IO ()
waitEither_ left right =
  atomically $
    (void $ waitSTM left)
      `orElse`
    (void $ waitSTM right)

-- | Like 'waitEither', but also 'cancel's both @Async@s before
-- returning.
--
waitEitherCancel :: Async a -> Async b -> IO (Either a b)
waitEitherCancel left right =
  waitEither left right `finally` (cancel left >> cancel right)

-- | Waits for both @Async@s to finish, but if either of them throws
-- an exception before they have both finished, then the exception is
-- re-thrown by 'waitBoth'.
--
waitBoth :: Async a -> Async b -> IO (a,b)
waitBoth left right =
  atomically $ do
    a <- waitSTM left
           `orElse`
         (waitSTM right >> retry)
    b <- waitSTM right
    return (a,b)


-- | Link the given @Async@ to the current thread, such that if the
-- @Async@ raises an exception, that exception will be re-thrown in
-- the current thread.
--
link :: Async a -> IO ()
link (Async _ w) = do
  me <- myThreadId
  void $ forkRepeat $ do
     r <- atomically $ w
     case r of
       Left e -> throwTo me e
       _ -> return ()

-- | Link two @Async@s together, such that if either raises an
-- exception, the same exception is re-thrown in the other @Async@.
--
link2 :: Async a -> Async b -> IO ()
link2 left@(Async tl _)  right@(Async tr _) =
  void $ forkRepeat $ do
    r <- waitEitherCatch left right
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
-- >   waitEither a b
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
-- >   waitBoth a b
concurrently :: IO a -> IO b -> IO (a,b)

#define USE_ASYNC_VERSIONS 0

#if USE_ASYNC_VERSIONS

race left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitEither a b

race_ left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitEither_ a b

concurrently left right =
  withAsync left $ \a ->
  withAsync right $ \b ->
  waitBoth a b

#else

-- MVar versions of race/concurrently
-- More ugly than the Async versions, but quite a bit faster.

-- @race left right@ forks a thread to perform the @left@ computation and
-- performs the @right@ computation in the current thread. When one of them
-- terminates, whether normally or by raising an exception, the other thread is
-- interrupt by way of a specially crafted asynchronous exception.
--
-- More concretely:
--
-- * When @left@ terminates, whether normally or by raising an
--   exception, it wraps its result in the 'UniqueInterruptWithResult'
--   exception and throws that to the right thread.
--
-- * When the right thread catches the 'UniqueInterruptWithResult'
--   exception it will either throw the contained exception or return
--   the left result normally.
--
-- * When @right@ terminates, whether normally or by raising an
--   exception, it throws an 'UniqueInterrupt' exception to the left
--   thread in order to stop that thread from doing any more work.
--
-- Because calls to @race@ can be nested it's important that different
-- 'UniqueInterrupt' or 'UniqueInterruptWithResult' exceptions are not
-- mixed-up. For this reason each call to @race@ creates a 'Unique'
-- value that gets embedded in the interrupt exceptions being
-- thrown. When catching the interrupt exceptions we check if the
-- Unique equals the Unique of this invocation of @race@. (This is the
-- same trick used in the Timeout exception from System.Timeout).

-- race :: IO a -> IO b -> IO (Either a b)
race left right = do
  rightTid <- myThreadId
  u <- newUnique
  mask $ \restore -> do
    leftTid <- forkIO $
      catch
        (do l <- restore left
            throwTo rightTid $ UniqueInterruptWithResult u $ Right l) $ \e ->
        case fromException e of
          Just (UniqueInterrupt u') | u == u' -> return ()
          _ -> throwTo rightTid $ UniqueInterruptWithResult u (Left e)
    catch
      (do r <- restore right
          throwTo leftTid $ UniqueInterrupt u
          return $ Right r) $ \e ->
      case fromException e of
        Just (UniqueInterruptWithResult u' leftResult) | u == u' ->
          case leftResult of
            Left ex -> throwIO ex
            Right l -> return $ Left $ unsafeCoerce l
        _ -> do
          throwTo leftTid $ UniqueInterrupt u
          throwIO e

-- race_ :: IO a -> IO b -> IO ()
race_ left right = void $ race left right

-- concurrently :: IO a -> IO b -> IO (a,b)
concurrently left right = do
    mv <- newEmptyMVar
    rightTid <- myThreadId
    u <- newUnique
    mask $ \restore -> do
      leftTid <- forkIO $ catch (restore left >>= putMVar mv) $ \e ->
        case fromException e of
          Just (UniqueInterrupt u') | u == u' -> return ()
          _ -> throwTo rightTid $ UniqueInterruptWithSomeException u e
      catch (flip (,) <$> restore right <*> takeMVar mv) $ \e ->
          case fromException e of
            Just (UniqueInterruptWithSomeException u' ex) | u == u' -> throwIO ex
            _ -> do throwTo leftTid (UniqueInterrupt u)
                    throwIO e

data UniqueInterrupt = UniqueInterrupt Unique
                     deriving Typeable

instance Show UniqueInterrupt where
    show _ = "<< UniqueInterrupt >>"

instance Exception UniqueInterrupt where
#if MIN_VERSION_base(4,7,0)
    toException = asyncExceptionToException
    fromException = asyncExceptionFromException
#endif

data UniqueInterruptWithResult =
    forall a. UniqueInterruptWithResult Unique (Either SomeException a)
    deriving Typeable

instance Show UniqueInterruptWithResult where
    show _ = "<< UniqueInterruptWithResult >>"

instance Exception UniqueInterruptWithResult where
#if MIN_VERSION_base(4,7,0)
    toException = asyncExceptionToException
    fromException = asyncExceptionFromException
#endif

data UniqueInterruptWithSomeException =
    UniqueInterruptWithSomeException Unique SomeException
    deriving Typeable

instance Show UniqueInterruptWithSomeException where
    show _ = "<< UniqueInterruptWithSomeException >>"

instance Exception UniqueInterruptWithSomeException where
#if MIN_VERSION_base(4,7,0)
    toException = asyncExceptionToException
    fromException = asyncExceptionFromException
#endif
#endif

-- | maps an @IO@-performing function over any @Traversable@ data
-- type, performing all the @IO@ actions concurrently, and returning
-- the original data structure with the arguments replaced by the
-- results.
--
-- For example, @mapConcurrently@ works with lists:
--
-- > pages <- mapConcurrently getURL ["url1", "url2", "url3"]
--
mapConcurrently :: Traversable t => (a -> IO b) -> t a -> IO (t b)
mapConcurrently f = runConcurrently . traverse (Concurrently . f)

-- -----------------------------------------------------------------------------

-- | A value of type @Concurrently a@ is an @IO@ operation that can be
-- composed with other @Concurrently@ values, using the @Applicative@
-- and @Alternative@ instances.
--
-- Calling @runConcurrently@ on a value of type @Concurrently a@ will
-- execute the @IO@ operations it contains concurrently, before
-- delivering the result of type @a@.
--
-- For example
--
-- > (page1, page2, page3)
-- >     <- runConcurrently $ (,,)
-- >     <$> Concurrently (getURL "url1")
-- >     <*> Concurrently (getURL "url2")
-- >     <*> Concurrently (getURL "url3")
--
newtype Concurrently a = Concurrently { runConcurrently :: IO a }

instance Functor Concurrently where
  fmap f (Concurrently a) = Concurrently $ f <$> a

instance Applicative Concurrently where
  pure = Concurrently . return
  Concurrently fs <*> Concurrently as =
    Concurrently $ (\(f, a) -> f a) <$> concurrently fs as

instance Alternative Concurrently where
  empty = Concurrently $ forever (threadDelay maxBound)
  Concurrently as <|> Concurrently bs =
    Concurrently $ either id id <$> race as bs

instance Monad Concurrently where
  return = pure
  Concurrently a >>= f =
    Concurrently $ a >>= runConcurrently . f

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

tryAll :: IO a -> IO (Either SomeException a)
tryAll = try

-- A version of forkIO that does not include the outer exception
-- handler: saves a bit of time when we will be installing our own
-- exception handler.
{-# INLINE rawForkIO #-}
rawForkIO :: IO () -> IO ThreadId
rawForkIO action = IO $ \ s ->
   case (fork# action s) of (# s1, tid #) -> (# s1, ThreadId tid #)

{-# INLINE rawForkOn #-}
rawForkOn :: Int -> IO () -> IO ThreadId
rawForkOn (I# cpu) action = IO $ \ s ->
   case (forkOn# cpu action s) of (# s1, tid #) -> (# s1, ThreadId tid #)
