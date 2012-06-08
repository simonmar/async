{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}

module Control.Concurrent.Async (

    -- * Async API
    Async, async, withAsync, asyncThreadId, wait, waitThrow, cancel, cancelWith,
    -- ** STM API
    waitSTM, waitSTMThrow,
    -- ** Waiting for multiple asyncs
    waitAny, waitAnyThrow, waitAnyThrowCancel,
    waitEither, waitEitherThrow, waitEitherThrow_, waitEitherThrowCancel,
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
-- Fork that executes an action at the end

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

-- -----------------------------------------------------------------------------
-- STM Async API


-- | An asynchronous action spawned by 'async' or 'withAsync'.
-- Asynchronous actions are executed in a separate thread, and
-- operations are provided for waiting for asynchronous actions to
-- complete and obtaining their results (see e.g. 'wait').
--
data Async a = Async { asyncThreadId :: ThreadId
                     , _asyncVar     :: TMVar (Either SomeException a) }

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

-- | A version of 'wait' that throws the exception if the asynchronous
-- action raised one, or otherwise returns its result.
--
-- > waitThrow = atomically . waitSTMThrow
--
{-# INLINE waitThrow #-}
waitThrow :: Async a -> IO a
waitThrow = atomically . waitSTMThrow

-- | A version of 'wait' that can be used inside an STM transaction.
--
{-# INLINE waitSTM #-}
waitSTM :: Async a -> STM (Either SomeException a)
waitSTM (Async _ var) = readTMVar var

-- | A version of 'waitThrow' that can be used inside an STM transaction.
--
waitSTMThrow :: Async a -> STM a
waitSTMThrow (Async _ var) = do
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
--
waitAny :: [Async a] -> IO ()
waitAny asyncs =
  atomically $
    foldr orElse (return ()) $
      map (void . waitSTM) asyncs

-- | Wait for any of the supplied @Async@s to complete.  If the first
-- to complete raises an exception, then that exception is re-thrown
-- by 'waitAnyThrow'.
--
waitAnyThrow :: [Async a] -> IO ()
waitAnyThrow asyncs =
  atomically $
    foldr orElse (return ()) $
      map (void . waitSTMThrow) asyncs

-- | Like 'waitAnyThrow', but also cancels the other asynchronous
-- operations as soon as one has completed.
--
waitAnyThrowCancel :: [Async a] -> IO ()
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

-- | Wait for the first of two @Async@s to finish.  If the @Async@
-- that finished first raised an exception, then the exception is
-- re-thrown by 'waitEitherThrow'.
--
waitEitherThrow :: Async a -> Async b -> IO (Either a b)
waitEitherThrow left right =
  atomically $
    (Left  <$> waitSTMThrow left)
      `orElse`
    (Right <$> waitSTMThrow right)

-- | Like 'waitEitherThrow', but the results are ignored.
--
waitEitherThrow_ :: Async a -> Async b -> IO ()
waitEitherThrow_ left right =
  atomically $
    (void $ waitSTMThrow left)
      `orElse`
    (void $ waitSTMThrow right)

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
    a <- waitSTMThrow left
           `orElse`
         (waitSTMThrow right >> retry)
    b <- waitSTMThrow right
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
        let tids = [lid,rid]
        let stop threads = mapM_ killThread threads
        r <- restore (collect done) `onException` stop tids
        stop tids
        return r

#endif

-- ----------------------------------------------------------------------------

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch

tryAll :: IO a -> IO (Either SomeException a)
tryAll = try

-- A version of forkIO that does not include the outer exception
-- handler: saves a bit of time when we will be installing our own
-- exception handler.
rawForkIO :: IO () -> IO ThreadId
rawForkIO action = IO $ \ s ->
   case (fork# action s) of (# s1, tid #) -> (# s1, ThreadId tid #)
