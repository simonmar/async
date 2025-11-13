{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A more flexible way to create 'Async's and have them automatically
-- cancelled when the 'Warden' is shut down.
module Control.Concurrent.Async.Warden
  ( Warden
  , withWarden
  , create
  , shutdown
  , spawn
  , spawn_
  , spawnMask
  , WardenException(..)
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Exception
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import System.IO (fixIO)

#if defined(__MHS__)
import Prelude hiding(mapM_)
import Control.Monad hiding(mapM_)
import Data.Foldable(mapM_)
#else
import Control.Monad
#endif

-- | A 'Warden' is an owner of 'Async's which cancels them on 'shutdown'.
--
-- 'Nothing' in the MVar means the 'Warden' has been shut down.
newtype Warden = Warden (MVar (Maybe (HashSet (Async ()))))

-- | Run the action with a new 'Warden', and call 'shutdown' when the action
-- exits.
withWarden :: (Warden -> IO a) -> IO a
withWarden = bracket create shutdown

-- | Create a new 'Warden'.
create :: IO Warden
create = Warden <$> newMVar (Just mempty)

-- | Shutdown a 'Warden', calling 'cancel' on all owned threads. Subsequent
-- calls to 'spawn' and 'shutdown' will be no-ops. 
-- 
-- Note that any exceptions thrown by the threads will be ignored. If you want
-- exceptions to be propagated, either call `wait` explicitly on the 'Async', 
-- or use 'link'.
shutdown :: Warden -> IO ()
shutdown (Warden v) = do
  r <- swapMVar v Nothing
  mapM_ (Async.mapConcurrently_ Async.cancel) r

forget :: Warden -> Async a -> IO ()
forget (Warden v) async = modifyMVar_ v $ \x -> case x of
  Just xs -> return $! Just $! HashSet.delete (void async) xs
  Nothing -> return Nothing

-- | Spawn a thread with masked exceptions and pass an unmask function to the
-- action.
spawnMask :: Warden -> ((forall b. IO b -> IO b) -> IO a) -> IO (Async a)
spawnMask (Warden v) action = modifyMVar v $ \r -> case r of
  Just asyncs -> do
    -- Create a new thread which removes itself from the 'HashSet' when it
    -- exits.
    this <- fixIO $ \this -> mask_ $ Async.asyncWithUnmask $ \unmask ->
      action unmask `finally` forget (Warden v) this
    return (Just $ HashSet.insert (void this) asyncs, this)
  Nothing -> throwIO $ WardenException "Warden has been shut down"

newtype WardenException = WardenException String
  deriving (Show)

instance Exception WardenException

-- | Spawn a new thread owned by the 'Warden'.
spawn :: Warden -> IO a -> IO (Async a)
spawn warden action = spawnMask warden $ \unmask -> unmask action

-- | Spawn a new thread owned by the 'Warden'.
spawn_ :: Warden -> IO () -> IO ()
spawn_ w = void . spawn w
