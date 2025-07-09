{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Processing streams with a fixed number of worker threads
module Control.Concurrent.Stream
  ( stream
  , streamBound
  , streamWithInput
  , streamWithOutput
  , streamWithInputOutput
  , mapConcurrentlyBounded
  , forConcurrentlyBounded
  ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.IORef

data ShouldBindThreads = BoundThreads | UnboundThreads

-- | Maps a fixed number of workers concurrently over a stream of values
-- produced by a producer function. The producer is passed a function to
-- call for each work item. If a worker throws a synchronous exception, it
-- will be propagated to the caller.
stream
  :: Int -- ^ Maximum Concurrency
  -> ((a -> IO ()) -> IO ()) -- ^ Producer
  -> (a -> IO ()) -- ^ Worker
  -> IO ()
stream maxConcurrency producer worker =
  streamWithInput producer (replicate maxConcurrency ()) $ const worker

-- | Like stream, but uses bound threads for the workers.  See
-- 'Control.Concurrent.forkOS' for details on bound threads.
streamBound
  :: Int -- ^ Maximum Concurrency
  -> ((a -> IO ()) -> IO ()) -- ^ Producer
  -> (a -> IO ()) -- ^ Worker
  -> IO ()
streamBound maxConcurrency producer worker =
  stream_ BoundThreads producer (replicate maxConcurrency ()) $ const worker

-- | Like stream, but each worker is passed an element of an input list.
streamWithInput
  :: ((a -> IO ()) -> IO ()) -- ^ Producer
  -> [b] -- ^ Worker state
  -> (b -> a -> IO ()) -- ^ Worker
  -> IO ()
streamWithInput = stream_ UnboundThreads

-- | Like 'stream', but collects the results of each worker
streamWithOutput
  :: Int 
  -> ((a -> IO ()) -> IO ()) -- ^ Producer
  -> (a -> IO c) -- ^ Worker
  -> IO [c]
streamWithOutput maxConcurrency producer worker =
  streamWithInputOutput producer (replicate maxConcurrency ()) $ 
    const worker

-- | Like 'streamWithInput', but collects the results of each worker
streamWithInputOutput
  :: ((a -> IO ()) -> IO ()) -- ^ Producer
  -> [b] -- ^ Worker input
  -> (b -> a -> IO c) -- ^ Worker
  -> IO [c]
streamWithInputOutput producer workerInput worker = do
  results <- newIORef []
  let prod write = producer $ \a -> do
        res <- newIORef Nothing
        modifyIORef results (res :)
        write (a, res)
  stream_ UnboundThreads prod workerInput $ \s (a,ref) -> do
    worker s a >>= writeIORef ref . Just
  readIORef results >>= mapM readIORef >>= return . catMaybes . reverse
    
stream_
  :: ShouldBindThreads -- use bound threads?
  -> ((a -> IO ()) -> IO ()) -- ^ Producer
  -> [b] -- Worker input
  -> (b -> a -> IO ()) -- ^ Worker
  -> IO ()
stream_ useBoundThreads producer workerInput worker = do
  let maxConcurrency = length workerInput
  q <- atomically $ newTBQueue (fromIntegral maxConcurrency)
  let write x = atomically $ writeTBQueue q (Just x)
  mask $ \unmask ->
    concurrently_ (runWorkers unmask q) $ unmask $ do
      -- run the producer
      producer write
      -- write end-markers for all workers
      replicateM_ maxConcurrency $
        atomically $ writeTBQueue q Nothing
  where
    runWorkers unmask q = case useBoundThreads of
      BoundThreads ->
        foldr1 concurrentlyBound $
          map (runWorker unmask q) workerInput
      UnboundThreads ->
        mapConcurrently_ (runWorker unmask q) workerInput

    concurrentlyBound l r =
      withAsyncBound l $ \a ->
      withAsyncBound r $ \b ->
      void $ waitBoth a b

    runWorker unmask q s = do
      v <- atomically $ readTBQueue q
      case v of
        Nothing -> return ()
        Just t -> do
          unmask (worker s t)
          runWorker unmask q s

-- | Concurrent map over a list of values, using a bounded number of threads.
mapConcurrentlyBounded
  :: Int -- ^ Maximum concurrency
  -> (a -> IO b) -- ^ Function to map over the input values
  -> [a] -- ^ List of input values
  -> IO [b] -- ^ List of output values
mapConcurrentlyBounded maxConcurrency f input =
  streamWithOutput maxConcurrency (forM_ input) f
  
-- | 'mapConcurrentlyBounded' but with its arguments reversed
forConcurrentlyBounded
  :: Int -- ^ Maximum concurrency
  -> [a] -- ^ List of input values
  -> (a -> IO b) -- ^ Function to map over the input values
  -> IO [b] -- ^ List of output values
forConcurrentlyBounded = flip . mapConcurrentlyBounded
