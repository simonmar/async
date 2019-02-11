{-# LANGUAGE ScopedTypeVariables,DeriveDataTypeable #-}
module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Exception
import Data.IORef
import Data.Typeable
import Control.Concurrent
import Control.Monad
import Data.List (sort)
import Data.Maybe

import Prelude hiding (catch)

main = defaultMain tests

tests = [
    testCase "async_wait"        async_wait
  , testCase "async_waitCatch"   async_waitCatch
  , testCase "async_exwait"      async_exwait
  , testCase "async_exwaitCatch" async_exwaitCatch
  , testCase "withasync_waitCatch" withasync_waitCatch
  , testCase "withasync_wait2"   withasync_wait2
  , testGroup "async_cancel_rep" $
      replicate 1000 $
         testCase "async_cancel"       async_cancel
  , testCase "async_cancelmany" async_cancelmany
  , testCase "async_poll"        async_poll
  , testCase "async_poll2"       async_poll2
  , testCase "withasync_waitCatch_blocked" withasync_waitCatch_blocked
  , testCase "withasync_wait_blocked" withasync_wait_blocked
  , testGroup "children surviving too long"
      [ testCase "concurrently+success" concurrently_success
      , testCase "concurrently+failure" concurrently_failure
      , testCase "race+success" race_success
      , testCase "race+failure" race_failure
      , testCase "cancel" cancel_survive
      , testCase "withAsync" withasync_survive
      ]
  , testCase "concurrently_" case_concurrently_
  , testCase "replicateConcurrently_" case_replicateConcurrently
  , testCase "replicateConcurrently" case_replicateConcurrently_
  , testCase "link1" case_link1
  , testCase "link2" case_link2
  , testCase "link1_cancel" case_link1cancel
  , testCase "concurrently_deadlock" case_concurrently_deadlock
 ]

value = 42 :: Int

data TestException = TestException deriving (Eq,Show,Typeable)
instance Exception TestException

async_waitCatch :: Assertion
async_waitCatch = do
  a <- async (return value)
  r <- waitCatch a
  case r of
    Left _  -> assertFailure ""
    Right e -> e @?= value

async_wait :: Assertion
async_wait = do
  a <- async (return value)
  r <- wait a
  assertEqual "async_wait" r value

async_exwaitCatch :: Assertion
async_exwaitCatch = do
  a <- async (throwIO TestException)
  r <- waitCatch a
  case r of
    Left e  -> fromException e @?= Just TestException
    Right _ -> assertFailure ""

async_exwait :: Assertion
async_exwait = do
  a <- async (throwIO TestException)
  (wait a >> assertFailure "") `catch` \e -> e @?= TestException

withasync_waitCatch :: Assertion
withasync_waitCatch = do
  withAsync (return value) $ \a -> do
    r <- waitCatch a
    case r of
      Left _  -> assertFailure ""
      Right e -> e @?= value

withasync_wait2 :: Assertion
withasync_wait2 = do
  a <- withAsync (threadDelay 1000000) $ return
  r <- waitCatch a
  case r of
    Left e  -> fromException e @?= Just AsyncCancelled
    Right _ -> assertFailure ""

async_cancel :: Assertion
async_cancel = do
  a <- async (return value)
  cancelWith a TestException
  r <- waitCatch a
  case r of
    Left e -> fromException e @?= Just TestException
    Right r -> r @?= value

async_cancelmany :: Assertion -- issue 59
async_cancelmany = do
  r <- newIORef []
  a <- async $ forConcurrently_ ['a'..'z'] $ \c ->
    delay 2 `finally` atomicModifyIORef r (\i -> (c:i,()))
  delay 1
  cancel a
  v <- readIORef r
  assertEqual "cancelmany" 26 (length v)
  where
    delay sec = threadDelay (sec * 1000000)

async_poll :: Assertion
async_poll = do
  a <- async (threadDelay 1000000)
  r <- poll a
  when (isJust r) $ assertFailure ""
  r <- poll a   -- poll twice, just to check we don't deadlock
  when (isJust r) $ assertFailure ""

async_poll2 :: Assertion
async_poll2 = do
  a <- async (return value)
  wait a
  r <- poll a
  when (isNothing r) $ assertFailure ""
  r <- poll a   -- poll twice, just to check we don't deadlock
  when (isNothing r) $ assertFailure ""

withasync_waitCatch_blocked :: Assertion
withasync_waitCatch_blocked = do
  r <- withAsync (newEmptyMVar >>= takeMVar) waitCatch
  case r of
    Left e ->
        case fromException e of
            Just BlockedIndefinitelyOnMVar -> return ()
            Nothing -> assertFailure $ show e
    Right () -> assertFailure ""

withasync_wait_blocked :: Assertion
withasync_wait_blocked = do
  r <- try $ withAsync (newEmptyMVar >>= takeMVar) wait
  case r of
    Left e ->
        case fromException e of
            Just BlockedIndefinitelyOnMVar -> return ()
            Nothing -> assertFailure $ show e
    Right () -> assertFailure ""

concurrently_success :: Assertion
concurrently_success = do
  finalRes <- newIORef "never filled"
  baton <- newEmptyMVar
  let quick = return ()
      slow = threadDelay 10000 `finally` do
        threadDelay 10000
        writeIORef finalRes "slow"
        putMVar baton ()
  _ <- concurrently quick slow
  writeIORef finalRes "parent"
  takeMVar baton
  res <- readIORef finalRes
  res @?= "parent"

concurrently_failure :: Assertion
concurrently_failure = do
  finalRes <- newIORef "never filled"
  let quick = error "a quick death"
      slow = threadDelay 10000 `finally` do
        threadDelay 10000
        writeIORef finalRes "slow"
  _ :: Either SomeException ((), ()) <- try (concurrently quick slow)
  writeIORef finalRes "parent"
  threadDelay 1000000 -- not using the baton, can lead to deadlock detection
  res <- readIORef finalRes
  res @?= "parent"

race_success :: Assertion
race_success = do
  finalRes <- newIORef "never filled"
  let quick = return ()
      slow = threadDelay 10000 `finally` do
        threadDelay 10000
        writeIORef finalRes "slow"
  race_ quick slow
  writeIORef finalRes "parent"
  threadDelay 1000000 -- not using the baton, can lead to deadlock detection
  res <- readIORef finalRes
  res @?= "parent"

race_failure :: Assertion
race_failure = do
  finalRes <- newIORef "never filled"
  baton <- newEmptyMVar
  let quick = error "a quick death"
      slow restore = restore (threadDelay 10000) `finally` do
        threadDelay 10000
        writeIORef finalRes "slow"
        putMVar baton ()
  _ :: Either SomeException () <-
    try $ mask $ \restore ->
       race_ quick (slow restore)
  writeIORef finalRes "parent"
  takeMVar baton
  res <- readIORef finalRes
  res @?= "parent"

cancel_survive :: Assertion
cancel_survive = do
  finalRes <- newIORef "never filled"
  a <- async $ threadDelay 10000 `finally` do
        threadDelay 10000
        writeIORef finalRes "child"
  cancel a
  writeIORef finalRes "parent"
  threadDelay 1000000 -- not using the baton, can lead to deadlock detection
  res <- readIORef finalRes
  res @?= "parent"

withasync_survive :: Assertion
withasync_survive = do
  finalRes <- newIORef "never filled"
  let child = threadDelay 10000 `finally` do
        threadDelay 10000
        writeIORef finalRes "child"
  withAsync child (\_ -> return ())
  writeIORef finalRes "parent"
  threadDelay 1000000 -- not using the baton, can lead to deadlock detection
  res <- readIORef finalRes
  res @?= "parent"

case_concurrently_ :: Assertion
case_concurrently_ = do
  ref <- newIORef 0
  () <- concurrently_
    (atomicModifyIORef ref (\x -> (x + 1, True)))
    (atomicModifyIORef ref (\x -> (x + 2, 'x')))
  res <- readIORef ref
  res @?= 3

case_replicateConcurrently :: Assertion
case_replicateConcurrently = do
  ref <- newIORef 0
  let action = atomicModifyIORef ref (\x -> (x + 1, x + 1))
  resList <- replicateConcurrently 100 action
  resVal <- readIORef ref
  resVal @?= 100
  sort resList @?= [1..100]

case_replicateConcurrently_ :: Assertion
case_replicateConcurrently_ = do
  ref <- newIORef 0
  let action = atomicModifyIORef ref (\x -> (x + 1, x + 1))
  () <- replicateConcurrently_ 100 action
  resVal <- readIORef ref
  resVal @?= 100

case_link1 :: Assertion
case_link1 = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  let ex = ErrorCall "oops"
  a <- async $ do takeMVar m1; throwIO ex; putMVar m2 ()
  link a
  e <- try $ (do
    putMVar m1 ()
    takeMVar m2)
  assertBool "link1" $
    case e of
      Left (ExceptionInLinkedThread a' e') ->
        compareAsyncs a' a == EQ &&
          case fromException e' of
            Just (ErrorCall s) -> s == "oops"
            _otherwise -> False
      _other -> False

case_link2 :: Assertion
case_link2 = do
  let
    setup = do
      m1 <- newEmptyMVar
      m2 <- newEmptyMVar
      let ex1 = ErrorCall "oops1"; ex2 = ErrorCall "oops2"
      a <- async $ do takeMVar m1; throwIO ex1
      b <- async $ do takeMVar m2; throwIO ex2
      link2 a b
      return (m1,m2,a,b)

  (m1,m2,a,b) <- setup
  e <- try $ do
    putMVar m1 ()
    wait b
  putMVar m2 ()  -- ensure the other thread is not deadlocked
  assertBool "link2a" $
    case e of
      Left (ExceptionInLinkedThread a' e') ->
        compareAsyncs a' a == EQ &&
          case fromException e' of
            Just (ErrorCall s) -> s == "oops1"
            _otherwise -> False
      _other -> False

  (m1,m2,a,b) <- setup
  e <- try $ do
    putMVar m2 ()
    wait a
  putMVar m1 ()  -- ensure the other thread is not deadlocked
  assertBool "link2b" $
    case e of
      Left (ExceptionInLinkedThread a' e') ->
        compareAsyncs a' b == EQ &&
          case fromException e' of
            Just (ErrorCall s) -> s == "oops2"
            _otherwise -> False
      _other -> False

case_link1cancel :: Assertion
case_link1cancel = do
  m1 <- newEmptyMVar
  let ex = ErrorCall "oops"
  a <- async $ do takeMVar m1
  link a
  e <- try $ do cancel a; wait a
  putMVar m1 ()
  assertBool "link1cancel" $
    case e of
      Left AsyncCancelled -> True  -- should not be ExceptionInLinkedThread
      _other -> False

-- See Issue #62
case_concurrently_deadlock :: Assertion
case_concurrently_deadlock = do
  tvar <- newTVarIO False :: IO (TVar Bool)
  e <- try $ void $ join (concurrently) (atomically $ readTVar tvar >>= check)
    -- should throw BlockedIndefinitelyOnSTM not BlockedIndefinitelyOnMVar
  assertBool "concurrently_deadlock" $
    case e of
      Left BlockedIndefinitelyOnSTM{} -> True
      _other -> False
