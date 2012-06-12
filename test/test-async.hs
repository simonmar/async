{-# LANGUAGE ScopedTypeVariables,DeriveDataTypeable #-}
module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Control.Concurrent.Async
import Control.Exception
import Data.Typeable
import Control.Concurrent
import Control.Monad
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
  , testCase "async_poll"        async_poll
  , testCase "async_poll2"       async_poll2
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
    Left e  -> fromException e @?= Just ThreadKilled
    Right _ -> assertFailure ""

async_cancel :: Assertion
async_cancel = do
  a <- async (return value)
  cancelWith a TestException
  r <- waitCatch a
  case r of
    Left e -> fromException e @?= Just TestException
    Right r -> r @?= value

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
