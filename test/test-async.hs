{-# LANGUAGE ScopedTypeVariables,DeriveDataTypeable #-}
module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Control.Concurrent.Async
import Control.Exception
import Data.Typeable
import Control.Concurrent

import Prelude hiding (catch)

main = defaultMain tests

tests = [
    testCase "async_wait"        async_wait
  , testCase "async_waitThrow"   async_waitThrow
  , testCase "async_exwait"      async_exwait
  , testCase "async_exwaitThrow" async_exwaitThrow
  , testCase "withasync_wait"    withasync_wait
  , testCase "withasync_wait2"   withasync_wait2
  , testGroup "async_cancel_rep" $
      replicate 1000 $
         testCase "async_cancel"       async_cancel
 ]

value = 42 :: Int

data TestException = TestException deriving (Eq,Show,Typeable)
instance Exception TestException

async_wait :: Assertion
async_wait = do
  a <- async (return value)
  r <- wait a
  case r of
    Left _  -> assertFailure ""
    Right e -> e @?= value

async_waitThrow :: Assertion
async_waitThrow = do
  a <- async (return value)
  r <- waitThrow a
  assertEqual "async_waitThrow" r value

async_exwait :: Assertion
async_exwait = do
  a <- async (throwIO TestException)
  r <- wait a
  case r of
    Left e  -> fromException e @?= Just TestException
    Right _ -> assertFailure ""

async_exwaitThrow :: Assertion
async_exwaitThrow = do
  a <- async (throwIO TestException)
  (waitThrow a >> assertFailure "") `catch` \e -> e @?= TestException

withasync_wait :: Assertion
withasync_wait = do
  withAsync (return value) $ \a -> do
    r <- wait a
    case r of
      Left _  -> assertFailure ""
      Right e -> e @?= value

withasync_wait2 :: Assertion
withasync_wait2 = do
  a <- withAsync (threadDelay 1000000) $ return
  r <- wait a
  case r of
    Left e  -> fromException e @?= Just ThreadKilled
    Right _ -> assertFailure ""

async_cancel :: Assertion
async_cancel = do
  a <- async (return value)
  cancelWith a TestException
  r <- wait a
  case r of
    Left e -> fromException e @?= Just TestException
    Right r -> r @?= value
