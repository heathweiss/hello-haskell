{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{- |
Test out IORef module
-}
module IORef(test1, test1b, test1c, test1d) where

import Data.IORef
import Control.Monad.Reader

import Test.HUnit

runTests = do
  runTestTT test1
  runTestTT test1b
  runTestTT test1c
  runTestTT test1d
  runTestTT test1e

{- | Create 2 IORef Int and compare them for equality.

-}
test1 = TestCase
  (do
     a' <- newIORef 3
     -- :: IORef Int <- (a -> IO (IORef a)) -> Int -> IO (IORef Int)
     a'' <- readIORef a'
     -- :: Int <- (IORef a -> IO a) -> IORef Int -> IO Int

     b' <- newIORef 3
     -- same as a
     b'' <- readIORef b'
     -- same as a'

     -- IORef(a) is not an intstance Eq, so have to extract the Int using readIORef, to use with assertEqual
     assertEqual "test1" a'' b''
  )


-- | Increment an IORef Int.
test1b = TestCase
  (do
     a <- newIORef 3
     -- :: IORef Int <- (a -> IO (IORef a)) -> Int -> IO (IORef Int)
     modifyIORef a (+ 2)
     -- :: IORef a -> (a -> a) -> IO () 
     aIncrementedRead <- readIORef a
     -- :: Int <- (IORef a -> IO a) -> IORef Int -> IO Int
     assertEqual "test1B" 5 aIncrementedRead
  )

{- |
Create an IORef Int and read the IORef.
Increment the IORef, read the IORef again.
Compare the 2 read values to show that the first read value is still the original IORef value.
-}
test1c = TestCase
  (do
     a <- newIORef 3
     -- :: IORef Int <- (a -> IO (IORef a)) -> Int -> IO (IORef Int)
     aOriginal <- readIORef a
     -- :: Int <- (IORef a -> IO a) -> IORef Int -> IO Int
     modifyIORef a (+ 2)
     -- :: IORef a -> (a -> a) -> IO () 
     aIncremented <- readIORef a
     -- :: Int <- (IORef a -> IO a) -> IORef Int -> IO Int

     --Had to (+ 2) the aOriginal to get the test to pass.
     assertEqual "test1c" (aOriginal + 2) aIncremented
  )


{- |
Modify an IORef 3 times as incrment fx is applied 3 times to the >> operator.
It is only required as an input, as they all ref the same IORef, and does not have to be passed in as a parameter.
See the Reader module for doing the same thing, with ask.
-}
test1d = TestCase
  (do
     a <- newIORef 0
     -- :: IORef Int <- (a -> IO (IORef a)) -> Int -> IO (IORef Int)
     aRead <- readIORef a
     -- :: Int <- (IORef a -> IO a) -> IORef Int -> IO Int
     let increment :: (IORef Int) -> (IO())
         increment env_ioRef = do
           modifyIORef env_ioRef (+ 2)

     --increment 'a' 3 times
     increment a >>  increment a >> increment a
     {-Could have done with do notation.
     increment a
     increment a
     increment a
     -}
     
     --take another reading.
     aIncremented <- readIORef a
     --same as aRead
     
     assertEqual "test1d" (aRead + 6) aIncremented
  )

--Should act similar to the RIO type from fpComplete RIO module.
type RIO = ReaderT (IORef Int) IO ()

{- |
Use an IORef as the environment of a ReaderT (IORef Int) IO ().
Increment the IORef twice with a fx :: RIO.
The result is a mutable Reader environment that acts like state.
-}
test1e = TestCase
  (do
     a <- newIORef 0
     -- :: IORef Int <- (a -> IO (IORef a)) -> Int -> IO (IORef Int)
     aRead <- readIORef a
     -- :: Int <- (IORef a -> IO a) -> IORef Int -> IO Int
     let
         increment :: RIO
         increment = do
           b <- ask
           -- IORef Int <- IO (IORef Int)
           

           --adds nothing to the process, but it here to show how to read.
           bRead <- liftIO $ readIORef b
           -- :: Int <- (IO a -> m a) -> ((IORef a -> IO a) -> IORef Int -> IO Int)

           liftIO $ modifyIORef b (+ 2)
           -- :: () <- (IO a -> m a) -> (IORef a -> (a -> a) -> IO () )
           
     runReaderT (increment) a
     runReaderT (increment) a
     aRead_inc <- readIORef a
     assertEqual "test1e" (aRead + 4) aRead_inc
  )

{-
test1e = TestCase
  (do
     a <- newIORef 0
     -- :: IORef Int <- (a -> IO (IORef a)) -> Int -> IO (IORef Int)
     aRead <- readIORef a
     -- :: Int <- (IORef a -> IO a) -> IORef Int -> IO Int
     let
         increment :: RIO
         increment = do
           b <- ask
           -- :: ReaderT (IORef Int) IO a0
           bRead <- liftIO $ readIORef b
           -- :: Int <- (IORef a -> IO a) -> IORef Int -> IO Int
           
           x <- liftIO $ modifyIORef bRead (+ 2)
           -- :: ? <- IORef a -> (a -> a) -> IO ()
           
           return ()
           --return bRead
           
     runReaderT (increment) a
     runReaderT (increment) a
     aRead_inc <- readIORef a
     assertEqual "test1e" (aRead + 4) aRead_inc
  )

-}
