{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{- |
Test out the Reader monad.
-}
module Reader(test2b, test2b2, test2c, test2d, test2f, test2g) where

import Control.Monad.Reader
import qualified Data.IORef as Ref

import Test.HUnit

runTests = do
  runTestTT test2b
  runTestTT test2b2
  runTestTT test2c
  runTestTT test2d
  runTestTT test2f
  runTestTT test2g

  


-- | Pass an Int though >>=, then ask for and add the environment to the Int.
test2b = TestCase $ assertEqual
  "test2b"
  (True)
  (let
     tryAsk ::  Int -> Bool
     tryAsk env =

       let
         start :: Reader Int Double
         start = return 3
         end :: Double -> Reader Int Bool
         end int = do
           i <- ask
           return $ i == 5 
       
       in
       runReader (start >>= end)  env
   in
   tryAsk 5
  )

-- | same as 2b but get rid of the input to end.
-- | Had to change the >>= to >> in order to throw away the output from start, otherwise would not compile.
test2b2 = TestCase $ assertEqual
  "test2b"
  (True)
  (let
     tryAsk ::  Int -> Bool
     tryAsk env =

       let
         start :: Reader Int Double
         start = return 3
         end :: Reader Int Bool
         end = do
           i <- ask
           return $ i == 5 
       
       in 
       runReader (start >> end)  env
   in
   tryAsk 5
  )

-- | Same as 2b but with Do notation
test2c = TestCase $ assertEqual
  "test2c"
  (True)
  (let
     tryAsk ::  Reader Int Bool
     tryAsk = do
       
       let
         start :: Reader Int Double
         start = return 3
         end :: Double -> Reader Int Bool
         end int = do
           i <- ask
           return $ i == 5 
       
       g <- start
       e <- end g
       return e
   in
   runReader tryAsk 5
  )

-- | Same as 2c but with remove the input to 'end'
-- | This makes it like 2b2 where had to go from >>= to >>. In this case, just dropped the g in e <- end g.
test2d = TestCase $ assertEqual
  "test2d"
  (True)
  (let
     tryAsk ::  Reader Int Bool
     tryAsk = do
       
       let
         start :: Reader Int Double
         start = return 3
         end :: Reader Int Bool
         end = do
           i <- ask
           return $ i == 5 
       
       g <- start
       e <- end 
       return e
   in
   runReader tryAsk 5
  )

-- | Use the fact that the return value gets passed along through the >>= operator to create some state.
-- | Will have an increment fx which adds 1 to the input, and passes it along. Then 'end' will see if the state is == env.
test2f = TestCase $ assertEqual
  "test2e"
  (True)
  (let
     tryAsk :: Int -> Bool
     tryAsk env = --
       let
         start :: Int -> Reader Int Int
         start int = return int
         increment :: Int -> Reader Int Int
         increment int = return $ int + 1
         isEqual :: Int -> Reader Int Bool
         isEqual int = do
           env <- ask
           return $  int == env
       
       in 
       runReader (start 1 >>= increment >>= increment >>= increment >>= isEqual) env
   in
   tryAsk 4
  )




type RIO = ReaderT (Ref.IORef Int) IO ()

-- | Access the inner value of an IORef that is a ReaderT (IORef Int) IO () environment.
-- | This is RIO where env == Int
test2g = TestCase
  (do
     a <- Ref.newIORef 0
     aRead <- Ref.readIORef a
     let
         accessTheIORefValue :: RIO
         accessTheIORefValue = do
           a <- ask -- :: ReaderT (Ref.IORef Int) IO a0
           a' <- liftIO $ Ref.readIORef a
           return ()
           
     
     runReaderT (accessTheIORefValue) a
     aRead_accessed <- Ref.readIORef a
     assertEqual "test2g" (aRead) aRead_accessed
  )



