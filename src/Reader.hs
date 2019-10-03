{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{- |
Test out the Reader monad.
-}
module Reader(test2b, test2c, test2d, test2f, test2g, test2h, test2i) where

import Control.Monad.Reader
import qualified Data.IORef as Ref

import Test.HUnit

runTests = do
  runTestTT test2b
  runTestTT test2c
  runTestTT test2d
  runTestTT test2f
  runTestTT test2g
  runTestTT test2h
  runTestTT test2i


-- | Pass an Int though >>=, then change return type to a bool by comparing it to the Reader env.
-- | Use >>= notations
test2b = TestCase $ assertEqual
  "test2b"
  (True)
  (let
     
         start :: Reader Int Double
         start = return 3
         --The input to 'end' has to match the return val of 'start' to align the values through >>=
         end :: Double -> Reader Int Bool
         end int = do
           i <- ask
           return $ i == 5 
       
   in
       runReader (start >>= end)  5
  )


-- | Pass an Int though >>=, then change return type to a bool by comparing it to the Reader env.
-- | Use Do notation
test2c = TestCase $ assertEqual
  "test2c"
  (True)
  (let
     start :: Reader Int Double
     start = return 3
     end :: Double -> Reader Int Bool
     end int = do
       i <- ask
       return $ i == 5 
   in
   runReader (do
                 g <- start
                 e <- end g
                 return e)
    5 --passed in Reader env
  )



-- | Use 'ask' to access the Reader env.
test2d = TestCase $ assertEqual
  "test2d"
  (True)
  (let
         end :: Reader Int Bool
         end = do
           i <- ask
           return $ i == 5 
       
       
   in
   runReader (end) 5
  )


-- | Use the Reader return value, passed through the >>= operator, to create some state.
test2f = TestCase $ assertEqual
  "test2e"
  (True)
  (let
      
     start :: Reader Int Int
     start = return 1
     increment :: Int -> Reader Int Int
     increment int = return $ int + 1
     isEqual :: Int -> Reader Int Bool
     isEqual int = do
       return $  int == 4
     ignoredEnv = 44  
   in
   --'start' needs a starter input, and the rest get it from return value of prev. fx.
   runReader (start  >>= increment >>= increment >>= increment >>= isEqual) ignoredEnv
   
  )


type RIO = ReaderT (Ref.IORef Int) IO ()

-- | Access an IORef that is a ReaderT (IORef Int) IO () environment.
-- | See the hello-haskell IORef module for more tests.
test2g = TestCase
  (do
     --This is done in IO, so no need for lifting.
     a <- Ref.newIORef 2
     aRead <- Ref.readIORef        a
 -- :: Int <- (IORef a -> IO a) -> IORef Int -> IO Int
     let
         --Gets at the underlying Int of the IORef, and uses that value to modify the IOREf.
         --Note that readIORef now needs lifting into the ReaderT monad.
         accessTheIORefValue :: RIO
         accessTheIORefValue = do
              a <- ask
   -- IORef Int <- ReaderT (IORef Int) IO ()
              a' <- liftIO $          Ref.readIORef        a
       -- :: Int <- (IO a -> m a) ->  (IORef a -> IO a) -> IORef Int -> IO Int
              liftIO $ Ref.modifyIORef a (a' * ) --multiply by itself. So (* 2)
              return ()
           
     --access the value. Does it work with IO because the final value in RIO is IO?
     runReaderT (accessTheIORefValue) a
     
     aRead_accessed <- Ref.readIORef a
     assertEqual "test2g" (aRead * 2) aRead_accessed
  )

-- | Use 'local' on a Reader Int Int
test2h = TestCase $ assertEqual
  "test2h"
  (2)
  (let
     --ask for the env, which will have been modified by 'local'
     start :: Reader Int Int
     start = ask
       
   in
   
   runReader (local (* 2) start  ) 1
   
  )


-- | Show that local is just a convenience wrapper for modifying Reader env.
test2i = TestCase $ assertEqual
  "test2i"
  ((2,2))
  (let
     --ask for the env, which will have been modified by 'local'
     start :: Reader Int Int
     start = ask
     modifyEnv = \env -> env * 2   
   in
   --Both calls of this tuple will modfy the env with (* 2)
   -- modfy env with local (* 2)        modify env prior to passing into Reader
   (runReader (local (* 2) start  ) 1,  runReader start (modifyEnv 1))
  )

  
