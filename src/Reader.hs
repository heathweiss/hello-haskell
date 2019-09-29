{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Reader() where

import Control.Monad.Reader
import qualified Data.IORef as Ref

import Test.HUnit

runTest = do
  runTestTT test_showIO
  
  runTestTT test2
  runTestTT test2a
  runTestTT test2b
  runTestTT test2b2
  runTestTT test2c
  runTestTT test2d
  runTestTT test2e
  runTestTT test2f

  runTestTT test3
  runTestTT test3a
  runTestTT test3b
  runTestTT test3c
  runTestTT test3c
  runTestTT test3d
  runTestTT test3e
  runTestTT test3f

  runTestTT test4

--show how to test in the IO Monad
test_showIO = TestCase (do
                      let a :: IO (Int)
                          a = return 5
                          b :: IO (Int)
                          b = return 5
                      x <- a
                      y <- b
                      assertEqual "test 2a" x y
                      )
  
---------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------
{-test2<a...> series of tests.
Combines 2 different Reader fx's with >>= >> and do notation, in different ways.
-}

--call runReader (Int -> Reader Int Int >>= Int -> Reader Int Bool) Int which results in a Bool test.
--Both start and end have an Int param, in addition to being a Reader Int <Int/Bool>.
--start param is supplied before the >>=.
--end param is supplied by return value of start, which is passed on via the >>=
--start gets it's Reader env from the call to runReader via the env input to tryAsk
--Note how the output of start matches the input of end.
test2 = TestCase $ assertEqual
  "test2"
  (True)
  (let
     tryAsk :: Int -> Bool
     tryAsk env =

       let
         start :: Int -> Reader Int Int
         start int = return int
         end :: Int -> Reader Int Bool
         end int =
           return $ int == 6
       
       in 
       runReader (start 6 >>= end) env
       -- start gets a 6 because: start :: Int -> Reader Int Int. Test2a gets rid of it.
   in
   tryAsk 10
  )

--same as 2 but with different env.
--Note how both functions env types have to match.
test2a = TestCase $ assertEqual
  "test2a"
  (True)
  (let
     tryAsk :: Double -> Bool
     tryAsk env =

       let
         start :: Int -> Reader Double Int
         start int = return int
         end :: Int -> Reader Double Bool
         end int =
           return $ int == 6 
       
       in 
       runReader (start 6 >>= end) env
   in
   tryAsk 10.5
  )

--same as 2a but get rid of the input to start in order to simplify.
--Notice how start no longer gets a value supplied to it before the >>= 
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

--same as 2b but get rid of the input to end.
--Had to change the >>= to >> in order to throw away the output from start, otherwise would not compile.
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

--same as 2b but with Do notation
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

--same as 2c but with remove the input to 'end'
--This makes it like 2b2 where had to go from >>= to >>. In this case, just dropped the g in e <- end g.
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

--differs in that end is a Reader Int Int instead of Reader Int Bool
test2e = TestCase $ assertEqual
  "test2e"
  (6)
  (let
     tryAsk :: Int -> Int
     tryAsk env = --
       let
         start :: Int -> Reader Int Int
         start int = return int
         end :: Int -> Reader Int Int
         end int = return $  int + 1
       
       in 
       runReader (start 5 >>= end) env
   in
   tryAsk 10
  )

--Use the fact that the return value gets passed along through the >>= operator to create some state.
--Will have an increment fx which adds 1 to the input, and passes it along. Then 'end' will see if the state is == env.
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


------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------
{-test3 series
Manipulate an IORef.
Then use the IORef in an env field, to replicate state.
-}

{-Create 2 IORef Int and compare them for equality.
Note how needed tor use readIORef to pull their value out of IORef and into IO. -}
test3 = TestCase
  (do
     let
       a :: IO (Ref.IORef Int)
       a = Ref.newIORef 3
       b :: IO (Ref.IORef Int)
       b = Ref.newIORef 3
     a' <- a
     a'' <- Ref.readIORef a'
     b' <- b
     b'' <- Ref.readIORef b'
     assertEqual "test 3" a'' b''
  )

--same as test3, but get rid of the let statement.
--Note that have to pull the IORef InnerValue out of IO with the <- operator before using readIORef to get the inner value into IO.
test3a = TestCase
  (do
     let
       innerValue = 3
     a <- Ref.newIORef innerValue
     aRead <- Ref.readIORef a
     b <- Ref.newIORef innerValue
     bRead <- Ref.readIORef b
     assertEqual "test 3a" aRead bRead
  )

--increment an IORef Int, and see that it is incremented.
test3b = TestCase
  (do
     a <- Ref.newIORef 3
     Ref.modifyIORef a (+ 2)
     aIncrementedRead <- Ref.readIORef a
     assertEqual "test 3b" 5 aIncrementedRead
  )

--increment an IORef Int, and compare pre/post increment read values.
--Note how I had to (+ 2) to aRead to get the test to pass.
--a was incremented, but readIORef captures a snapshot of a, which is why aRead and aIncremented are not the same.
test3c = TestCase
  (do
     a <- Ref.newIORef 3
     aRead <- Ref.readIORef a
     Ref.modifyIORef a (+ 2)
     aIncrementedRead <- Ref.readIORef a
     assertEqual "test 3c" (aRead + 2) aIncrementedRead
  )



--Pass an IORef along via >>= to act like state..
--It is being passed along like state, but is an IORef.
--As per test3e is not required to be passed along, as it is always ref'g the same IORef
test3d = TestCase
  (do
     --A function that can be chained together with >>= so the input param gets passed along like state.
     let increment :: (Ref.IORef Int) -> (IO (Ref.IORef Int))
         increment env_ioRef = do
           Ref.modifyIORef env_ioRef (+ 2)
           return env_ioRef

     --initialize an IORef Int
     a <- Ref.newIORef 0
     --and get a reading from it.
     aRead <- Ref.readIORef a

     b <- increment a >>= increment
     bRead <- Ref.readIORef b
     assertEqual "test 3d" (aRead + 4) bRead 
  )

--This time, the IORef does not get passed along.
--It is only required as an input, as they all ref the same IORef.
test3e = TestCase
  (do
     a <- Ref.newIORef 0
     aRead <- Ref.readIORef a
     let increment :: (Ref.IORef Int) -> (IO())
         increment env_ioRef = do
           Ref.modifyIORef env_ioRef (+ 2)

     --increment 'a' by 6
     increment a >>  increment a >> increment a
     --take another reading.
     aRead' <- Ref.readIORef a
     assertEqual "test 3e" (aRead + 6) aRead' 
  )

--same as test3e, but showing how it can be done with do notation instead of >>
test3f = TestCase
  (do
     a <- Ref.newIORef 0
     aRead <- Ref.readIORef a
     let increment :: (Ref.IORef Int) -> (IO())
         increment env_ioRef = do
           Ref.modifyIORef env_ioRef (+ 2)
           
     increment a
     increment a 
     aRead' <- Ref.readIORef a
     assertEqual "test 3f" (aRead + 4) aRead' 
  )


---------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------
-- Use an IORef like state, but as the environment of the RIO type

--Should act similar to the RIO type from fpComplete RIO module.
type RIO = ReaderT (Ref.IORef Int) IO ()


test4 = TestCase
  (do
     a <- Ref.newIORef 0
     read_inital_a <- Ref.readIORef a
     let
         increment :: RIO
         increment = do
           a <- ask -- :: ReaderT (Ref.IORef Int) IO a0
           a' <- liftIO $ Ref.readIORef a
           liftIO $ Ref.modifyIORef a (+ 2)
           return ()
           
     runReaderT (increment) a
     runReaderT (increment) a
     read_inc'd_a' <- Ref.readIORef a
     assertEqual "test 4" (read_inital_a + 4) read_inc'd_a' 
  )



