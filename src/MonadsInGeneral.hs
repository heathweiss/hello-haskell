{- |
Some general tests that hold for all monads.

Examples:

Passing return values through >>= or <- operators.

Comparing >>= and do notation.
-}
module MonadsInGeneral(testPT1, testPT2, testPT3, testPT4, testPT5, testPT6

                      ) where
import Test.HUnit
import Control.Monad.Reader

runTests = do
  runTestTT testPT1
  runTestTT testPT2
  runTestTT testPT3
  runTestTT testPT4
  runTestTT testPT5
  runTestTT testPT5
  runTestTT testPT6
  runTestTT testPT7


  

-- | Pass the return value through a Reader monad without any modifications to type, using >>=
-- | Does'nt use the Reader env.
testPT1 = TestCase $ assertEqual
  "testPT1"
  (2)
  (let
     start :: Int -> Reader Int Int
     start int = return int
     end :: Int -> Reader Int Int
     end int = return $ int + 2
       
   in 
       runReader (start 0 >>= end) 10
       -- note that the env never gets used.
  )

-- | Pass the return value through a IO monad without any modifications to value or type, using do notation.
testPT2 = TestCase
  (do
     let
       start :: Int -> IO (Int)
       start int = return int
       end :: Int -> IO (Int)
       end int = return int
     s <- start 1 -- :: IO (Int) -> Int
     e <- end s   -- :: IO (Int) -> Int
     assertEqual "testPT2" 1 e
  )

-- | Pass the return value through a IO monad without any modifications to value or type, using >>=
testPT3 = TestCase
  (do
     let
       start :: Int -> IO (Int)
       start int = return int
       end :: Int -> IO (Int)
       end int = return int

     --pull out of the IO monad with <- so it can be put into assertEqual
     -- ::      (Int -> IO (Int)) -> (Int -> IO (Int)) -> Int
     result  <- start 0           >>= end 
     assertEqual "testPT3" 0 result
  )

-- | Pass the return value through a IO monad while changing it's type, using do notation.
testPT4 = TestCase
  (do
     let
       start :: Int -> IO (Int)
       start int = return int
       end :: Int -> IO (Bool)
       end int = return True
     -- :: (Int -> IO (Int)) -> Int
     s <-  start 0
     -- :: Int -> IO (Bool) -> Bool
     e <-  end s   
     assertEqual "testPT4" True e
  )

-- | Pass the return value through a IO monad while changing it's type, using >>=
testPT5 = TestCase
  (do
     let
       start :: Int -> IO (Int)
       start int = return int
       end :: Int -> IO (Bool)
       end int = return True
     -- ::      (Int -> IO (Int)) ->  Int -> IO (Bool) -> Int
     result <-  start 0           >>= end           -- <-
     assertEqual "testPT5" True result
  )

-- | Pass the return value through a Maybe monad while without changing it's type, using >>=
testPT6 = TestCase $ assertEqual
  "testPT6"
  (Just 2)
  (do
     let
       start :: Int -> Maybe Int
       start int = return int
       end :: Int -> Maybe Int
       end int = return $ int + 1
     start 1 >>= end
  )

-- | Pass the return value through a Maybe monad while without changing it's type, using do notation
testPT7 = TestCase $ assertEqual
  "testPT7"
  (Just 2)
  (do
     let
       start :: Int -> Maybe Int
       start int = return int
       end :: Int -> Maybe Int
       end int = return $ int + 1
     s <- start 1
     e <- end s
     return e
  )


