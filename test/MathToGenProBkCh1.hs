{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module MathToGenProBkCh1(runTests) where

import Test.HUnit
import FromMathToGenericPrgrmBk.Chap1
import qualified Prelude as P
import RIO

runTests = do
 P.putStrLn "=============== Math to Generic Programming Chap 1 ====================="  

 let
  testMult0Eq = TestCase $ assertEqual
   "Vectors are equal"
   (6)
   (mult0 2 3)
 runTestTT testMult0Eq
