{-# LANGUAGE OverloadedStrings #-}
module FromMathToGenericPrgrmBk.Chap1() where
import Test.HUnit
import Gauge.Main

-- See thhe benchmark exmaples from https://www.stackage.org/haddock/lts-15.8/gauge-0.2.5/Gauge-Main.html
----------------------------- sec 2.1-------------------------------------------
--this is not in the book, but can benchmark it to compare.
multWithPreludeMult :: Int -> Int -> Int
multWithPreludeMult n a = n * a

multWithNoOptimizations :: Int -> Int -> Int
multWithNoOptimizations n a =
  
   case isNReducedTo1 n of
    True -> a
    moreReductionNeeded -> a + (multWithNoOptimizations (n-1) a)
   where
    isNReducedTo1 n' = n' ==  1
    moreReductionNeeded = False 
    {-
    isReduced -> 6-- a
    moreReductionNeeded -> a + (multWithNoOptimizations (n-1) a)-}

 
{-
Uses divide and conquer, by halving n and doubling a.
Has to handle div to an integer by using 'odd' function.
This 'odd' fx creates a remainder which is kill tail recursion
-}
multWithHalvingButNoTailOptmization :: Int -> Int -> Int
multWithHalvingButNoTailOptmization n a
            | let doneReducingNto1 = n == 1
              in doneReducingNto1 = a
            | let
                 nIsAnOddNumber = (odd n)
              in
                nIsAnOddNumber =
                  let
                    addOddRemainerToResultsOfFurtherProcessing resultOfFurtherProcessing oddRemainder = resultOfFurtherProcessing + oddRemainder
                    resultOfFurtherHalving =  multWithHalvingButNoTailOptmization (fromIntegral((n-1)`div`2)) (a + a)
                  in
                  addOddRemainerToResultsOfFurtherProcessing resultOfFurtherHalving  a
            | let
                nIsAnEvenNumber = True
              in
                nIsAnEvenNumber = multWithHalvingButNoTailOptmization (n`div`2) (2*a)



-- 
multWithFixedOddTailRecurson :: Int -> Int -> Int -> Int
multWithFixedOddTailRecurson accumulator n a
            | let doneReducingNto1 = n == 1
              in doneReducingNto1 = accumulator + a
            | let
                 nIsAnOddNumber = (odd n)
              in
                nIsAnOddNumber =
                   multWithFixedOddTailRecurson a (fromIntegral((n-1)`div`2)) (a + a)
            | let
                nIsAnEvenNumber = True
              in
                nIsAnEvenNumber = multNIsRarely1 0 (n`div`2) (2 * a + accumulator)
            

multNIsRarely1 :: Int -> Int -> Int -> Int
multNIsRarely1 accumulator n a
            | let
                 nIsAnOddNumber = (odd n)
              in
                nIsAnOddNumber =
                  case n == 1 of
                    True -> accumulator + a
                    False -> multNIsRarely1 a (fromIntegral((n-1)`div`2)) (a + a)
            | let
                nIsAnEvenNumber = True
              in
                nIsAnEvenNumber = multNIsRarely1 0 (n`div`2) (2 * a + accumulator)
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------- benchmarks ------------------------------------------------------------
benchmark_multWithNoOptimizations =
     defaultMain [
       bgroup "multWithNoOptimizations" [ bench"10 10" $ whnf (multWithNoOptimizations 10) 10 -- 5.193 μs
                      , bench "5 20" $ whnf (multWithNoOptimizations 5)  20 -- 2.307 μs
                      , bench "4 25" $ whnf (multWithNoOptimizations 4)  25 -- 1.891 μs
                    ]
       ]


bench_MultWithHalvingButNoTailOptmization =
     defaultMain [
       bgroup "multWithHalvingButNoTailOptmization" [ bench"10 10" $ whnf (multWithHalvingButNoTailOptmization 10) 10       -- 3.370 μs
                          , bench "5 20" $ whnf (multWithHalvingButNoTailOptmization 5)  20       -- 2.529 μs 
                          , bench "4 25" $ whnf (multWithHalvingButNoTailOptmization 4)  25       -- 2.016 μs
                          ]
                ]

bench_multWithFixedOddTailRecurson =
     defaultMain [
       bgroup "multWithFixedOddTailRecurson" [ bench"10 10" $ whnf (multWithFixedOddTailRecurson 0 10) 10       -- 3.370 μs
                          , bench "5 20" $ whnf (multWithFixedOddTailRecurson 0 5)  20       -- 2.529 μs 
                          , bench "4 25" $ whnf (multWithFixedOddTailRecurson 0 4)  25       -- 2.016 μs
                          ]
                ]

bench_multNIsRarely1 =
     defaultMain [
       bgroup "multNIsRarely1" [ bench"10 10" $ whnf (multNIsRarely1 0 10) 10       -- 3.370 μs
                          , bench "5 20" $ whnf (multNIsRarely1 0 5)  20       -- 2.529 μs 
                          , bench "4 25" $ whnf (multNIsRarely1 0 4)  25       -- 2.016 μs
                          ]
                ]

benchmark_multWithPreludeMult =
     defaultMain [
       bgroup "multWithPreludeMult" [ bench"10 10" $ whnf (multWithPreludeMult 10) 10 -- 5.193 μs
                      , bench "5 20" $ whnf (multWithPreludeMult 5)  20 -- 2.307 μs
                      , bench "4 25" $ whnf (multWithPreludeMult 4)  25 -- 1.891 μs
                    ]
       ]
-----------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------- tests ------------------------------------------------------------------
runTests = do
 putStrLn $ "=============== mult 0 ====================="  

 let
  testMultWithNoOptimizationsEq = TestCase $ assertEqual
   "2 * 3 = 6"
   (6)
   (multWithNoOptimizations 2 3)
 runTestTT testMultWithNoOptimizationsEq

 let
  testMultWithNoOptimizationsEq2 = TestCase $ assertEqual
   "Book said they have to be positive but is ok if the second value 'a' is neg. The first value 'n' will cause stack overflow when neg."
   (-6)
   (multWithNoOptimizations 2 (-3))
 runTestTT testMultWithNoOptimizationsEq2

  
 putStrLn $ "=============== multWithHalvingButNoTailOptmization ====================="

 let
  testMultWithHalvingButNoTailOptmizationEq = TestCase $ assertEqual
   "Vectors are equal"
   (100)
   (multWithHalvingButNoTailOptmization 10 10)
 runTestTT testMultWithHalvingButNoTailOptmizationEq

 let
  testMultWithHalvingButNoTailOptmizationEq2 = TestCase $ assertEqual
   "Vectors are equal"
   (84)
   (multWithHalvingButNoTailOptmization 12 7)
 runTestTT testMultWithHalvingButNoTailOptmizationEq2
 
 putStrLn $ "=============== multWithFixedOddTailRecurson ====================="
 let
  testmultWithFixedOddTailRecurson = TestCase $ assertEqual
   "Vectors are equal"
   (100)
   (multWithFixedOddTailRecurson 0 10 10)
 runTestTT testmultWithFixedOddTailRecurson

 let
  testmultWithFixedOddTailRecurson2 = TestCase $ assertEqual
   "Vectors are equal"
   (84)
   (multWithFixedOddTailRecurson 0 12 7)
 runTestTT testmultWithFixedOddTailRecurson2

 putStrLn $ "=============== multNIsRarely1 ====================="
 let
  testmultNIsRarely1 = TestCase $ assertEqual
   "Vectors are equal"
   (100)
   (multNIsRarely1 0 10 10)
 runTestTT testmultNIsRarely1

 let
  testmultNIsRarely12 = TestCase $ assertEqual
   "Vectors are equal"
   (84)
   (multNIsRarely1 0 12 7)
 runTestTT testmultNIsRarely12
{-
benchmarking multWithNoOptimizations/10 10 ... took 26.92 s, total 183127 iterations
benchmarked multWithNoOptimizations/10 10
time                 3.076 μs   (1.300 μs .. 4.860 μs)
                     0.722 R²   (0.434 R² .. 0.898 R²)
mean                 5.193 μs   (4.928 μs .. 5.523 μs)
std dev              514.3 ns   (366.8 ns .. 626.4 ns)
variance introduced by outliers: 29% (moderately inflated)

benchmarking multWithNoOptimizations/5 20 ... took 29.98 s, total 441110 iterations
benchmarked multWithNoOptimizations/5 20
time                 2.525 μs   (1.998 μs .. 2.919 μs)
                     0.948 R²   (0.888 R² .. 0.995 R²)
mean                 2.307 μs   (2.267 μs .. 2.372 μs)
std dev              83.11 ns   (34.85 ns .. 116.6 ns)

benchmarking multWithNoOptimizations/4 25 ... took 29.64 s, total 510685 iterations
benchmarked multWithNoOptimizations/4 25
time                 1.562 μs   (1.053 μs .. 2.021 μs)
                     0.885 R²   (0.780 R² .. 0.984 R²)
mean                 1.891 μs   (1.838 μs .. 1.946 μs)
std dev              92.74 ns   (73.70 ns .. 109.8 ns)

-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------

benchmarked multWithHalvingButNoTailOptmization/10 10
time                 4.132 μs   (3.348 μs .. 5.119 μs)
                     0.944 R²   (0.908 R² .. 0.987 R²)
mean                 3.161 μs   (3.068 μs .. 3.302 μs)
std dev              190.5 ns   (116.1 ns .. 272.6 ns)
variance introduced by outliers: 18% (moderately inflated)

benchmarking multWithHalvingButNoTailOptmization/5 20 ... took 28.34 s, total 420091 iterations
benchmarked multWithHalvingButNoTailOptmization/5 20
time                 2.925 μs   (2.350 μs .. 3.409 μs)
                     0.959 R²   (0.895 R² .. 0.996 R²)
mean                 2.396 μs   (2.347 μs .. 2.479 μs)
std dev              109.0 ns   (68.04 ns .. 162.6 ns)

benchmarking multWithHalvingButNoTailOptmization/4 25 ... took 29.35 s, total 536234 iterations
benchmarked multWithHalvingButNoTailOptmization/4 25
time                 2.141 μs   (1.754 μs .. 2.729 μs)
                     0.930 R²   (0.850 R² .. 0.991 R²)
mean                 1.837 μs   (1.795 μs .. 1.902 μs)
std dev              89.10 ns   (32.55 ns .. 116.4 ns)

---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------

benchmarking multWithFixedOddTailRecurson/10 10 ... took 27.43 s, total 270694 iterations
benchmarked multWithFixedOddTailRecurson/10 10
time                 3.635 μs   (2.415 μs .. 4.341 μs)
                     0.862 R²   (0.567 R² .. 0.987 R²)
mean                 3.889 μs   (3.776 μs .. 4.071 μs)
std dev              234.0 ns   (99.94 ns .. 305.4 ns)
variance introduced by outliers: 18% (moderately inflated)

benchmarking multWithFixedOddTailRecurson/5 20 ... took 28.80 s, total 329090 iterations
benchmarked multWithFixedOddTailRecurson/5 20
time                 2.822 μs   (1.669 μs .. 3.553 μs)
                     0.884 R²   (0.811 R² .. 0.984 R²)
mean                 2.874 μs   (2.786 μs .. 2.960 μs)
std dev              143.2 ns   (116.2 ns .. 170.8 ns)

benchmarking multWithFixedOddTailRecurson/4 25 ... took 29.54 s, total 362851 iterations
benchmarked multWithFixedOddTailRecurson/4 25
time                 1.486 μs   (684.7 ns .. 3.086 μs)
                     0.542 R²   (0.252 R² .. 0.840 R²)
mean                 2.919 μs   (2.680 μs .. 3.313 μs)
std dev              492.2 ns   (268.9 ns .. 754.2 ns)
variance introduced by outliers: 58% (severely inflated)



---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
benchmarking multNIsRarely1/10 10 ... took 26.75 s, total 245502 iterations
benchmarked multNIsRarely1/10 10
time                 3.277 μs   (2.276 μs .. 4.485 μs)
                     0.903 R²   (0.852 R² .. 0.991 R²)
mean                 3.824 μs   (3.719 μs .. 4.034 μs)
std dev              241.1 ns   (122.7 ns .. 381.6 ns)
variance introduced by outliers: 18% (moderately inflated)

benchmarking multNIsRarely1/5 20 ... took 27.27 s, total 345559 iterations
benchmarked multNIsRarely1/5 20
time                 2.101 μs   (1.232 μs .. 2.748 μs)
                     0.816 R²   (0.485 R² .. 0.997 R²)
mean                 2.960 μs   (2.879 μs .. 3.250 μs)
std dev              227.9 ns   (52.31 ns .. 376.7 ns)
variance introduced by outliers: 19% (moderately inflated)

benchmarking multNIsRarely1/4 25 ... took 27.91 s, total 400073 iterations
benchmarked multNIsRarely1/4 25
time                 3.401 μs   (2.792 μs .. 3.993 μs)
                     0.929 R²   (0.818 R² .. 0.988 R²)
mean                 2.630 μs   (2.541 μs .. 2.739 μs)
std dev              173.8 ns   (143.4 ns .. 198.8 ns)
variance introduced by outliers: 19% (moderately inflated)

---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
benchmarking multWithPreludeMult/10 10 ... took 39.46 s, total 8244743 iterations
benchmarked multWithPreludeMult/10 10
time                 165.0 ns   (140.0 ns .. 196.8 ns)
                     0.948 R²   (0.913 R² .. 0.991 R²)
mean                 125.1 ns   (121.8 ns .. 132.2 ns)
std dev              7.526 ns   (4.892 ns .. 11.12 ns)
variance introduced by outliers: 18% (moderately inflated)

benchmarking multWithPreludeMult/5 20 ... took 38.75 s, total 8244743 iterations
benchmarked multWithPreludeMult/5 20
time                 141.1 ns   (117.4 ns .. 163.4 ns)
                     0.971 R²   (0.958 R² .. 0.996 R²)
mean                 122.2 ns   (120.5 ns .. 125.5 ns)
std dev              3.961 ns   (1.805 ns .. 6.294 ns)

benchmarking multWithPreludeMult/4 25 ... took 39.01 s, total 8244743 iterations
benchmarked multWithPreludeMult/4 25
time                 129.7 ns   (123.9 ns .. 138.1 ns)
                     0.996 R²   (0.990 R² .. 0.999 R²)
mean                 122.4 ns   (121.5 ns .. 123.5 ns)
std dev              1.667 ns   (1.271 ns .. 2.155 ns)


-}
