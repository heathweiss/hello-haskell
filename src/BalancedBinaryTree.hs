{- | Build balanced binary Data.Map.Strict from the containers package on stackage.

Will first make a simple tree of Int.

Then a tree using a complex datatype, which will use Eq and Ord based on 2 contained datatypes: Year, Quarter.
These will correspond to a financial entry that can have 4 Quarterly entries for a single year.
-}

module BalancedBinaryTree where

import Test.HUnit

--import Data.Tree.AVL
import qualified Data.Map.Strict as Map

runTests = do
  runTestTT test1
  runTestTT test2
  runTestTT test3
  runTestTT test4
  runTestTT test5
  runTestTT test6
  runTestTT test7
  runTestTT test8
  runTestTT test9
  runTestTT test10
  runTestTT test11
  runTestTT test12
  runTestTT test12a
  runTestTT test13
  runTestTT test14
  runTestTT test15

  runTestTT testYear1
  runTestTT testYear2
  runTestTT testYear3
  runTestTT testYear4
  runTestTT testYear5

  runTestTT testPeriod1
  runTestTT testPeriod2
  runTestTT testPeriod3
  runTestTT testPeriod4

  runTestTT testPeriodOrd1
  runTestTT testPeriodOrd2
  runTestTT testPeriodOrd4
  runTestTT testPeriodOrd5
  runTestTT testPeriodOrd6

  runTestTT testPeriodMap1

-- * Get a strict Map up and running
--
-- $upAndRunning
--
-- Check out some basic functionality.

-- | Create an empty map, and check that it is empty.
--
-- >>> Map.empty $ Map.null
-- > True
test1 = TestCase $ assertEqual
  "tes1b"
  (True)
  (Map.null $ Map.empty
  )


-- | Create an empty map, and insert a key value pair, then get the values as a list.
--
-- See that the items are returned in the order they were inserted, which is also the ascending order of the keys.
--
-- >>> Map.elems $ Map.insert 2 "two" $ Map.insert 1 "one" $ Map.empty
-- > ["one","two"]
test2 = TestCase $ assertEqual
  "test2"
  (["one","two"])
  (Map.elems $ Map.insert 2 "two" $ Map.insert 1 "one" $ Map.empty
  )


-- | Same as test2, but with the inputs reversed.
--
-- The items are returned in the ascending order of the keys, instead of the order that they were inserted in, which was descending order.
--
-- >>> Map.elems $ Map.insert 1 "one" $ Map.insert 2 "two" $ Map.empty
-- > ["one","two"]
test3 = TestCase $ assertEqual
  "test3"
  (["one","two"])
  (Map.elems $ Map.insert 1 "one" $ Map.insert 2 "two" $ Map.empty
  )


-- | Same as test2 and 3, but with more inputs, which are in a random ordering
--
-- The items are returned in the ascending order of the keys, instead of the order that they were inserted in, which was random.
--
-- >>> Map.elems $ Map.fromList [(33,"thirty-three"),(1,"one"),(3,"three"),(0,"zero")]
-- > ["zero","one","three","thirty-three"]
test4 = TestCase $ assertEqual
  "test4"
  (["zero","one","three","thirty-three"])
  (Map.elems $ Map.fromList [(33,"thirty-three"),(1,"one"),(3,"three"),(0,"zero")]
  )


-- | Attempt to insert the same key value pair twice. It only has 1 inserted 
--
-- >>> Map.elems $ Map.fromList [(1,"one"),(2,"two"),(3,"three"),(2,"two")
-- > ["one","two","three"]
test5 = TestCase $ assertEqual
  "test5"
  (["one","two","three"])
  (Map.elems $ Map.fromList [(1,"one"),(2,"two"),(3,"three"),(2,"two")]
  )

-- | Insert the same key twice, each with a different value, results in the 1st value being overwritten.
--
-- >>> Map.elems $ Map.fromList [(1,"one"),(2,"two"),(3,"three"),(2,"twoooo")
-- > ["one","twooooo","three"]
test6 = TestCase $ assertEqual
  "test6"
  (["one","twooooo","three"])
  (Map.elems $ Map.fromList [(1,"one"),(2,"two"),(3,"three"),(2,"twooooo")]
  )


-- * Use a datatype that is ordered on both a Year and a Quarter
--
-- $upAndRunning
--
-- Build a datatype that contains a year, a quarter, and a value. It ordered based on both year and quarter.
--
-- Then it needs to group the quarters by year so that a Q4 and be extracted.

-- ** Start by building an ordered Quater datatype

-- | Represents a financial reporting period.
data Quarter = Q1 | Q2 | Q3 | Q4

instance Eq Quarter where
  Q1 == Q1 = True
  Q2 == Q2 = True
  Q3 == Q3 = True
  Q4 == Q4 = True
  q == q' = False

-- | Q1 == Q1
--
-- > True
test7 = TestCase $ assertEqual
  "test7"
  (True)
  (Q1 == Q1)

-- | Q1 == Q2
-- 
-- > False
test8 = TestCase $ assertEqual
  "test8"
  (False)
  (Q1 == Q2)

instance Ord Quarter where
  Q1 `compare` Q2 = LT
  Q1 `compare` Q3 = LT
  Q1 `compare` Q4 = LT
  Q4 `compare` Q1 = GT
  Q3 `compare` Q1 = GT
  Q2 `compare` Q1 = GT
  Q1 `compare` Q1 = EQ
  
  Q2 `compare` Q3 = LT
  Q2 `compare` Q4 = LT
  Q4 `compare` Q2 = GT
  Q3 `compare` Q2 = GT
  Q2 `compare` Q2 = EQ

  Q3 `compare` Q4 = LT
  Q4 `compare` Q3 = GT
  Q3 `compare` Q3 = EQ

  Q4 `compare` Q4 = EQ
  

-- | Q1 < Q4
--
-- > True
test9 = TestCase $ assertEqual
  "test9"
  (True)
  (Q1 < Q4)


-- | Q4 > Q1
-- 
-- > True
test10 = TestCase $ assertEqual
  "test10"
  (True)
  (Q4 > Q1)

-- | Q1 == Q1
--
-- > True
test11 = TestCase $ assertEqual
  "test11"
  (True)
  (Q1 == Q1)

-- | Q1 == Q4
--
-- > False
test12 = TestCase $ assertEqual
  "test12"
  (False)
  (Q1 == Q4)

-- | Q1 `compare` Q1
--
-- > EQ
test12a = TestCase $ assertEqual
  "test12a"
  (EQ)
  (Q1 `compare` Q1)


-- | Q2 < Q4
--
-- > True
test13 = TestCase $ assertEqual
  "test13"
  (True)
  (Q2 < Q4)


-- | Q4 > Q2
--
-- > True
test14 = TestCase $ assertEqual
  "test14"
  (True)
  (Q4 > Q2)

-- | Q4 == Q2
--
-- > False
test15 = TestCase $ assertEqual
  "test15"
  (False)
  (Q4 == Q2)

-- ** Now create a Year type that is an instance of ordered and eq.

-- | Datatype for a Year. 
data Year = Year {_year :: Int}
 deriving (Eq, Ord)

-- | Year 1 == Year 2
--
-- > False
testYear1 = TestCase $ assertEqual
  "testYear1"
  (False)
  (Year 1 == Year 2)

-- | Year 1 == Year 1
--
-- > True
testYear2 = TestCase $ assertEqual
  "testYear2"
  (True)
  (Year 1 == Year 1)

-- | Year 1 >= Year 1
--
-- > True
testYear3 = TestCase $ assertEqual
  "testYear3"
  (True)
  (Year 1 >= Year 1)

-- | Year 2 > Year 1
--
-- > True
testYear4 = TestCase $ assertEqual
  "testYear4"
  (True)
  (Year 2 > Year 1)

-- | Year 2 `compare` Year 1
--
-- > GT
testYear5 = TestCase $ assertEqual
  "testYear5"
  (GT)
  (Year 2 `compare` Year 1)


-- ** Now combine the Year and Quater into a single Ordered datatype

-- | Combine a Year, Quarter, and a polymorphic value being reported.
data Period a = Period {_period_val :: a, _period_year :: Year, _period_quarter :: Quarter}
  --deriving (Eq, Ord)
  -- can't derive these, as it also uses val.

instance Eq (Period a) where
  (Period _ year quarter) == (Period _ year' quarter') = (year == year') && (quarter == quarter')

instance Ord (Period a) where
  (Period _ year quarter) `compare` (Period _ year' quarter') =
    case year == year' of
      True  ->
        quarter `compare` quarter'
      False ->
        year `compare` year'

-- | All values are EQ 
--
-- >>> Period 1 (Year 2) Q1 == Period 1 (Year 2) Q1
-- > True
testPeriod1 = TestCase $ assertEqual
  "testPeriod1"
  (True)
  (Period 1 (Year 2) Q1 == Period 1 (Year 2) Q1)

-- | All values are EQ, except the polymorphic value. 
--
-- >>> Period 1111 (Year 2) Q1 == Period 1 (Year 2) Q1
-- > True
testPeriod2 = TestCase $ assertEqual
  "testPeriod2"
  (True)
  (Period 1111 (Year 2) Q1 == Period 1 (Year 2) Q1)

-- | All values are EQ, except the Year.
--
-- >>>  Period 1 (Year 1) Q1 == Period 1 (Year 2) Q1
-- > False
testPeriod3 = TestCase $ assertEqual
  "testPeriod3"
  (False)
  ((Period 1 (Year 1) Q1) == (Period 1 (Year 2) Q1))

-- | All values are EQ, except the Quarter.
--
-- >>>  Period 1 (Year 2) Q1 == Period 1 (Year 2) Q1
-- > False
testPeriod4 = TestCase $ assertEqual
  "testPeriod4"
  (False)
  ((Period 1 (Year 1) Q1) == (Period 1 (Year 1) Q2))



-- | All values are equal.
--
-- >>> (Period 1 (Year 1) Q1 `compare` Period 1 (Year 1) Q1
-- > EQ
testPeriodOrd1 = TestCase $ assertEqual
  "testPeriodOrd1"
  (EQ)
  (Period 1 (Year 1) Q1 `compare` Period 1 (Year 1) Q1)

-- | All values are equal, except the polymorphic val.
--
-- >>> Period 2 (Year 2) Q1 `compare` Period 1 (Year 2) Q1
-- > EQ
testPeriodOrd2 = TestCase $ assertEqual
  "testPeriodOrd2"
  (EQ)
  (Period 2 (Year 2) Q1 `compare` Period 1 (Year 2) Q1)

-- | All values are equal, except the Year.
--
-- >>> Period 1 (Year 1) Q1 `compare` Period 1 (Year 2) Q1
-- > LT
testPeriodOrd4 = TestCase $ assertEqual
  "testPeriodOrd4"
  (LT)
  (Period 1 (Year 1) Q1 `compare` Period 1 (Year 2) Q1)

-- | All values are equal, except the Quarter is LT.
--
-- >>> Period 1 (Year 1) Q1 `compare` Period 1 (Year 1) Q2
-- > LT
testPeriodOrd5 = TestCase $ assertEqual
  "testPeriodOrd5"
  (LT)
  (Period 1 (Year 1) Q1 `compare` Period 1 (Year 1) Q2)

-- | The Year is GT while Quarter is LT.
--
-- >>> Period 1 (Year 11) Q1 `compare` Period 1 (Year 1) Q2
-- > GT
testPeriodOrd6 = TestCase $ assertEqual
  "testPeriodOrd6"
  (GT)
  (Period 1 (Year 11) Q1 `compare` Period 1 (Year 1) Q2)

-- ** Now do some mapping of the Period
-- $periodMapping
-- See that Periods are inserted into a Map properly, according to their Eq, Ord properties.

-- | Insert with Years in descending order. All else Eq.
--
-- >>> Map.elems $ Map.fromList [(Period 11 (Year 11) Q1), (Period 1 (Year 1) Q1)]
-- > GT
testPeriodMap1 = TestCase $ assertEqual
  "testPeriodMap1"
  ([])
  (Map.elems $ Map.fromList [(Period 1 (Year 11) Q1), (Period 1 (Year 1) Q2)])
