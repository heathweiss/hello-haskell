module Covariance() where
import Test.HUnit 
{-
https://www.fpcomplete.com/blog/2016/11/covariance-contravariance
-}
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
--------------------------- Functor typeclass: covariant functor --------------------------------------------------------
showInt :: Int -> String
showInt = show

floorInt :: Double -> Int
floorInt = floor

maybeInt :: Maybe Int
maybeInt = Just 5

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe = fmap

maybeString :: Maybe String
maybeString = fmapMaybe showInt maybeInt


stage1Testing = do
  
  --use showInt on it's own
  let useShowIntTest = TestCase $ assertEqual
        "useShowIntTest"
        ("5")
        (showInt 5)
  runTestTT useShowIntTest

  --map showInt over a Maybe Int
  let mapShowIntOverMabyeInt = TestCase $ assertEqual
        "can map showInt onto maybeInt"
        (Just "5")
        (fmapMaybe showInt maybeInt)
        --which is the same as:
        --(fmap show $ Just 5)
  runTestTT mapShowIntOverMabyeInt
  
  {- does not compile as floorInt takes a double, but got a Int from maybeInt :: Maybe Int
     This shows that fmap is covariant on it's type argument.
  let mapFloorIntOverMabyeInt = TestCase $ assertEqual
        "can't map floorInt onto maybeInt"
        (Just "5")
        (fmapMaybe floorInt maybeInt)
  runTestTT mapFloorIntOverMabyeInt -}
  --but it does compile if a Mabye Double is supplied.
  let mapFloorIntOverMabyeDouble = TestCase $ assertEqual
        "can't map floorInt onto maybeInt"
        (Just 5)
        (fmapMaybe floorInt $ Just 5.1)
        --same as
        --(fmap floorInt $ Just 5.1)
  runTestTT mapFloorIntOverMabyeDouble


-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------
----------------------------------------------- non-covariant data type -------------------------------------------------
newtype MakeString a = MakeString {makeString :: a -> String}

--printShowInt = print $ (makeString showInt) 5
--same as above, but without the showInt wrapper
printShowInt = print $ makeString (MakeString show) 5

plus3ShowInt :: MakeString Int
plus3ShowInt = MakeString (show . (+ 3))

mapMakeString :: (b -> a) -> MakeString a -> MakeString b
mapMakeString f (MakeString g) = MakeString (g . f)


--same as plus3ShowInt but using mapMakeString
plus3ShowInt' :: MakeString Int
plus3ShowInt' = mapMakeString (+ 3) (MakeString show)



stage2Testing = do
  let
    --To test a string in the IO monad, need to return a IO (String)
    returnIOVal :: String -> IO (String)
    returnIOVal string = do
      return string


  --Testing showInt' in the IO monad
  let useShowIntTest = TestCase
        (do
           --x <- returnIOVal $ makeString showInt' 5
           x <- return $ makeString (MakeString show) 5
           --y <- returnIOVal $ show 5
           y <- return $ show 5
           assertEqual "They should not be equal, but they are!" x y
        )
  
  runTestTT useShowIntTest

  --test showInt' outside the IO monad.
  let useShowIntTest2 = TestCase $ assertEqual
        "Fails as it should"
        (show 5)
        (makeString (MakeString show) 5)
  runTestTT useShowIntTest2

  let plus3ShowIntTest = TestCase $ assertEqual
        "can map + 3 . show onto MakeString"
        ("8")
        --(makeString plus3ShowInt 5)
        --same as above, but without the plus3ShowInt wrapper
        (makeString  ( MakeString (show . (+ 3))) 5)
  runTestTT plus3ShowIntTest

  ------------------------------------------------------------------------------------------------------
  ------------------------------------------------------------------------------------------------------
  ------------------------------------ now use mapping on the showInt-----------------------------------
  
  let mapMakeStringTest = TestCase $ assertEqual
        "can map another fx onto MakeString"
        ("8")
        ((makeString --MakeString a -> (a -> String)
          --plus3ShowInt' --
          (mapMakeString --mapMakeString :: (b -> a) -> MakeString a -> MakeString b
                         --mapMakeString f (MakeString g) = MakeString (g . f)
             (+ 3)       --the f in mapMakeString  
             (MakeString show) --the MakeString a
          )
         ) 5
        )
  runTestTT mapMakeStringTest

  
