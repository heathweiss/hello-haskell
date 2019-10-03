{-# LANGUAGE TemplateHaskell #-}
-- | Try out some lenses as per the RIO tutorial.
-- | Refer to https://tech.fpcomplete.com/haskell/library/rio
module Lenses(test1, test2, test3) where

import Control.Lens
import Test.HUnit
import Control.Monad.Reader

runTests = do
  runTestTT test1
  runTestTT test2
  runTestTT test3




data App = App
    {appName :: !String}

class HasName env where
  nameL :: Lens' env String

instance HasName App where
  nameL = lens appName (\x y -> x {appName = y})

-- | Access a field using view, and classes.
test1 = TestCase $ assertEqual
  "test1"
  ("Joe")
  (let
      app = App "Joe"
   in
   view nameL app
  )

-- | Access a field using Reader, view, and classes.
test2 = TestCase $ assertEqual
  "test2"
  ("Joe is my name")
  (let
      app = App "Joe"
      start :: Reader App String
      start = do
        name <- view nameL
        
        return $ name ++ " is my name"
        --Or bypass the: name <- view nameL
        --return $  (view nameL a) ++ " is Joe"
   in
   
   runReader (start ) app
  )

-- | Set a field using view, set, and classes.
test3 = TestCase $ assertEqual
  "test3"
  ("Joe Blow")
  (let
      app = App "Joe"

      --Note the never have to say type 'App' as the App is always accessed via the HasName typeclass, even for the assertEqual.
      addLastName :: HasName app => app -> app
      addLastName app_in =
        let
          name =  view nameL app_in
        in set nameL (name ++ " Blow") app_in 
        
   in
   
   view nameL $ addLastName app
   
  )

