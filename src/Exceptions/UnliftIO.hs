{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exceptions.UnliftIO() where
import RIO
import qualified Prelude as P
import qualified RIO.Text as T

--import qualified UnliftIO.Exception  as Ex
--not req'd as this is what RIO supplies for exception handling

{-
Recommended Exception handling by FP Complete.
The RIO monad supplies UnliftIO.Exception instead of Control.Exception.

Work through the tutorials:
https://tech.fpcomplete.com/haskell/tutorial/exceptions
https://tech.fpcomplete.com/haskell/library/unliftio

https://tech.fpcomplete.com/haskell/library/unliftio
-}





-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
-- from the tutorial: https://tech.fpcomplete.com/haskell/tutorial/exceptions


-----------------main 1 & 2---------------------------------
main1 :: IO ()
main1 = runSimpleApp $ do
  let fp = "myfile.txt"
  message <- readFileUtf8 fp `catchIO` \e -> do
    logWarn $ "Could not open " <> fromString fp <> ": " <> displayShow e
    pure "This is the default message"
  logInfo $ display message

oneSecond, fiveSeconds :: Int
oneSecond = 1000000
fiveSeconds = 5000000

main2 :: IO ()
main2 = runSimpleApp $ do
  res <- timeout oneSecond $ do
    logInfo "Inside the timeout"
    res <- tryAny $ threadDelay fiveSeconds `finally`
      logInfo "Inside the finally"
    logInfo $ "Result: " <> displayShow res
  logInfo $ "After timeout: " <> displayShow res



------------------------------- main 3----------------------------
data MyException = MyException
                 | MyExceptionWithMsg T.Text

instance Show MyException where
  show MyException = "MyException"
  show (MyExceptionWithMsg text) = T.unpack $  "MyExceptionWithMsg: " <> text

instance Exception MyException

--this is not from the tutorial
throwFromIO :: IO ()
throwFromIO = throwIO MyException `catch` \(SomeException e) -> do
                                      P.putStrLn "caught"

main3 :: IO ()
main3 =
  runSimpleApp $
  throwIO MyException `catch` \MyException ->
  logInfo "I caught my own exception!"


-------------------------- main 4 -------------------------------
data Parent = Parent1 Child1 | Parent2 Child2
  deriving (Show, Typeable)
instance Exception Parent

data Child1 = Child1
  deriving (Show, Typeable)
instance Exception Child1 where
  toException = toException . Parent1 -- cast up through the Parent type
  fromException se =
    case fromException se of
      Just (Parent1 c) -> Just c
      _ -> Nothing

data Child2 = Child2
  deriving (Show, Typeable)
instance Exception Child2 where
  toException = toException . Parent2 -- cast up through the Parent type
  fromException se =
    case fromException se of
      Just (Parent2 c) -> Just c
      _ -> Nothing

main4 :: IO ()
main4 = runSimpleApp $ do
  throwIO Child1 `catch` (\(_ :: SomeException) -> logInfo "Caught it!")
  throwIO Child1 `catch` (\(_ :: Parent) -> logInfo "Caught it again!")
  throwIO Child1 `catch` (\(_ :: Child1) -> logInfo "One more catch!")
  throwIO Child1 `catch` (\(_ :: Child2) -> logInfo "Missed!")


------------------------- Type ambiguity ----------------------------
{- Does not compile, as I need to implement openResource useResource closeResource. Probably with something like a file handle: open read close
foo :: IO ()
foo = do
  resource <- openResource
  eitherResult <- try $ useResource resource
  closeResource resource
  case eitherResult of
    Left e -> throwIO e
    Right result -> pure result
-}

--------------------------------- Throwing --------------------------------------------------
---------------------------- Throwing from pure ---------------------------------------------
{-
Not recommended to throw from pure code.
See the next section --use Either-- for a better implementaion
-}
throwIntFromPure :: Int -> Int
throwIntFromPure int =
  case int == 4 of
    True -> 4
    False -> impureThrow MyException



runThrowIntFromPure :: IO ()
runThrowIntFromPure =
  (do
    let
      e_good  = throwIntFromPure 4
      e_error = throwIntFromPure 5
    runSimpleApp $ logInfo $ displayShow e_good -- "done"-}
    runSimpleApp $ logInfo $ displayShow e_error -- "done"-}
  )
  `catch` \(SomeException e) -> P.putStrLn "caught"

--------------------------------- Throwing --------------------------------------------------
-------------------------- returning Either from pure ---------------------------------------

{-
The new version of throwIntFromPure(from prev. section), which allows return of Left MyException,
which is then handled by calling it with runEitherIO, which uses throwIO on Left, otherwise extracts value from Right
-}
giveMeA4 :: Int -> Either MyException Int
--giveMeA4 ::  (Exception e, MonadIO m) =>  Int -> Either e Int
--Need to make it more generic? But why, when it can still be caught by SomeException
giveMeA4 int =
  case int == 4 of
    True -> Right 4
    False ->
      case int > 4 of
        True -> Left $ MyExceptionWithMsg " was > 4"
        False -> Left $ MyExceptionWithMsg " was < 4"

{- Call a function returns an Either MyException a.
If value was good, gets unwrapped from the Right constructor.
If value was bad, uses throwIO to short circuit the IO monad.
-}
runEitherIO ::  T.Text -> Either MyException a -> IO (a)
--runEitherIO ::  (Exception e, MonadIO m) =>   T.Text -> Either e a -> IO (a)
--Need to make it more generic? But why, when it can still be caught by SomeException
runEitherIO _ (Right a) = return a
runEitherIO location (Left (MyException)) = do
  throwIO $ MyExceptionWithMsg location
runEitherIO location (Left (MyExceptionWithMsg msg)) = do
  throwIO $ MyExceptionWithMsg $ location <> ": " <> msg

--Use the function giveMeA4 which returns an Either Int MyException
runThowIntExceptionFromPure :: IO ()
runThowIntExceptionFromPure =
  (do
    e_good  <- (runEitherIO "e_good" (giveMeA4 4 ))
    runSimpleApp $ logInfo $ displayShow e_good 

    e_error <- runEitherIO "e_error" $ giveMeA4 5
    runSimpleApp $ logInfo $ displayShow e_error 
  )
  `catch` (\(MyExceptionWithMsg msg) -> runSimpleApp $ logInfo $ displayShow msg)
  `catch` \(SomeException e) -> runSimpleApp $ logInfo "caught by SomeException e"
  
-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------
-- from the tutorial:
--https://tech.fpcomplete.com/haskell/library/unliftio
--
