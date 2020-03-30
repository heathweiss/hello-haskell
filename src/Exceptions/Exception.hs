{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-
Use the standard exception package Control.Exception
-}
module Exceptions.Exception() where

import qualified Control.Exception as Ex

import qualified Prelude as P
import RIO


data MyException = ThisException | ThatException
    deriving Show

instance Ex.Exception MyException

throwBoolThatExceptionInPure :: Int -> Bool
throwBoolThatExceptionInPure myInt =
    case myInt == 4 of
      True -> True
      False -> 
        Ex.throw ThatException


--Throw an exception in throwInPure, catch with `catch`.
--Exception module does not recommend this.
catchExceptionThrownFromPureCode =
  (do
    P.putStrLn $ show $ throwInPure 5
  )   `Ex.catch` \e -> P.putStrLn ("Caught " ++ show (e :: MyException))


{-
see: https://stackoverflow.com/questions/6009384/exception-handling-in-haskell

Notice how it returns an Either, instead of a Maybe.
The function throwInPure must be raised to the IO monad to work in the tryJust.
-}               
tryExceptionThrownFromPureCode2 :: IO ()
tryExceptionThrownFromPureCode2 =
  (
  do
    let
      handleThisException :: MyException -> Maybe Bool
      handleThisException ThisException = Just False
      handleThisException _ = Nothing

    --The version with `return` instead of `evaluate` compiles, but gets caught in the final `catch`
    --thisException <- tryJust handleThisException (return $ throwInPure 5)
    
    --This version, using `evaluate` gets caught locally.
    thisException <- tryJust handleThisException  (evaluate $ throwInPure 4)
    case thisException of
      Right val -> P.putStrLn $ "Right: " ++ show val
      Left msg -> P.putStrLn $ "Left: " ++ show msg

    --this gets handled in the final catch because it returns a ThatException which is not handled in handleThisException
    thatException <- tryJust handleThisException (evaluate $ throwBoolThatExceptionInPure 5) -- (evaluate $ throwInPure 4)
    P.putStrLn $ "local thatException: " ++  show thatException
    return ()
  )
  `Ex.catch` \(Ex.SomeException e) -> do
                     P.putStrLn $ "in final catch:" ++ show e
                     --"Right in the final catch"
                     return ()
-----------------------------------------------------------------------------------------------------------------------
--throwing and catching an exception in IO can be done with throw and throwIO.
--The difference is that throwIO can only be used from IO, while throw can be used anywhere, including pure code.

--use throw
throwInIO = do
  Ex.throw ThisException `Ex.catch` \e -> P.putStrLn ("Caught " ++ show (e :: MyException))
  return ()

--use throwIO
throwIOInIO = do
  Ex.throwIO ThisException `Ex.catch` \e -> P.putStrLn ("Caught " ++ show (e :: MyException))
  return ()

throwInPure  :: Int -> Bool
throwInPure myInt =
    case myInt == 4 of
      True -> True
      False -> 
        Ex.throw ThisException
