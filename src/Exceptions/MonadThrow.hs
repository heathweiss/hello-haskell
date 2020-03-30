{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Try out the MonadThrow typeclass, directed towards using pure code from within the RIO monad.

It uses the Control.Exception module to throw exceptions from pure code, but catches them with `catch` from Control.Monad.Catch

The goal is to try to push erorr handling upstream as per: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
which I found in: https://williamyaoh.com/posts/2019-11-24-design-and-testing-articles.html
In the case of RIO, he is recommending throwing exceptions for Either values, so upstream I will throw an exception instead of an Either.
Seems like there should be a fx for doing it either way <Either/Exception> as just throwing upstream exceptions seems a little extreme.
-}
module Exceptions.MonadThrow where

import qualified Control.Monad.Catch as Ex
--import qualified Control.Exception as ExBase
import qualified Prelude as P
import RIO

data MyException = ThisException | ThatException
    deriving Show

instance Ex.Exception MyException


-- throwM can throw (& catch?) exceptions in monads, including IO
-- But throwing in IO and RIO can just use the regular Control.Exception module
throwInIOFromMonadCatch = do
  Ex.throwM ThisException `Ex.catch` \e -> P.putStrLn ("Caught " ++ show (e :: MyException))
  return ()





  
