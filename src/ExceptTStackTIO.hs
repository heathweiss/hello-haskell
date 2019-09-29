module ExceptTStackTIO where

import qualified Control.Monad.Trans.Except as TE
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
import Control.Monad.Writer (Writer, tell, execWriter)
import qualified System.IO as SIO

{-
Show that using this transformer stack does not require a single ADT to be used as the monad return type,
as shown by the definition of >>= which is (a -> b) -> ma -> mb.

This ExceptStackIOBuilder represents the Builder used for ChampCad GMSH.

-}


{-
Declare a monad-transformer that takes a type parameter, so the final type returned by the do statement can vary.
The type for a gets supplied when the Builder fx is declared. Now I can return any type of
value that is inside the ExceptStackIOBuilder stack.


-}
type ExceptStackIOBuilder a =  E.ExceptT String (SL.StateT Int (IO)) a


{-Can lift a String or an Int into the monad, all that matters, is that the Bool is returned,
to match the Bool for the ExceptStackIOBuilder type parameter.

Can't leave it up to the compiler to infer, as the type will by ambiguous.
-}
test1 :: ExceptStackIOBuilder Bool
test1 = do
  x <- return True
  y <- return "this is a string"
  return x

runTest1 :: IO ()
runTest1 = do
  a <- (SL.evalStateT $ E.runExceptT test1) 4
  putStrLn $ show a
