{-# LANGUAGE DeriveFunctor #-}
module FreeMonad() where
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.Except as E
{-
http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
Got as far as 'program' but it made no sense. Added instance Show FixE so could run 'program' but that did not good on understanding it. up to that point so quit.
The work is commented out at the bottom of this module so can work on: https://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html

next:
googled Free Monad haskell and came up with this.
https://markkarpov.com/post/free-monad-considered-harmful.html
It in turn referred to the first site, and the additional site:

https://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html
Will try to implement this next example of Free monads as it comes with a concrete example.

Another Free monad tutorial: https://medium.com/@fintan.halpenny/free-me-exploring-the-free-data-type-c863499a82f8
-}
---------------------------------------------------------https://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html------------------------------------------------------------------
{-----------------------------------------------------------left off---------------------------------------------------------------------------------
Ready to add the "Commnads with info" to the BillingProgram ADT which is about 1/4 way down the page
-}




--for making instance of Functor and Applicative refer to: https://medium.com/@fintan.halpenny/free-me-exploring-the-free-data-type-c863499a82f8
--or maybe could have used {-# LANGUAGE DeriveFunctor #-} from botom of parsonmatt example to derive the Functor.
data Free f a
  = Pure a
  | Free (f(Free f a))
  deriving Functor
{-
No need for this as it was done by {-# LANGUAGE DeriveFunctor #-}
instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free freer ) = Free $ fmap (fmap f) freer 
-}
instance Functor f => Applicative (Free f) where
  pure a = Pure a
  (Pure f) <*> (Free freer) = Free $ fmap (fmap f) freer

instance (Functor f) => Monad (Free f) where
  return = Pure
  Pure a >>= k = k a
  --this 1st version from https://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html also compiles
  --Free m >>= k = Free ((>>= k) <$> m)
  --this version is from  https://medium.com/@fintan.halpenny/free-me-exploring-the-free-data-type-c863499a82f8 where I got the instances for Functor and Applicative.
  --(>>= k) must be another way of saying <$> m
  --As is says in www.parsonsmatt: exact implementation is unimportant as long as f is a functor
  Free freer >>= k = Free $ fmap (>>= k) freer
  
data BillingProgram
    = GetUserBalance
    | GetUserLastPaymentDate
    | CancelSubscription
    | ChargeUser
    | SendLateNotice

--all these should be newtype
type UserId = Int
type SubscriptionId = Int
type Day = Int

data BillingState
    = BillingState
    { userId :: UserId
    , userBalance :: Double
    , userSubscription :: SubscriptionId
    , lastPaymentDate :: Day
    }
  deriving Show

--I passed in the balance vial the BillingState as I don't have the Stripe.getUserBalance he uses.
--I should create a simple canned fx to simulate it.
--I have deviated from the original. Should get it back to what he does.
interpret :: BillingProgram -> SL.StateT BillingState IO (Double)
interpret  GetUserBalance = do
    
    id <- SL.gets userId
    balance <- SL.gets userBalance
    E.liftIO $ putStrLn $ show balance
    SL.modify (\s -> s {userBalance = balance + 4})
    balance2 <- SL.gets userBalance
    --SL.modify (\s -> s {userBalance = balance + 4})
    E.liftIO $ putStrLn "adding 4"
    E.liftIO $ putStrLn $ show balance2
    return balance2
    --modify (\s -> s { userBalance = balance })
    --E.liftIO $ putStrLn "hell"


--using runStatT and the extra putStrLn's are added by me just to look at what is going on.
--
runInterpret = do
  (a,s) <- ((SL.runStateT $ interpret GetUserBalance) (BillingState 1 10 2 31))
  putStrLn "showing state"
  putStrLn $ show s
  putStrLn "showing userBalance"
  putStrLn $ show a
  
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
--commented out what I got done so far to try another example.
{-
{-

 Why free monads matter
Interpreters

Good programmers decompose data from the interpreter that processes that data. Compilers exemplify this approach, where they will typically represent the source code as an abstract syntax tree, and then pass that tree to one of many possible interpreters. We benefit from decoupling the interpreter and the syntax tree, because then we can interpret the syntax tree in multiple ways. For example, we could:

    compile it to an executable,
    run it directly (i.e. the traditional sense of "interpret"),
    pretty print it,
    compress and archive it,
    or do nothing at all with it! 

Each of those options corresponds to a different interpreter.

Let's try to come up with some sort of abstraction that represents the essence of a syntax tree. Abstractions always begin from specific examples, so let's invent our own toy programming language
and try to represent it as a data type.

Our toy language will only have three commands:

output b -- prints a "b" to the console
bell     -- rings the computer's bell
done     -- end of execution

So we represent it as a syntax tree where subsequent commands are leaves of prior commands: 
-}
data Toy b next =
    Output b next
  | Bell next
  | Done
{-jhw: Added this in attempt to run 'program' but all I needed wast instance Show FixE.
instance (Show b, Show next) => Show (Toy b next) where
  show Done = "Done"
  show (Bell next) = "Bell next"
  show (Output b next) = "Output b next"-}
{-
Notice how the Done command has no leaf since it must be the last command.

Then I could write a sample program that I might want to pass to an interpreter:
-- output 'A'
-- done
-}
test :: Toy a (Toy Char (Toy b next))
test = Bell (Output 'A' Done) 
{-... but unfortunately this doesn't work because every time I want to add a command, it changes the type: -}
-- bell
-- output 'A'
-- done

test2 :: Toy a (Toy Char (Toy b next))
test2 = Bell (Output 'A' Done) 

{-Fortunately, we can cheat and use the following data type to wrap as many Toys as we want into the same data type: -}
data Cheat f = Cheat (f(Cheat f))

{-With Cheat we've defined a stream of functors that will only end when it gets to the Done constructor. Fortunately, Cheat already exists in Haskell and goes by another name: -}
data Fix f = Fix (f(Fix f))



{-It's named Fix because it is "the fixed point of a functor".

With Fix in hand, now we can fix our example programs: -}
test1Fix :: Fix (Toy Char)
test1Fix = Fix(Output 'A'(Fix Done))

test2Fix :: Fix(Toy Char)
test2Fix = Fix (Bell (Fix (Output 'A' (Fix Done)))) 


{-Now they have the same type. Perfect! Or is it?

There's still a problem. This approach only works if you can use the Done constructor to terminate every chain of functors. Unfortunately, programmers don't often have the luxury of writing the entire program from start to finish. We often just want to write subroutines that can be called from within other programs and our Fix trick doesn't let us write a subroutine without terminating the entire program.

Ok, so let's hack together a quick and dirty fix to work around this problem. Our subroutine finished but we are not ready to call Done, so instead we throw an exception and let whoever calls our subroutine catch it and resume from where we left off: -}
data FixE f e = Fix' (f(FixE f e))
              | Throw e
instance {-(Show f, Show e) =>-} Show (FixE f e) where
  --show (Fix' (f(FixE f e))) = "Fix' (f(FixE f e))"
  show (Fix' _) = "Fix' (f(FixE f e))"
  show (Throw _) = "Throw e"
  

{-Then we write a catch function: -}
catch :: (Functor f) => FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Fix' x) f = Fix'(fmap(flip catch f)x)
catch (Throw e) f = f e

{-We can only use this if Toy b is a functor, so we muddle around until we find something that type-checks (and satisfies the Functor laws): -}
instance Functor (Toy b) where
  fmap f (Output x next) = Output x (f next)
  fmap f (Bell next) = Bell (f next)
  fmap f Done = Done

{-Now we can write code that can be caught and resumed: -}
data IncompletException  = IncompletException
subroutine :: FixE(Toy Char) IncompletException
subroutine = Fix' (Output 'A'(Throw IncompletException))

program :: FixE (Toy Char) e
program = subroutine `catch` (\_ -> Fix'(Bell(Fix' Done)))

-}
