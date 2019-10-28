{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE  #-}
{- |
The pdf for the book sample chapters in in dropbox.
Chapter 1 covers:
Isomorphism
-}
module ThinkingWithTypes.Chap1_AlgebraBehindTypes where

-- * Isomorphism
--
-- $ismorphism
--
-- Two datatypes are isomorphic is they have the same number of constructors, and and they can be converted directly into the other datatype.
--
-- The datatypes Spin and Bool will be made isomorphic in the following example.

-- | Like a Quark with up and down spin. For this example, 2 Down Spin make a DownQuark, while anything else remains 2 Quarks.
-- So I can convert Down into True, and use Bool to see if 2 Spin make a DownQuark. 
--
-- Note that it has a cardinality of 2, that is it has 2 constructors, each with a cardinality of 1. This matches the cardinality of Bool, which makes them isomorphic.
data Spin = Up | Down

-- | For testing.
instance Show Spin where
  show Up = "Up"
  show Down = "Down"

-- | An isomorphism between 2 types with == cardinality, is defined by a pair of functions to convert them back and forth. I have made them into a typeclass for convenience.
class Iso s t where
  to :: s -> t -- > 
  from :: t -> s

-- | They each have a cardinality of 2, so their are 2 possible isomorphisms between them.
-- Could have made this the opposite, by making Up as True, which is the other isomorphism that can be made between them.
instance Iso Bool Spin where
  to True = Down
  to False = Up
  from Up = False
  from Down = True
  
{- |
Given:
2 Spin

Task:
Check to see if the 2 Quarks combine to make a DownQuark, using their isomorphism with Bool as defined by their instance of 'Iso'. They do only if both had Down spin, which
means that this can be evaluated using && from Bool. 

Return:
Return True if both Spins are Down, otherwise False
-}
checkForDownQuark :: Spin -> Spin -> Bool
checkForDownQuark s1 s2 =
  (from s1) && (from s2)

-- | Run all 4 combinations of Spin, and see if they make a DownQuark. The Down Down combination is the only possible way.
--
-- Uses 'checkForDownQuark' to do the calculations.
--
-- Returns the results as a Tuple which checks (Up Up, Up Down, Down Up, Down Down) which results in (False, False, False, True).
runAllSpinCominations =
  let
    twoUpQuarks = checkForDownQuark Up Up
    
    upAndDownQuarks = checkForDownQuark Up Down

    downAndUpQuarks = checkForDownQuark Down Up

    twoDownQuarks = checkForDownQuark Down Down
  in
  (twoUpQuarks, upAndDownQuarks, downAndUpQuarks, twoDownQuarks)
    

-- ** TicTacToe board example
--
-- $ticTacToe
-- Explore product types through a TicTacToe board example. A TicTacToe board is a 3x3 board and would normally be declared like 'TicTacToe'.
-- The purpose of this exercise it to use higher level types to create the version 'TicTacToe2'. Supposedly this will simplify working with the board
-- though the book example does not show how.

-- | A tic tac toe board set up as a normal ADT product type, with each field of the board declared.
-- The purpose the datatype is to compare it to the 'TicTacToe2' which is declared with a type level system.
data TicTacToe a = TicTacToe
 {  topLeft :: a
  , topCenter :: a
  , topRight :: a
  , midLeft :: a
  , midCenter :: a
  , midRight :: a
  , botLeft :: a
  , botCenter :: a
  , botRight :: a
 }
 deriving Show

-- | Instantiate the 'TicTacToe' by supplying a value for each of the 9 positions on the board.
emptyTicTacToeBoard :: TicTacToe (Maybe Bool)
emptyTicTacToeBoard =
 TicTacToe
  Nothing Nothing Nothing
  Nothing Nothing Nothing
  Nothing Nothing Nothing
  

-- | Used in the declaration of the 'TicTacToe2' ADT. Assume that this sum type corresponds to each of the 3 rows and columns.
-- Not sure how it's use in the 'board' field of 'TicTacToe2' works.
data Three = One | Two | Three
 deriving (Eq, Ord, Enum, Bounded, Show)

-- | The tic tac toe ADT based on the 'Three' ADT. Not sure how it works so lets have a look at it's types in ghci:
--
-- >>> :t TicTacToe2
-- > (Three -> Three -> a) -> TicTacToe2 a
-- > So it take a functions that will process 2 'Three' ADT's, and return the 'a' polymorphic parameter,
-- > and supplies it for the 'board' field.
--
-- >>> :t board
-- > TicTacToe2 a -> Three -> Three -> a
-- > It takes a 'TicTacToe2' as a parmeter along with 2 'Three' params. Notice that the type of board as declared, says nothing about taking a 'TicTacToe2' param. 
data TicTacToe2 a = TicTacToe2
 { board :: Three -> Three -> a}

-- | Had to declare something to test out 'emptyTicTacToe2Board' but it is totally misleading and needs to be changed.
instance Show (TicTacToe2 a) where
  show (TicTacToe2 a ) = show One

-- | Initialize an empty 'TicTacToe2'. No idea how it works. How do I access individual board positions?
--
-- A note about 'const'. It is a function that returns the 1st parameter, and ignores the second parameter.
--
-- Let's check it's type at the repl as applied here, but without the 'TicTacToe2' constructor.
--
-- >>> :t const $ const Nothing
-- > :: b1 -> b2 -> Maybe a
--
-- And now have a look as applied with the 'TicTacToe2' constructor.
--
-- >>> :t TicTacToe2 $ const $ const Nothing
-- > :: TicTacToe2 (Maybe a)
-- > Notice that is is a Maybe a. That is because I am checking the type outside the context of the 'emptyTicTacToe2Board' fx.
--
-- See how that a is changed into a Bool when checked in that context.
-- >>> :t emptyTicTacToe2Board
-- > :: TicTacToe2 (Maybe Bool)
emptyTicTacToe2Board :: TicTacToe2 (Maybe Bool)
emptyTicTacToe2Board =
  TicTacToe2 $ const $ const Nothing


-- ** TicTacToe next
--
-- $ticTacToeNext
-- Play around with 'TicTacToe' and 'TicTacToe2' to see what I have, and compare it to them.
-- Don't need to actually implement a game, but rather see how it is that they are the same, supposedly.
