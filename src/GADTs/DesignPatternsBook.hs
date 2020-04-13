{-# LANGUAGE GADTs #-}
module GADTs.DesignPatternsBook() where

data Expr t where  -- phantom 't'
  -- built-in smart constructors
  I :: Int  -> Expr Int
  B :: Bool -> Double -> Expr Int
  Add :: Expr Int -> Expr Int -> Expr Int


eval :: Expr t -> t
eval (I v) = v
eval (B v x) = 1
eval (Add x y) = (eval x) + (eval y)

runEval = eval (Add (I 10) (I 12))
runEval2 = eval (Add (I 10) (B True 2.3))
