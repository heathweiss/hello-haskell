{-# LANGUAGE TypeOperators #-}
--used for data ( f:+: g) = L1 f | R1 g

{- |
From: https://www.stackbuilders.com/tutorials/haskell/generics/
-}
module Generics.GenericsExplained where

data V1

data U1 = U1

data ( f :+: g) = L1 f | R1 g

data (f :*: g) = f :*: g
