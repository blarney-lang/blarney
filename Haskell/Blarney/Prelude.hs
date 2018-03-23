{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies #-}

-- Prelude of commonly-used components and combinators
module Blarney.Prelude
  ( tree1
  , tree
  , orList
  , andList
  , sumList
  , select
  , (?)
  , (!)
  ) where

import Blarney.Bit
import Blarney.Bits
import GHC.TypeLits

-- Parallel reduce for a commutative an associative operator
-- Input list must be non-empty
tree1 :: (a -> a -> a) -> [a] -> a
tree1 f [x] = x
tree1 f (x:y:ys) = tree1 f (ys ++ [f x y])

-- Like 'tree1', but input list may be empty,
-- in which case the zero element is returned
tree :: (a -> a -> a) -> a -> [a] -> a
tree f z xs = if null xs then z else tree1 f xs

-- Tree of bitwise-and
andList :: KnownNat n => [Bit n] -> Bit n
andList = tree (.&.) high

-- Tree of bitwise-or
orList :: KnownNat n => [Bit n] -> Bit n
orList = tree (.|.) low

-- Adder tree
sumList :: KnownNat n => [Bit n] -> Bit n
sumList = tree (.+.) low

-- One-hot select
select :: KnownNat n => [(Bit 1, Bit n)] -> Bit n
select alts = orList [replicateBit sel .&. val | (sel, val) <- alts]

-- Mux
(?) :: Bits a => Bit 1 -> (a, a) -> a
c ? (a, b) = unpack (mux c (pack a) (pack b))

-- Reverse function application
infixl 9 !
(!) :: a -> (a -> b) -> b
x!f = f x
