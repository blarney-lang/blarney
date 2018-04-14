{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeOperators     #-}

-- Prelude of commonly-used components and combinators
module Blarney.Prelude
  ( tree1
  , tree
  , orList
  , andList
  , sumList
  , select
  , index
  , (?)
  , o
  , (.==.)
  , (.!=.)
  ) where

import Prelude
import Blarney.Bit
import Blarney.Bits
import Blarney.IfThenElse
import GHC.TypeLits

-- Parallel reduce for a commutative an associative operator
-- Input list must be non-empty
tree1 :: (a -> a -> a) -> [a] -> a
tree1 f [x] = x
tree1 f (x:y:ys) = tree1 f (ys ++ [f x y])

-- Monadic version of tree1
treeM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
treeM1 f [x] = return x
treeM1 f (x:y:ys) =
  do z <- f x y
     treeM1 f (ys ++ [z])

-- Like 'tree1', but input list may be empty,
-- in which case the zero element is returned
tree :: (a -> a -> a) -> a -> [a] -> a
tree f z xs = if null xs then z else tree1 f xs

-- Adder tree
sumList :: KnownNat n => [Bit n] -> Bit n
sumList = tree (.+.) low

-- Tree of bitwise-and
andList :: Bits a => [a] -> a
andList = unpack . tree (.&.) high . map pack

-- Tree of bitwise-or
orList :: Bits a => [a] -> a
orList = unpack . tree (.|.) low . map pack

-- Tree with specified branching factor
treeB1 :: Int -> ([a] -> a) -> [a] -> a
treeB1 b f [x] = x
treeB1 b f xs = treeB1 b f (drop b xs ++ [f (take b xs)])

-- Like 'treeB1', but input list may be empty,
-- in which case the zero element is returned
treeB :: Int -> ([a] -> a) -> a -> [a] -> a
treeB b f z xs = if null xs then z else treeB1 b f xs

-- One-hot select
select :: Bits a => [(Bit 1, a)] -> a
select alts =
  unpack (orList [replicateBit sel .&. pack val | (sel, val) <- alts])

-- Index a list
index :: (KnownNat n, Bits a) => Bit n -> [a] -> a
index i xs = select [(i .==. fromInteger j, x) | (j, x) <- zip [0..] xs]

-- Mux
(?) :: Bits a => Bit 1 -> (a, a) -> a
c ? (a, b) = unpack (mux c (pack a) (pack b))

-- Function composition
infixr 9 `o`
o :: (a -> b) -> (c -> a) -> c -> b
(f `o` g) x = f (g x)

-- Equality
infix 4 .==.
(.==.) :: Bits a => a -> a -> Bit 1
a .==. b = pack a `eq` pack b

-- Disequality
infix 4 .!=.
(.!=.) :: Bits a => a -> a -> Bit 1
a .!=. b = pack a `neq` pack b
