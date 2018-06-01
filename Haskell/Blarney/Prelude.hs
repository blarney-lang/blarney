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
  , zero
  , register
  , registerEn
  , ram
  , ramInit
  , ramTrueDual
  , ramTrueDualInit
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
sumList = tree (.+.) zero

-- Tree of bitwise-and
andList :: Bits a => [a] -> a
andList = tree and ones
  where and a b = unpack (pack a .&. pack b)

-- Tree of bitwise-or
orList :: Bits a => [a] -> a
orList = tree or zero
  where or a b = unpack (pack a .|. pack b)

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
  orList [sel ? (val, zero) | (sel, val) <- alts]

-- Index a list
index :: (KnownNat n, Bits a) => Bit n -> [a] -> a
index i xs = select [(i .==. fromInteger j, x) | (j, x) <- zip [0..] xs]

-- Mux
infixl 3 ?
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

-- All 0's
zero :: Bits a => a
zero = replicateBit 0

-- All 1's
ones :: Bits a => a
ones = replicateBit 0

-- Register
register :: Bits a => a -> a -> a
register init a = unpack (reg (pack init) (pack a))

-- Register with enable
registerEn :: Bits a => a -> Bit 1 -> a -> a
registerEn init en a =
  unpack (regEn (pack init) en (pack a))

-- Uninitialised RAM
-- (Reads new data on write)
ram :: (Bits a, Bits d) => (a, d, Bit 1) -> d
ram (a, d, en) = 
  unpack (ramPrim (sizeOf d) Nothing (pack a, pack d, en))

-- Initilaised RAM
-- (Reads new data on write)
ramInit :: (Bits a, Bits d) => String -> (a, d, Bit 1) -> d
ramInit init (a, d, en) =
  unpack (ramPrim (sizeOf d) (Just init) (pack a, pack d, en))

-- Uninitialised true dual-port RAM
-- (Reads new data on write)
-- (When read-address == write-address on different ports, read old data)
ramTrueDual :: (Bits a, Bits d) =>
               (a, d, Bit 1)
            -> (a, d, Bit 1)
            -> (d, d)
ramTrueDual (a0, d0, en0)
            (a1, d1, en1) = (unpack o0, unpack o1)
  where
    (o0, o1) = ramTrueDualPrim (sizeOf d0) Nothing
                 (pack a0, pack d0, en0)
                 (pack a1, pack d1, en1)

-- Initilaised true dual-port RAM
-- (Reads new data on write)
-- (When read-address == write-address on different ports, read old data)
ramTrueDualInit :: (Bits a, Bits d) =>
                   String
                -> (a, d, Bit 1)
                -> (a, d, Bit 1)
                -> (d, d)
ramTrueDualInit init (a0, d0, en0)
                     (a1, d1, en1) = (unpack o0, unpack o1)
  where
    (o0, o1) = ramTrueDualPrim (sizeOf d0) (Just init)
                 (pack a0, pack d0, en0)
                 (pack a1, pack d1, en1)
