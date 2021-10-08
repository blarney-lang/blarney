{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Blarney.Core.Prelude
Description : Commonly used HDL functions
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}
module Blarney.Core.Prelude
  ( tree1           -- Tree fold (for non-empty lists)
  , treeM1          -- Monadic tree fold (for non-empty lists)
  , tree            -- Tree fold
  , orList          -- Bitwise-or tree
  , andList         -- Bitwise-and tree
  , sumList         -- Adder tree
  , select          -- One-hot selection returning bits
  , selectList      -- One-hot selection returning list
  , listIndex       -- Index a list of bit-vectors using a bit-vector
  , (?)             -- Ternary conditional operator
  , o               -- Function composition
  , (===)           -- Generic equality
  , (=!=)           -- Generic disequality
  , zero            -- Generic zero
  , ones            -- Generic all-ones
  , dontCare        -- Generic don't care
  , delay           -- Generic register
  , buffer          -- Generic register with don't care initialiser
  , old             -- Generic register with don't care initialiser
  , delayEn         -- Generic register with enable
  , binaryEncode    -- One-hot to binary encoder
  , firstHot        -- Isolate first hot bit in vector
  , merge           -- Merge input values according to a given merging strategy
  ) where

import Prelude
import GHC.TypeLits
import Data.List (transpose)
import Blarney.Core.Prim
import Blarney.Core.BV
import Blarney.Core.Bit
import Blarney.Core.Bits
import Blarney.Core.IfThenElse

-- |Parallel reduce for a commutative and associative operator.
-- Input list must be non-empty.
tree1 :: (a -> a -> a) -> [a] -> a
tree1 f [x] = x
tree1 f (x:y:ys) = tree1 f (ys ++ [f x y])

-- |Monadic version of tree1
treeM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
treeM1 f [x] = return x
treeM1 f (x:y:ys) =
  do z <- f x y
     treeM1 f (ys ++ [z])

-- |Like 'tree1', but input list may be empty,
-- in which case the zero element is returned
tree :: (a -> a -> a) -> a -> [a] -> a
tree f z xs = if null xs then z else tree1 f xs

-- |Adder tree
sumList :: Num a => [a] -> a
sumList = tree (+) 0

-- |Tree of bitwise-and
andList :: Bits a => [a] -> a
andList = tree and ones
  where and a b = unpack (pack a .&. pack b)

-- |Tree of bitwise-or
orList :: Bits a => [a] -> a
orList = tree or zero
  where or a b = unpack (pack a .|. pack b)

-- |Tree with specified branching factor
treeB1 :: Int -> ([a] -> a) -> [a] -> a
treeB1 b f [x] = x
treeB1 b f xs = treeB1 b f (drop b xs ++ [f (take b xs)])

-- |Like 'treeB1', but input list may be empty,
-- in which case the zero element is returned
treeB :: Int -> ([a] -> a) -> a -> [a] -> a
treeB b f z xs = if null xs then z else treeB1 b f xs

-- |One-hot select
select :: Bits a => [(Bit 1, a)] -> a
select alts =
  orList [sel ? (val, zero) | (sel, val) <- alts]

-- |Variant of 'select' where right-hand-side is a list
selectList :: Bits a => [(Bit 1, [a])] -> [a]
selectList alts =
  map orList $ transpose
    [map (\x -> cond ? (x, zero)) rhs | (cond, rhs) <- alts]

-- |Index a list
listIndex :: (KnownNat n, Bits a) => Bit n -> [a] -> a
listIndex _ [] = dontCare
listIndex i xs = unpack $ mux i (pack <$> xs)

-- |Ternary conditional operator (generic multiplexer)
infixl 3 ?
(?) :: Bits a => Bit 1 -> (a, a) -> a
c ? (a, b) = unpack (mux c [pack b, pack a])

-- |Overloaded if-then-else
instance {-# OVERLAPPABLE #-} Bits a => IfThenElse (Bit 1) a where
  ifThenElse cond a b = cond ? (a, b)

-- |Function composition
infixr 9 `o`
o :: (a -> b) -> (c -> a) -> c -> b
(f `o` g) x = f (g x)

-- |Generic equality
infix 4 ===
(===) :: Bits a => a -> a -> Bit 1
a === b = pack a .==. pack b

-- |Generic disequality
infix 4 =!=
(=!=) :: Bits a => a -> a -> Bit 1
a =!= b = pack a .!=. pack b

-- |All 0's
zero :: forall a. Bits a => a
zero = unpack $ FromBV $ constBV w 0
  where w = sizeOf (error "_|_" :: a)

-- |All 1's
ones :: forall a. Bits a => a
ones = unpack $ FromBV $ constBV w ((2^w) - 1)
  where w = sizeOf (error "_|_" :: a)

-- |Don't care
dontCare :: forall a. Bits a => a
dontCare = unpack $ FromBV $ dontCareBV w
  where w = sizeOf (error "_|_" :: a)

-- |Generic register
delay :: Bits a => a -> a -> a
delay init a = unpack (reg (pack init) (pack a))

-- |Generic register with don't care initialiser
buffer :: Bits a => a -> a
buffer = delay dontCare

-- |Generic register with don't care initialiser
old :: Bits a => a -> a
old = delay dontCare

-- |Generic register with enable
delayEn :: Bits a => a -> Bit 1 -> a -> a
delayEn init en a =
  unpack (regEn (pack init) en (pack a))

-- | One-hot to binary encoder
binaryEncode :: Bit (2^n) -> Bit n
binaryEncode xs = unsafeFromBitList $ encode $ unsafeToBitList xs
  where
    encode [_] = []
    encode as  = zipWith (.|.) (encode ls) (encode rs) ++ [orList rs]
      where (ls, rs) = splitAt (length as `div` 2) as

-- | Isolate first hot bit in a bit vector
firstHot :: KnownNat n => Bit n -> Bit n
firstHot x = x .&. (inv x + 1)

merge :: (KnownNat (SizeOf a), Bits a) => MergeStrat -> [(Bit 1, a)] -> a
merge strat ins = unpack $ mergeBit strat ins'
  where ins' = map (\(en, x) -> (en, pack x)) ins
