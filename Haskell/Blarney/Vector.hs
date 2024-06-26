{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-|
Module      : Blarney.Vector
Description : A module for handling vectors
Copyright   : (c) Alexandre Joannou, 2019-2021
License     : MIT
Maintainer  : alexandre.joannou@gmail.com
Stability   : experimental

A blarney module for handling vectors
-}

module Blarney.Vector (
  -- * Vec
  Blarney.Vector.Vec (..)
  -- * 'Vec' constructors
, Blarney.Vector.newVec
, Blarney.Vector.genVec
, Blarney.Vector.fromList
, Blarney.Vector.replicate
, Blarney.Vector.replicateM
, Blarney.Vector.genWith
, Blarney.Vector.genWithM
, Blarney.Vector.cons
, Blarney.Vector.nil
--
, Blarney.Vector.append
, Blarney.Vector.concat
, Blarney.Vector.select
, Blarney.Vector.split
, Blarney.Vector.update
--
, Blarney.Vector.head
, Blarney.Vector.last
, Blarney.Vector.tail
, Blarney.Vector.init
, Blarney.Vector.take
, Blarney.Vector.takeTail
, Blarney.Vector.drop
, Blarney.Vector.takeAt
--
, Blarney.Vector.rotateL
, Blarney.Vector.rotateR
, Blarney.Vector.rotateLBy
, Blarney.Vector.rotateRBy
, Blarney.Vector.reverse
--
, Blarney.Vector.elem
, Blarney.Vector.any
, Blarney.Vector.all
, Blarney.Vector.or
, Blarney.Vector.and
--
, Blarney.Vector.countElem
, Blarney.Vector.countIf
, Blarney.Vector.find
--
, Blarney.Vector.zip
, Blarney.Vector.zip3
, Blarney.Vector.zip4
, Blarney.Vector.unzip
--
, Blarney.Vector.vectorise
, Blarney.Vector.map
, Blarney.Vector.mapM
, Blarney.Vector.mapM_
, Blarney.Vector.zipWith
, Blarney.Vector.zipWithM
, Blarney.Vector.zipWithM_
, Blarney.Vector.zipWith3
, Blarney.Vector.zipWith3M
, Blarney.Vector.zipAny
, Blarney.Vector.zipWithAny
, Blarney.Vector.zipWithAny3
--
, Blarney.Vector.tree
, Blarney.Vector.tree1
--
, Blarney.Vector.foldr
, Blarney.Vector.foldl
, Blarney.Vector.foldr1
, Blarney.Vector.foldl1
--
, Blarney.Vector.scanr
, Blarney.Vector.sscanr
, Blarney.Vector.scanl
, Blarney.Vector.sscanl
-- TODOs
-- toChunks
-- shiftInAt0, shiftInAtN, shiftOutFrom0, shiftOutFromN
-- findElem, findIndex, rotateBitsBy, countOnesAlt
-- transpose, transposeLN
-- mapPairs, joinActions
-- mapAccumL, mapAccumR
) where

-- Blarney imports
import Blarney
import Blarney.Option
import Blarney.Core.BV
import Blarney.TypeFamilies

import qualified Data.List as L
import qualified Data.Type.Bool as B
import Data.Type.Equality

-- | 'Vec' type
data Vec (n :: Nat) a = Vec { toList :: [a] } deriving (Generic, FShow)

instance (KnownNat n, Bits a) => Bits (Vec n a) where
  type SizeOf (Vec n a) = n * SizeOf a
  sizeOf xs = valueOf @n * sizeOf (error "sizeOf: _|_ " :: a)
  pack x
    | null xs = FromBV $ constBV 0 0
    | otherwise = FromBV $ L.foldr1 concatBV $
                    fmap toBV $ fmap pack $ L.reverse xs
    where xs = toList x
  unpack x = Vec xs
    where
      len = valueOf @n
      xs  = [ let bits = unsafeSlice ((w*i)-1, w*(i-1)) x
                  elem = unpack bits
                  w    = sizeOf elem
              in elem
            | i <- [1..len] ]
  nameBits nm xs = Vec [ nameBits (nm ++ "_vec_" ++ show i) b
                       | (i,b) <- L.zip [0..] (toList xs) ]

instance (KnownNat n, Interface a) => Interface (Vec n a) where
  toIfc vec = (tm, ty)
    where
      tm = encode (valueOf @n) (toList vec)
      ty = L.foldr IfcTypeProduct IfcTypeUnit (L.replicate (valueOf @n) t)
      t = IfcTypeField portEmpty (toIfcType (undefined :: a))
      encode 0 _ = IfcTermUnit
      encode i ~(x:xs) = IfcTermProduct (toIfcTerm x) (encode (i-1) xs)
  fromIfc term = Vec $ decode (valueOf @n) term
    where
      decode 0 _ = []
      decode i ~(IfcTermProduct x0 x1) = fromIfcTerm x0 : decode (i-1) x1

instance Functor (Vec n) where
  fmap = Blarney.Vector.map

-- | Generate a 'Vec' of size 'n' initialized with 'undefined' in each element
newVec :: forall n a. KnownNat n => Vec n a
newVec = Vec (L.replicate (valueOf @n) undefined)

-- | Generate a 'Vec' of size 'n' initialized with integers from '0' to 'n-1'
genVec :: forall n. KnownNat n => Vec n Integer
genVec = Vec (L.take (valueOf @n) [0..])

-- | Convert a list to a vector, after  size check
fromList :: forall n a. KnownNat n => [a] -> Vec n a
fromList xs
  | valueOf @n == length xs = Vec xs
  | otherwise = error ("Blarney.Vector.fromList: " ++
      "list size does not match vector size")

-- | Generate a 'Vec' with each element initialized to the given value
replicate :: forall n a. KnownNat n => a -> Vec n a
replicate x = Vec (L.replicate (valueOf @n) x)

replicateM :: forall n a m. (Monad m, KnownNat n) => m a -> m (Vec n a)
replicateM x = do
  xs <- Blarney.replicateM (valueOf @n) x
  return $ Vec xs

-- | Generate a 'Vec' from the given function 'f' applied to integers from '0'
--   to 'n-1'
genWith :: forall n a. KnownNat n => (Integer -> a) -> Vec n a
genWith f = Vec (L.take (valueOf @n) $ L.map f [0..])

genWithM :: forall n a m. (Monad m, KnownNat n) =>
              (Integer -> m a) -> m (Vec n a)
genWithM f = do
  xs <- Blarney.mapM f [0 .. toInteger (valueOf @n - 1)]
  return $ Vec xs

-- | Construct a new 'Vec' from a new element and an exisiting 'Vec'. The new
--   element is the head of the new 'Vec'.
cons :: a -> Vec n a -> Vec (n+1) a
cons x xs = Vec (x : toList xs)

-- | The "nil" 'Vec'
nil :: Vec 0 a
nil = Vec []

-- | Append the second 'Vec' to the first 'Vec'
append :: Vec n a -> Vec m a -> Vec (n+m) a
append xs ys = Vec (toList xs ++ toList ys)

-- | Concatenate a 'Vec' of 'Vec's into one flattened 'Vec'
concat :: Vec m (Vec n a) -> Vec (m*n) a
concat xss = Vec (L.concatMap toList (toList xss))

-- | Select the element from a 'Vec' at the given type-level index
select :: forall i n a. (KnownNat i, (i+1) <= n) => Vec n a -> a
select xs = toList xs !! valueOf @i

-- | Return a pair of 'Vec', the first element being the 'Vec' of length 'n0'
--   prefix of the given 'Vec' of length 'n', and the second element being the
--   'Vec of length 'n1' suffix of the given 'Vec' of length 'n'
split :: forall n n0 n1 a. (KnownNat n0, (n0+n1) ~ n) =>
         Vec n a -> (Vec n0 a, Vec n1 a)
split xs = (Vec v0, Vec v1)
           where (v0, v1) = splitAt (valueOf @n0) (toList xs)

-- | Generate a new 'Vec' from the given 'Vec' with the element at index 'idx'
--   updated
update :: Vec n a -> Int -> a -> Vec n a
update xs idx x = Vec (start ++ (x:end))
                  where (start, (_:end)) = splitAt idx (toList xs)

-- | Return the head element of the given 'Vec' (element at index 0)
head :: (1 <= n) => Vec n a -> a
head = L.head . toList

-- | Return the last element of the given 'Vec' (element at last index)
last :: (1 <= n) => Vec n a -> a
last = L.last . toList

-- | Return the given 'Vec' with its head element removed
tail :: Vec (n+1) a -> Vec (n) a
tail xs = Vec (L.tail $ toList xs)

-- | Return the given 'Vec' with its last element removed
init :: Vec (n+1) a -> Vec (n) a
init xs = Vec (L.init $ toList xs)

-- | Return the 'Vec' composed of the first 'm' elements of the given 'Vec'
take :: forall n m a. (KnownNat m, m <= n) => Vec n a -> Vec m a
take xs = Vec (L.take (valueOf @m) (toList xs))

-- | Return the 'Vec' composed of the last 'm' elements of the given 'Vec'
takeTail :: forall n m a. (KnownNat m, m <= n) => Vec n a -> Vec m a
takeTail xs = Vec (L.drop (L.length elems - valueOf @m) elems)
  where elems = toList xs

-- | Drop the first 'i' elements of the given 'Vec'
drop :: forall i n a. (KnownNat i, i <= n) => Vec n a -> Vec (n-i) a
drop xs = Vec (L.drop (valueOf @i) (toList xs))

-- | Return the 'Vec' composed of the 'm' elements of the given 'Vec' starting
--   at index 'idx'
takeAt :: forall n m a. (KnownNat n, KnownNat m, m <= n) =>
          Int -> Vec n a -> Vec m a
takeAt idx xs
  | valueOf @m > valueOf @n - idx = error "not enough elements"
  | otherwise = Vec (L.take (valueOf @m) end)
                where (_, end) = L.splitAt idx (toList xs)

-- | Return a 'Vec' image of the given 'Vec' with its elements rotated left by
--   one, with the head element becoming the last element
rotateL :: Vec n a -> Vec n a
rotateL xs = Vec (L.tail xss ++ [L.head xss])
             where xss = toList xs

-- | Return a 'Vec' image of the given 'Vec' with its elements rotated right by
--   one, with the last element becoming the head element
rotateR :: Vec n a -> Vec n a
rotateR xs = Vec (L.last xss : L.init xss)
             where xss = toList xs

-- Internal function: rotate vector left/right
rotateBy :: Bits a => Bool -> Bit m -> Vec n a -> Vec n a
rotateBy left i =
    Vec
  . L.map unpack
  . L.map unsafeFromBitList
  . L.transpose
  . L.map unsafeToBitList
  . L.map (`rot` i)
  . L.map unsafeFromBitList
  . L.transpose
  . L.map unsafeToBitList
  . L.map pack
  . toList
  where rot = if left then rotr else rotl

-- | Return a 'Vec' image of the given 'Vec' with its elements rotated right by
--   'i', with the last 'i' elements becoming the first 'i' elements
rotateRBy :: Bits a => Bit m -> Vec n a -> Vec n a
rotateRBy = rotateBy False

-- | Return a 'Vec' image of the given 'Vec' with its elements rotated left by
--   'i', with the first 'i' elements becoming the last 'i' elements
rotateLBy :: Bits a => Bit m -> Vec n a -> Vec n a
rotateLBy = rotateBy True

-- | Reverse the given 'Vec'
reverse :: Vec n a -> Vec n a
reverse xs = Vec (L.reverse $ toList xs)

-- | Check that the given value is and element of the given 'Vec'
elem :: Cmp a => a -> Vec n a -> Bit 1
elem x = Blarney.Vector.any (.==. x)

-- | Check that the given predicate holds for any element of the given 'Vec'
any :: (a -> Bit 1) -> Vec n a -> Bit 1
any pred = Blarney.Vector.or . fmap pred

-- | Check that the given predicate holds for all element of the given 'Vec'
all :: (a -> Bit 1) -> Vec n a -> Bit 1
all pred = Blarney.Vector.and . fmap pred

-- | Reduces a 'Vec' of 'Bit 1' by "or-ing" its elements
or :: Vec n (Bit 1) -> Bit 1
or = Blarney.Vector.tree (.||.) false

-- | Reduces a 'Vec' of 'Bit 1' by "and-ing" its elements
and :: Vec n (Bit 1) -> Bit 1
and = Blarney.Vector.tree (.&&.) true

-- | Return the number of elements of 'Vec' which are equal to the given value
countElem :: (Cmp a, 1 <= n, _) => a -> Vec n a -> Bit (Log2Ceil n + 1)
countElem e = countIf (.==. e)

-- | Return the number of elements of 'Vec' for which the given predicate holds
countIf :: (1 <= n, _) => (a -> Bit 1) -> Vec n a -> Bit (Log2Ceil n + 1)
countIf p =
  Blarney.Vector.tree (+) 0 . fmap (\x -> if p x then 1 else 0)

-- | Return a 'some' 'Option' with the first element in the given 'Vec' that
--   satisfies the given predicate, or 'none' if no such element is found
find :: Bits a => (a -> Bit 1) -> Vec n a -> Option a
find p xs = L.foldl (\c x -> if p x then some x else c) none (toList xs)

-- | Return a 'Vec' of pairs of elements at the same index in both given 'Vec's
zip :: Vec n a -> Vec n b -> Vec n (a, b)
zip xs ys = Vec $ L.zip (toList xs) (toList ys)

-- | Return a 'Vec' of tuple-3 of elements at the same index in the given 'Vec's
zip3 :: Vec n a -> Vec n b -> Vec n c -> Vec n (a, b, c)
zip3 xs ys zs = Vec $ L.zip3 (toList xs) (toList ys) (toList zs)

-- | Return a 'Vec' of tuple-4 of elements at the same index in the given 'Vec's
zip4 :: Vec n a -> Vec n b -> Vec n c -> Vec n d -> Vec n (a, b, c, d)
zip4 ws xs ys zs = Vec $ L.zip4 (toList ws) (toList xs) (toList ys) (toList zs)

-- | Return a 'Vec' of pairs of elements at the same index in both given 'Vec's
--   with the resulting 'Vec' being as long as the smaller input 'Vec'
zipAny :: Vec n a -> Vec m b -> Vec (Min n m) (a, b)
zipAny xs ys = Vec $ L.zip (toList xs) (toList ys)

-- | Return a pair of 'Vec' from a given 'Vec' of pairs
unzip :: Vec n (a, b) -> (Vec n a, Vec n b)
unzip xys = (Vec xs, Vec ys)
            where (xs, ys) = L.unzip (toList xys)

-- | Appy list function to elements of vector
vectorise :: ([a] -> [b]) -> Vec n a -> Vec n b
vectorise f v
  | L.length inputs == L.length outputs = Vec outputs
  | otherwise =
      error "Blarney.Vector.onElems: function does not preserve length"
  where
    inputs = toList v
    outputs = f inputs

-- | Map a function over the given 'Vec'
map :: (a -> b) -> Vec n a -> Vec n b
map f xs = Vec $ L.map f (toList xs)

mapM :: Monad m => (a -> m b) -> Vec n a -> m (Vec n b)
mapM f xs = do
  xs <- Blarney.mapM f (toList xs)
  return $ Vec xs

mapM_ :: Monad m => (a -> m b) -> Vec n a -> m ()
mapM_ f xs = do
  _ <- Blarney.mapM f (toList xs)
  return ()

-- | Return a 'Vec', result of mapping a function over the two input 'Vec's
zipWith :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
zipWith f xs ys = Vec $ L.map (uncurry f) (L.zip (toList xs) (toList ys))

zipWithM :: Monad m => (a -> b -> m c) -> Vec n a -> Vec n b -> m (Vec n c)
zipWithM f xs ys = do
  zs <- Blarney.mapM (uncurry f) (L.zip (toList xs) (toList ys))
  return $ Vec zs

zipWithM_ :: Monad m => (a -> b -> m c) -> Vec n a -> Vec n b -> m ()
zipWithM_ f xs ys = do
  _ <- Blarney.mapM (uncurry f) (L.zip (toList xs) (toList ys))
  return ()

-- | Return a 'Vec', result of mapping a function over the two input 'Vec's,
--   truncated to the length of the shortest one
zipWithAny :: (a -> b -> c) -> Vec n a -> Vec m b -> Vec (Min n m) c
zipWithAny f xs ys = Vec $ L.map (uncurry f) (L.zip (toList xs) (toList ys))

-- | Return a 'Vec', result of mapping a function over the three input 'Vec's
zipWith3 :: (a -> b -> c -> d) -> Vec n a -> Vec n b -> Vec n c -> Vec n d
zipWith3 f xs ys zs = Vec $ L.map (\(x, y, z) -> f x y z)
                                  (L.zip3 (toList xs) (toList ys) (toList zs))

zipWith3M :: Monad m => (a -> b -> c -> m d) -> Vec n a -> Vec n b -> Vec n c
                        -> m (Vec n d)
zipWith3M f xs ys zs = do
  res <- Blarney.mapM (\(x, y, z) -> f x y z)
                      (L.zip3 (toList xs) (toList ys) (toList zs))
  return $ Vec res

-- | Return a 'Vec', result of mapping a function over the three input 'Vec's,
--   truncated to the length of the shortest one
zipWithAny3 :: (a -> b -> c -> d) -> Vec n0 a -> Vec n1 b -> Vec n2 c
            -> Vec (Min n0 (Min n1 n2)) d
zipWithAny3 f xs ys zs = Vec $ L.map (\(x, y, z) -> f x y z)
                               (L.zip3 (toList xs) (toList ys) (toList zs))

-- | Tree reduction for vectors
tree :: (a -> a -> a) -> a -> Vec n a -> a
tree f z = Blarney.tree f z . toList

-- | Tree reduction for nonempty vectors
tree1 :: (a -> a -> a) -> Vec n a -> a
tree1 f = Blarney.tree1 f . toList

-- | Reduce a 'Vec' using the given function, starting with a provided seed and
--   the last element of the 'Vec'
foldr :: (a -> b -> b) -> b -> Vec n a -> b
foldr f seed xs = L.foldr f seed (toList xs)

-- | Reduce a 'Vec' using the given function, starting with a provided seed and
--   the first element of the 'Vec'
foldl :: (b -> a -> b) -> b -> Vec n a -> b
foldl f seed xs = L.foldl f seed (toList xs)

-- | Reduce a 'Vec' using the given function, starting with the last element of
--   the 'Vec' as the seed
foldr1 :: 1 <= n => (a -> a -> a) -> Vec n a -> a
foldr1 f xs = L.foldr1 f (toList xs)

-- | Reduce a 'Vec' using the given function, starting with the first element of
--   the 'Vec' as the seed
foldl1 :: 1 <= n => (a -> a -> a) -> Vec n a -> a
foldl1 f xs = L.foldl1 f (toList xs)

-- | Reduce a 'Vec' using the given function in a tree structure
fold :: 1 <= n => (a -> a -> a) -> Vec n a -> a
fold f xs = Blarney.tree1 f (toList xs)

-- | Apply a function over a 'Vec' starting with the given seed and the last
--   element, yielding a 'Vec' one element bigger than the provided one
scanr :: (a -> b -> b) -> b -> Vec n a -> Vec (n+1) b
scanr f seed xs = Vec $ L.scanr f seed (toList xs)

-- | Apply a function over a 'Vec' starting with the given seed and the last
--   element, dropping the new last element (provided seed), effectively
--   yielding a 'Vec' of the same size as the provided one
sscanr :: (a -> b -> b) -> b -> Vec n a -> Vec (n+1) b
sscanr f seed xs = Vec $ L.init (L.scanr f seed (toList xs))

-- | Apply a function over a 'Vec' starting with the given seed and the first
--   element, yielding a 'Vec' one element bigger than the provided one
scanl :: (b -> a -> b) -> b -> Vec n a -> Vec (n+1) b
scanl f seed xs = Vec $ L.scanl f seed (toList xs)

-- | Apply a function over a 'Vec' starting with the given seed and the first
--   element, dropping the new first element (provided seed), effectively
--   yielding a 'Vec' of the same size as the provided one
sscanl :: (b -> a -> b) -> b -> Vec n a -> Vec (n+1) b
sscanl f seed xs = Vec $ L.tail (L.scanl f seed (toList xs))

-- |Index a vector using a bit vector
instance (Interface a, KnownNat n) => Lookup (Vec m a) (Bit n) a where
  v ! i = toList v ! i

-- |Index a vector using an 'Int'
instance Lookup (Vec m a) Int a where
  v ! i = toList v ! i

-- |Index a vector using an 'Integer'
instance Lookup (Vec m a) Integer a where
  v ! i = toList v ! fromIntegral i
