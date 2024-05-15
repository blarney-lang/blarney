{- |
Module      : Blarney.Core.Ternary
Description : Ternary bit-vectors
Copyright   : (c) Matthew Naylor, 2024
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}

module Blarney.Core.Ternary
  ( Ternary
  , dontCare
  , integerToTernary
  , ternaryToInteger
  , concat
  , take
  , drop
  , select
  ) where

import Prelude hiding (concat, take, drop)
import Data.List qualified as L
import Data.Bits

-- | Ternary bit vectors represented as a list of width/value pairs.
-- 'Nothing' values represent don't care values.
type Ternary = [(Int, Maybe Integer)]

-- | Construct don't care bit-vector of given width
dontCare :: Int -> Ternary
dontCare w = [(w, Nothing)]

-- | Convert non-negative integer to ternary bit-vector of given width
integerToTernary :: Int -> Integer -> Ternary
integerToTernary w i
  | i < 0 = error "integerToTernary: non-negative integer expected"
  | w == 0 = []
  | otherwise = [(w, Just i)]

-- | Convert ternary bit-vector to an integer, with don't cares converted to 0s
ternaryToInteger :: Ternary -> Integer
ternaryToInteger [] = 0
ternaryToInteger ((w, x):rest) = val .|. (ternaryToInteger rest `shiftL` w)
  where val = case x of { Nothing -> 0; Just val -> val }

-- | Concatenate ternary bit-vectors
concat :: Ternary -> Ternary -> Ternary
concat xs [] = xs
concat [] ys = ys
concat ((wx, x):xs) [(wy, y)] =
  case (x, y) of
    (Nothing, Nothing) -> (wx+wy, Nothing) : xs
    (Just x, Just y)   -> (wx+wy, Just ((x `shiftL` wy) .|. y)) : xs
    other              -> (wy, y) : (wx, x) : xs
concat xs (y:ys) = y : concat xs ys

-- | Drop lower bits of bit vector
drop :: Int -> Ternary -> Ternary
drop 0 xs = xs
drop n [] = []
drop n ((w, x):rest)
  | n >= w = drop (n-w) rest
  | otherwise =
      case x of
        Nothing -> (w-n, Nothing) : rest
        Just val -> (w-n, Just (val `shiftR` n)) : rest

-- | Obtain lower bits of bit vector
take :: Int -> Ternary -> Ternary
take 0 xs = xs
take n [] = []
take n ((w, x):rest)
  | n >= w = (w, x) : take (n-w) rest
  | otherwise =
      case x of
        Nothing -> [(n, Nothing)]
        Just val -> [(n, Just (val .&. ((1 `shiftL` n) - 1)))]

-- | Bit-range selection of ternary bit-vector
select :: Int -> Int -> Ternary -> Ternary
select hi lo = take (hi-lo+1) . drop lo
