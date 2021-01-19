{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ConstraintKinds      #-}

{-|
Module      : Blarney.Core.FShow
Description : For implementing display statements
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

Blarney's 'Blarney.RTL.display' statement can display values of any
types in the 'FShow' class.  The 'FShow' class supports generic
deriving.
-}
module Blarney.Core.FShow where

-- Blarney imports
import Blarney.Core.BV
import Blarney.Core.Bit
import Blarney.Core.Prim (DisplayArg(..), DisplayArgRadix(..))

-- Standard imports
import Prelude
import Data.Monoid
import GHC.Generics

-- | Format for displaying values in simulation
data Format = Format [FormatItem]

-- | A format item is a display arg, tagged with a bit vector
type FormatItem = (DisplayArg, BV)

-- | Convert a string to a format
formatString :: String -> Format
formatString str = Format [(DisplayArgString str, noBV)]
  where noBV = error "Blarney.Core.FShow: string has no bit vector"

-- | Convert bit vector to a format
formatBit :: DisplayArgRadix -> Maybe Int -> Bit n -> Format
formatBit radix pad b =
    Format [(DisplayArgBit w radix pad True, bv)]
  where
    bv = toBV b
    w = bvPrimOutWidth bv

-- | Format bit vector in binary with given amount of zero padding
formatBin :: Int -> Bit n -> Format
formatBin pad = formatBit Bin (Just pad)

-- | Format bit vector in decimal with given amount of zero padding
formatDec :: Int -> Bit n -> Format
formatDec pad = formatBit Dec (Just pad)

-- | Format bit vector in hex with given amount of zero padding
formatHex :: Int -> Bit n -> Format
formatHex pad = formatBit Hex (Just pad)

-- FShow
class FShow a where
  fshow :: a -> Format

  fshowList :: [a] -> Format
  fshowList xs = fshow "[" <> list xs <> fshow "]"
    where
      list [] = mempty
      list [x] = fshow x
      list (x:xs) = fshow x <> fshow "," <> list xs

  default fshow :: (Generic a, GFShow (Rep a)) => a -> Format
  fshow x = gfshow False (from x)

-- | Format concatention
instance Semigroup Format where
  Format a <> Format b = Format (a ++ b)

-- | Empty format
instance Monoid Format where
  mempty = Format []

instance FShow Char where
  fshow c = formatString [c]
  fshowList cs = formatString cs

instance FShow (Bit n) where
  fshow = formatDec 0

instance FShow Int where
  fshow i = formatString (show i)

instance FShow Integer where
  fshow i = formatString (show i)

instance FShow Format where
  fshow f = f

instance FShow a => FShow [a] where
  fshow = fshowList

instance FShow () where
  fshow _ = fshow "()"

instance (FShow a, FShow b) => FShow (a, b) where
  fshow (a, b) = fshow "(" <> fshow a <> fshow "," <> fshow b <> fshow ")"

instance (FShow a, FShow b, FShow c) => FShow (a, b, c) where
  fshow (a, b, c) =
      fshow "("
   <> fshow a <> fshow ","
   <> fshow b <> fshow ","
   <> fshow c <> fshow ")"

instance (FShow a, FShow b, FShow c, FShow d) => FShow (a, b, c, d) where
  fshow (a, b, c, d) =
      fshow "("
   <> fshow a <> fshow ","
   <> fshow b <> fshow ","
   <> fshow c <> fshow ","
   <> fshow d <> fshow ")"

instance (FShow a, FShow b, FShow c, FShow d, FShow e) =>
         FShow (a, b, c, d, e) where
  fshow (a, b, c, d, e) =
      fshow "("
   <> fshow a <> fshow ","
   <> fshow b <> fshow ","
   <> fshow c <> fshow ","
   <> fshow d <> fshow ","
   <> fshow e <> fshow ")"

instance (FShow a, FShow b, FShow c, FShow d, FShow e, FShow f) =>
         FShow (a, b, c, d, e, f) where
  fshow (a, b, c, d, e, f) =
      fshow "("
   <> fshow a <> fshow ","
   <> fshow b <> fshow ","
   <> fshow c <> fshow ","
   <> fshow d <> fshow ","
   <> fshow e <> fshow ","
   <> fshow f <> fshow ")"

-- Generic deriving for FShow
class GFShow f where
  -- First argument: are we showing a record constructor?
  gfshow :: Bool -> f a -> Format

instance GFShow U1 where
  gfshow isRec U1 = mempty

instance (GFShow a, GFShow b) => GFShow (a :*: b) where
  gfshow isRec (a :*: b) = gfshow isRec a <> fshow sep <> gfshow isRec b
    where sep = case isRec of { False -> " "; True -> ", " }

instance (GFShow a, Selector c) => GFShow (M1 S c a) where
  gfshow isRec y@(M1 x)
    | null (selName y) = fshow "(" <> gfshow False x <> fshow ")"
    | otherwise = fshow (selName y) <> fshow " = " <> gfshow False x

instance (GFShow a) => GFShow (M1 D c a) where
  gfshow isRec (M1 x) = gfshow isRec x

instance (GFShow a, Constructor c) => GFShow (M1 C c a) where
  gfshow isRec y@(M1 x)
    | conIsRecord y = fshow (conName y)
                   <> fshow " { " <> gfshow True x <> fshow " }"
    | otherwise = fshow (conName y) <> fshow " " <> gfshow False x

instance FShow a => GFShow (K1 i a) where
  gfshow isRec (K1 x) = fshow x
