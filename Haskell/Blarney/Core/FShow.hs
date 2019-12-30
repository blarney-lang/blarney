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

-- Standard imports
import Prelude
import Data.Monoid
import GHC.Generics

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


-- | Format for displaying values in simulation
newtype Format = Format [FormatItem]

-- | Allow displaying of bit vectors and strings
data FormatItem =
    FormatBit Int BV
  | FormatString String

-- | Format concatention
instance Semigroup Format where
  Format a <> Format b = Format (a ++ b)

-- | Empty format
instance Monoid Format where
  mempty = Format []

instance FShow Char where
  fshow c = Format [FormatString [c]]
  fshowList cs = Format [FormatString cs]

instance FShow (Bit n) where
  fshow b = Format [FormatBit (bvWidth ub) ub]
    where ub = toBV b

instance FShow Int where
  fshow i = Format [FormatString (show i)]

instance FShow Integer where
  fshow i = Format [FormatString (show i)]

instance FShow Format where
  fshow f = f

instance FShow a => FShow [a] where
  fshow = fshowList

instance (FShow a, FShow b) => FShow (a, b) where
  fshow (a, b) = fshow "(" <> fshow a <> fshow "," <> fshow b <> fshow ")"

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
