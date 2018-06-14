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

module Blarney.Format where

import Prelude
import Blarney.Unbit
import Blarney.Bit
import GHC.Generics
import Data.Monoid

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

data FormatItem = 
    FormatBit Int Unbit
  | FormatString String

newtype Format = Format [FormatItem]


instance Semigroup Format where
  Format a <> Format b = Format (a ++ b)

instance Monoid Format where
  mempty = Format []

instance FShow Char where
  fshow c = Format [FormatString [c]]
  fshowList cs = Format [FormatString cs]

instance FShow (Bit n) where
  fshow b = Format [FormatBit (unbitWidth ub) ub]
    where ub = unbit b

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
