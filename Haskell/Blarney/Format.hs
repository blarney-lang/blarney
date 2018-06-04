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

data FormatItem = 
    FormatBit Int Unbit
  | FormatString String

newtype Format = Format [FormatItem]

emptyFormat :: Format
emptyFormat = Format []

appendFormat :: Format -> Format -> Format
appendFormat (Format a) (Format b) = Format (a ++ b)

str :: String -> Format
str s = Format [FormatString s]

bv :: Bit n -> Format
bv b = Format [FormatBit (unbitWidth ub) ub]
  where ub = unbit b

instance Semigroup Format where
  (<>) = appendFormat

instance Monoid Format where
  mempty = emptyFormat

-- FShow

class FShow a where
  fshow :: a -> Format

  fshowList :: [a] -> Format
  fshowList xs = str "[" <> list xs <> str "]"
    where
      list [] = emptyFormat
      list [x] = fshow x
      list (x:xs) = fshow x <> str "," <> list xs

  default fshow :: (Generic a, GFShow (Rep a)) => a -> Format
  fshow x = gfshow False (from x)

instance FShow Char where
  fshow c = str [c]
  fshowList cs = str cs

instance FShow (Bit n) where
  fshow = bv

instance FShow Format where
  fshow f = f

instance FShow a => FShow [a] where
  fshow = fshowList

instance (FShow a, FShow b) => FShow (a, b) where
  fshow (a, b) = str "(" <> fshow a <> str "," <> fshow b <> str ")"

-- Generic deriving for FShow

class GFShow f where
  -- First argument: are we showing a record constructor?
  gfshow :: Bool -> f a -> Format

instance GFShow U1 where
  gfshow rec U1 = emptyFormat

instance (GFShow a, GFShow b) => GFShow (a :*: b) where
  gfshow rec (a :*: b) = gfshow rec a <> str sep <> gfshow rec b
    where sep = case rec of { False -> " "; True -> ", " }

instance (GFShow a, Selector c) => GFShow (M1 S c a) where
  gfshow rec y@(M1 x)
    | null (selName y) = str "(" <> gfshow False x <> str ")"
    | otherwise = str (selName y) <> str " = " <> gfshow False x

instance (GFShow a) => GFShow (M1 D c a) where
  gfshow rec (M1 x) = gfshow rec x

instance (GFShow a, Constructor c) => GFShow (M1 C c a) where
  gfshow rec y@(M1 x)
    | conIsRecord y = str (conName y)
                   <> str " { " <> gfshow True x <> str " }"
    | otherwise = str (conName y) <> str " " <> gfshow False x

instance FShow a => GFShow (K1 i a) where
  gfshow rec (K1 x) = fshow x
