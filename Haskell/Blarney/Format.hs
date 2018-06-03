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

data FormatItem = 
    FormatBit Int Unbit
  | FormatString String

newtype Format = Format [FormatItem]

emptyFormat :: Format
emptyFormat = Format []

(<.>) :: Format -> Format -> Format
Format a <.> Format b = Format (a ++ b)

class FormatType a where
  formatType :: Format -> a

instance FormatType Format where
  formatType f = f

instance FormatType a => FormatType (String -> a) where
  formatType f s = formatType (f <.> Format [FormatString s])

instance FormatType a => FormatType (Bit n -> a) where
  formatType f b = formatType (f <.> Format [FormatBit (unbitWidth ub) ub])
    where ub = unbit b

instance FormatType a => FormatType (Format -> a) where
  formatType f f' = formatType (f <.> f')

format :: FormatType a => a
format = formatType emptyFormat

-- FShow

class FShow a where
  fshow :: a -> Format

  fshowList :: [a] -> Format
  fshowList xs = format "[" <.> list xs <.> format "]"
    where
      list [] = emptyFormat
      list [x] = fshow x
      list (x:xs) = fshow x <.> format "," <.> list xs

  default fshow :: (Generic a, GFShow (Rep a)) => a -> Format
  fshow x = gfshow False (from x)

instance FShow Char where
  fshow c = format [c]
  fshowList cs = format cs

instance FShow (Bit n) where
  fshow b = format b

instance FShow Format where
  fshow f = f

instance FShow a => FShow [a] where
  fshow = fshowList

instance (FShow a, FShow b) => FShow (a, b) where
  fshow (a, b) = format "(" (fshow a) "," (fshow b) ")"

-- Generic deriving for FShow

class GFShow f where
  -- First argument: are we showing a record constructor?
  gfshow :: Bool -> f a -> Format

instance GFShow U1 where
  gfshow rec U1 = emptyFormat

instance (GFShow a, GFShow b) => GFShow (a :*: b) where
  gfshow rec (a :*: b) = gfshow rec a <.> format sep <.> gfshow rec b
    where sep = case rec of { False -> " "; True -> ", " }

instance (GFShow a, Selector c) => GFShow (M1 S c a) where
  gfshow rec y@(M1 x)
    | null (selName y) = format "(" <.> gfshow False x <.> format ")"
    | otherwise = format (selName y) <.> format " = " <.> gfshow False x

instance (GFShow a) => GFShow (M1 D c a) where
  gfshow rec (M1 x) = gfshow rec x

instance (GFShow a, Constructor c) => GFShow (M1 C c a) where
  gfshow rec y@(M1 x)
    | conIsRecord y = format (conName y)
                  <.> format " { " <.> gfshow True x <.> format " }"
    | otherwise = format (conName y) <.> format " " <.> gfshow False x

instance FShow a => GFShow (K1 i a) where
  gfshow rec (K1 x) = fshow x
