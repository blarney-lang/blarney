{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Blarney.Format where

import Blarney.Unbit
import Blarney.Bit

data FormatItem = 
    FormatBit Unbit
  | FormatString String

newtype Format = Format [FormatItem]

emptyFormat :: Format
emptyFormat = Format []

(<>) :: Format -> Format -> Format
Format a <> Format b = Format (a ++ b)

class FormatType a where
  formatType :: Format -> a

instance FormatType Format where
  formatType f = f

instance FormatType a => FormatType (String -> a) where
  formatType f s = formatType (f <> Format [FormatString s])

instance FormatType a => FormatType (Bit n -> a) where
  formatType f b = formatType (f <> Format [FormatBit (unbit b)])

instance FormatType a => FormatType (Format -> a) where
  formatType f f' = formatType (f <> f')

format :: FormatType a => a
format = formatType emptyFormat

class FShow a where
  fshow :: a -> Format

instance FShow String where
  fshow s = format s

instance FShow (Bit n) where
  fshow b = format b

instance FShow Format where
  fshow f = f

instance (FShow a, FShow b) => FShow (a, b) where
  fshow (a, b) = format "(" (fshow a) "," (fshow b) ")"
