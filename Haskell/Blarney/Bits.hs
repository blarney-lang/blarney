-- For type-level naturals
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators,
      TypeFamilies, UndecidableInstances, FlexibleContexts #-}

module Blarney.Bits where

import Prelude
import Blarney.Bit
import GHC.TypeLits

class KnownNat (SizeOf a) => Bits a where
  type SizeOf a :: Nat
  pack :: a -> Bit (SizeOf a)
  unpack :: Bit (SizeOf a) -> a

sizeOf :: Bits a => a -> Int
sizeOf a = fromInteger (natVal (pack a))

instance KnownNat n => Bits (Bit (n :: Nat)) where
  type SizeOf (Bit n) = n
  pack = id
  unpack = id

instance (Bits a,
          Bits b,
          n ~ (SizeOf a + SizeOf b),
          KnownNat n,
          SizeOf a <= n,
          SizeOf b <= n) => Bits (a, b) where
  type SizeOf (a, b) = SizeOf a + SizeOf b
  pack (a, b) = pack a # pack b
  unpack p = (unpack (upper p), unpack (lower p))
