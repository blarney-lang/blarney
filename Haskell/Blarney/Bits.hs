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
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Blarney.Bits where

import Prelude
import Blarney.Bit
import GHC.TypeLits
import GHC.Generics

class BitsClass a where
  type SizeOf a :: Nat
  pack :: a -> Bit (SizeOf a)
  unpack :: Bit (SizeOf a) -> a

  -- Defaults
  type SizeOf a  =  GSizeOf (Rep a)
  default pack   :: (Generic a, GBitsClass (Rep a),
                     GSizeOf (Rep a) ~ SizeOf a)
                 => a -> Bit (SizeOf a)
  pack a         =  gpack (from a) 
  default unpack :: (Generic a, GBitsClass (Rep a),
                     GSizeOf (Rep a) ~ SizeOf a)
                 => Bit (SizeOf a) -> a
  unpack a       =  to (gunpack a)

type Bits a = (BitsClass a, KnownNat (SizeOf a))

sizeOf :: Bits a => a -> Int
sizeOf a = fromInteger (natVal (pack a))

-- Generic deriving for BitsClass
-- ==============================

class GBitsClass f where
  type GSizeOf f :: Nat
  gpack :: f a -> Bit (GSizeOf f)
  gunpack :: Bit (GSizeOf f) -> f a

instance GBitsClass U1 where
  type GSizeOf U1 = 0
  gpack U1 = 0
  gunpack bs = U1

instance (KnownNat (GSizeOf a), GBitsClass a, GBitsClass b) =>
         GBitsClass (a :*: b) where
  type GSizeOf (a :*: b) = GSizeOf a + GSizeOf b
  gpack (a :*: b) = gpack a # gpack b
  gunpack bs = gunpack a :*: gunpack b
    where (a, b) = split bs

instance GBitsClass a => GBitsClass (M1 i c a) where
  type GSizeOf (M1 i c a) = GSizeOf a
  gpack (M1 x) = gpack x
  gunpack x = M1 (gunpack x)

instance Bits a => GBitsClass (K1 i a) where
  type GSizeOf (K1 i a) = SizeOf a
  gpack (K1 x) = pack x
  gunpack x = K1 (unpack x)

-- Standard instances
-- ==================

instance BitsClass (Bit n) where
  type SizeOf (Bit n) = n
  pack = id
  unpack = id

instance BitsClass ()

instance (Bits a, Bits b) => BitsClass (a, b)

instance (Bits a, Bits b, Bits c) => BitsClass (a, b, c)
