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
  sizeOf :: a -> Int
  pack   :: a -> Bit (SizeOf a)
  unpack :: Bit (SizeOf a) -> a

  -- Defaults
  type SizeOf a  =  GSizeOf (Rep a)
  default sizeOf :: (Generic a, GBitsClass (Rep a),
                     GSizeOf (Rep a) ~ SizeOf a)
                 => a -> Int
  sizeOf a       = gsizeOf(from a)
  default pack   :: (Generic a, GBitsClass (Rep a),
                     GSizeOf (Rep a) ~ SizeOf a)
                 => a -> Bit (SizeOf a)
  pack a         =  gpack (from a) 
  default unpack :: (Generic a, GBitsClass (Rep a),
                     GSizeOf (Rep a) ~ SizeOf a)
                 => Bit (SizeOf a) -> a
  unpack a       =  to (gunpack a)

type Bits a = (BitsClass a, KnownNat (SizeOf a))

-- Generic deriving for BitsClass
-- ==============================

class GBitsClass f where
  type GSizeOf f :: Nat
  gsizeOf :: f a -> Int
  gpack   :: f a -> Bit (GSizeOf f)
  gunpack :: Bit (GSizeOf f) -> f a

instance GBitsClass U1 where
  type GSizeOf U1 = 0
  gsizeOf U1 = 0
  gpack U1 = 0
  gunpack bs = U1

instance (GBitsClass a, GBitsClass b) => GBitsClass (a :*: b) where
  type GSizeOf (a :*: b) = GSizeOf a + GSizeOf b
  gsizeOf (a :*: b) = gsizeOf a + gsizeOf b
  gpack (a :*: b) = gpack a # gpack b
  gunpack bs = a :*: b
    where
      a  = gunpack (unsafeGetBits (wa+wb-1, wb) bs)
      b  = gunpack (unsafeGetBits (wb-1, 0) bs)
      wa = gsizeOf a
      wb = gsizeOf b

instance GBitsClass a => GBitsClass (M1 i c a) where
  type GSizeOf (M1 i c a) = GSizeOf a
  gpack (M1 x) = gpack x
  gsizeOf (M1 x) = gsizeOf x
  gunpack x = M1 (gunpack x)

instance BitsClass a => GBitsClass (K1 i a) where
  type GSizeOf (K1 i a) = SizeOf a
  gsizeOf (K1 x) = sizeOf x
  gpack (K1 x) = pack x
  gunpack x = K1 (unpack x)

-- Standard instances
-- ==================

instance KnownNat n => BitsClass (Bit n) where
  type SizeOf (Bit n) = n
  sizeOf = widthOf
  pack = id
  unpack = id

instance BitsClass ()

instance (Bits a, Bits b) => BitsClass (a, b)

instance (Bits a, Bits b, Bits c) => BitsClass (a, b, c)

instance (Bits a, Bits b, Bits c, Bits d) => BitsClass (a, b, c, d)

instance (Bits a, Bits b, Bits c, Bits d, Bits e) => BitsClass (a, b, c, d, e)

instance (Bits a, Bits b, Bits c, Bits d,
          Bits e, Bits f) => BitsClass (a, b, c, d, e, f)

instance (Bits a, Bits b, Bits c, Bits d,
          Bits e, Bits f, Bits g) => BitsClass (a, b, c, d, e, f, g)
