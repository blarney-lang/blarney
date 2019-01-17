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

{-|
Module      : Blarney.Bits
Description : Convert types to bit vectors and back
Copyright   : (c) Matthew Naylor, 2019
License     : GPL-3
Maintainer  : mattfn@gmail.com

Any type in the Bits class can be represented in hardware at runtime.
For example, values of a type in the Bits class can be stored in a
register, or carried along a wire.
-}
module Blarney.Bits where

-- Typed bit vectors
import Blarney.Bit

-- Standard imports
import Prelude
import GHC.TypeLits
import GHC.Generics

class Bits a where
  type SizeOf a :: Nat
  sizeOf        :: a -> Int
  pack          :: a -> Bit (SizeOf a)
  unpack        :: Bit (SizeOf a) -> a

  -- Defaults
  type SizeOf a        =  GSizeOf (Rep a)
  default sizeOf       :: (Generic a, GBits (Rep a),
                           GSizeOf (Rep a) ~ SizeOf a)
                       => a -> Int
  sizeOf a             =  gsizeOf (from a)
  default pack         :: (Generic a, GBits (Rep a),
                           GSizeOf (Rep a) ~ SizeOf a)
                       => a -> Bit (SizeOf a)
  pack a               =  gpack (from a) 
  default unpack       :: (Generic a, GBits (Rep a),
                           GSizeOf (Rep a) ~ SizeOf a)
                       => Bit (SizeOf a) -> a
  unpack a             =  to (gunpack a)

-- Generic deriving for Bits
-- ==============================

class GBits f where
  type GSizeOf f :: Nat
  gsizeOf        :: f a -> Int
  gpack          :: f a -> Bit (GSizeOf f)
  gunpack        :: Bit (GSizeOf f) -> f a

instance GBits U1 where
  type GSizeOf U1 = 0
  gsizeOf U1 = 0
  gpack U1 = 0
  gunpack bs = U1

instance (GBits a, GBits b) => GBits (a :*: b) where
  type GSizeOf (a :*: b) = GSizeOf a + GSizeOf b
  gsizeOf (a :*: b) = gsizeOf a + gsizeOf b
  gpack (a :*: b) = gpack a # gpack b
  gunpack bs = a :*: b
    where
      a  = gunpack (unsafeBits (wa+wb-1, wb) bs)
      b  = gunpack (unsafeBits (wb-1, 0) bs)
      wa = gsizeOf a
      wb = gsizeOf b

instance GBits a => GBits (M1 i c a) where
  type GSizeOf (M1 i c a) = GSizeOf a
  gpack (M1 x) = gpack x
  gsizeOf (M1 x) = gsizeOf x
  gunpack x = M1 (gunpack x)

instance Bits a => GBits (K1 i a) where
  type GSizeOf (K1 i a) = SizeOf a
  gsizeOf (K1 x) = sizeOf x
  gpack (K1 x) = pack x
  gunpack x = K1 (unpack x)

-- Standard instances
-- ==================

instance KnownNat n => Bits (Bit n) where
  type SizeOf (Bit n) = n
  sizeOf = widthOf
  pack = id
  unpack = id

instance Bits ()

instance (Bits a, Bits b) => Bits (a, b)

instance (Bits a, Bits b, Bits c) => Bits (a, b, c)

instance (Bits a, Bits b, Bits c, Bits d) => Bits (a, b, c, d)

instance (Bits a, Bits b, Bits c, Bits d, Bits e) => Bits (a, b, c, d, e)

instance (Bits a, Bits b, Bits c, Bits d,
          Bits e, Bits f) => Bits (a, b, c, d, e, f)

instance (Bits a, Bits b, Bits c, Bits d,
          Bits e, Bits f, Bits g) => Bits (a, b, c, d, e, f, g)
