{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : Blarney.TypeFamilies
Description : Common type families
Copyright   : (c) Matthew Naylor, 2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
This module defines commonly used type families, not available in base.
-}
module Blarney.TypeFamilies where

import Blarney

-- GHC imports
import GHC.Types
import Data.Type.Bool
import Data.Type.Equality

-- | Type function for computing the min of two type-level nats
type family Min (x :: Nat) (y::Nat) :: Nat where
  Min x y = If (CmpNat x y == LT) x y

-- | Type function for computing the max of two type-level nats
type family Max (a :: Nat) (b :: Nat) :: Nat where
  Max a b = If (CmpNat a b == GT) a b

-- | Type function to compute the length of a list
type family Length (ts :: [Type]) :: Nat where
  Length '[] = 0
  Length (u ': us) = 1 + Length us

-- | Min number of bits needed to hold given nat
type family Log2Ceil (n :: Nat) :: Nat where
  Log2Ceil n = If (n <=? 1) n (1 + Log2 (n-1))
