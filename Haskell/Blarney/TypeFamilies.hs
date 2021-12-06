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

-- | Type function for computing the max of two type-level nats
type family Max (a :: Nat) (b :: Nat) :: Nat where
  Max a b = If (CmpNat a b == GT) a b

-- | Type function for computing the max size of a list of types
type family MaxSizeOf (ts :: [Type]) :: Nat where
  MaxSizeOf '[] = 0
  MaxSizeOf (u ': us) = Max (SizeOf u) (MaxSizeOf us)

-- | Type function to compute the length of a list
type family Length (ts :: [Type]) :: Nat where
  Length '[] = 0
  Length (u ': us) = 1 + Length us
