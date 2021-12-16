{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : Blarney.TypeNatHelpers
Description : Helpers for type-level nats
Copyright   : (c) Matthew Naylor, 2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

-}
module Blarney.TypeNatHelpers where

import Blarney

-- GHC imports
import GHC.Types
import Data.Type.Bool
import Data.Type.Equality

-- | Type function for computing the max of two type-level nats
type family Max (a :: Nat) (b :: Nat) :: Nat where
  Max a b = If (CmpNat a b == GT) a b

-- | Type function for computing the min of two type-level nats
type family Min (a :: Nat) (b :: Nat) :: Nat where
  Min a b = If (CmpNat a b == LT) a b
