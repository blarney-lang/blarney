{-# LANGUAGE BlockArguments          #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FunctionalDependencies  #-}

{-|
Module      : Blarney.Core.Lookup
Description : Provide a generic lookup operator
Copyright   : (c) Matthew Naylor, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

-}
module Blarney.Core.Lookup
  ( Lookup(..)
  ) where

-- Standard imports
import Prelude
import GHC.TypeLits
import GHC.Generics
import Control.Monad.Fix
import Control.Monad hiding (when)
import Data.List (transpose)

-- Blarney imports
import Blarney.Core.BV
import Blarney.Core.Bit
import Blarney.Core.Bits
import Blarney.Core.Prim
import Blarney.Core.Module
import Blarney.Core.Prelude
import Blarney.Core.Interface

-- |Index a collection 'c' of elements 'e' using index 'i'
class Lookup c i e | c -> e where
  (!) :: c -> i -> e

infixl 8 !

-- |Index a list using a bit vector
instance (Interface a, KnownNat n) => Lookup [a] (Bit n) a where
  (!) = lookupInterface

-- |Index a list using an 'Int'
instance Lookup [a] Int a where
  rs ! i = rs !! i

-- |Index a list using an 'Integer'
instance Lookup [a] Integer a where
  rs ! i = rs !! fromIntegral i

-- |Index a register file
instance Lookup (RegFile a d) a d where
  rf ! i = index rf i

-- |Index a bit vector using a bit vector
instance KnownNat m => Lookup (Bit n) (Bit m) (Bit 1) where
  b ! i = unsafeToBitList b ! i

-- |Index a bit vector using an Int
instance Lookup (Bit n) Int (Bit 1) where
  b ! i = unsafeToBitList b ! i

-- |Index a bit vector using an Integer
instance Lookup (Bit n) Integer (Bit 1) where
  b ! i = unsafeToBitList b ! i

-- |Index a list of interfaces using bit-vector
lookupInterface :: (KnownNat n, Interface a) => [a] -> Bit n -> a
lookupInterface ifcs i = fromIfcTerm (idx $ toIfcTerm <$> ifcs)
  where
    -- All elements in 'ifcs' have the same type, and hence all elements
    -- in the argument to 'idx' are the same constructor.
    idx [] = error "Blarney.Core.Lookup: looking up an empty list"
    idx (terms@(IfcTermBV{}:_)) =
      IfcTermBV $ muxBV (toBV i) [ x | (IfcTermBV x) <- terms ]
    idx (terms@(IfcTermAction{}:_)) = IfcTermAction do
      rets <- sequence [ whenAction (i .==. fromInteger j) act
                       | (j, IfcTermAction act) <- zip [0..] terms ]
      return (idx rets)
    idx (terms@(IfcTermProduct{}:_)) =
      IfcTermProduct (idx [x0 | IfcTermProduct x0 _ <- terms])
                     (idx [x1 | IfcTermProduct _ x1 <- terms])
    idx (terms@(IfcTermFun{}:_)) =
      IfcTermFun $ \x -> idx [f x | IfcTermFun f <- terms]

    maskBV c x = muxBV (toBV c) [constBV w 0, x]
      where w = bvPrimOutWidth x
