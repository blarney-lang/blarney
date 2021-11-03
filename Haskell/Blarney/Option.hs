{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE OverloadedRecordDot   #-}

{- |
Module      : Blarney.Option
Description : An 'Option' type, similar to 'Maybe' in functionality
Copyright   : (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : alexandre.joannou@gmail.com
Stability   : experimental

An 'Option' type, similar to 'Maybe' in functionality but represented as a pair
of a flag and a value.
-}
module Blarney.Option
  ( -- * Option type
    Option(..), some, none, isSome, isNone, fromOption
  ) where

-- Blarney imports
import Blarney

{- |
'Option' type to wrap a value. An 'Option t' is represented as a pair of a
'Bit 1' indicating whether the value held is valid, and a value of type 't'.
-}

data Option t =
  Option {
    valid :: Bit 1
  , val :: t
  } deriving (Generic, Bits, Interface, FShow)

instance Functor Option where
  fmap f (Option valid value) = Option valid (f value)

-- | Builds an 'Option' with a valid value
some :: Bits t => t -> Option t
some val = Option true val

-- | Builds an invalid 'Option'
none :: Bits t => Option t
none = Option false dontCare

-- | Tests if an 'Option' is a 'some'
isSome :: Bits t => Option t -> Bit 1
isSome opt = opt.valid

-- | Tests if an 'Option' is a 'none'
isNone :: Bits t => Option t -> Bit 1
isNone opt = inv opt.valid

-- | Gets the value from a valid 'Option', or a given default value otherwise
fromOption :: Bits t => t -> Option t -> t
fromOption dflt opt = opt.valid ? (opt.val, dflt)
