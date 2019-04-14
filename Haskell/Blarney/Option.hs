{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Blarney.Option
  ( -- * Option type
    Option(..), some, none, isSome, isNone, fromOption
  ) where

-- Blarney imports
import Blarney

-- |Option type
newtype Option t = Option (Bit 1, t) deriving (Generic, Bits, FShow)
instance  Valid (Option t) where
  valid (Option (x, _)) = x
instance Val (Option t) t where
  val (Option (_, y))= y

-- |Helper to build an Option with a valid value
some :: Bits t => t -> Option t
some val = Option (true, val)

-- |Helper to build an invalid Option
none :: Bits t => Option t
none = Option (false, dontCare)

-- |Test valididty of Option
isSome :: Bits t => Option t -> Bit 1
isSome opt = opt.valid

-- |Test valididty of Option
isNone :: Bits t => Option t -> Bit 1
isNone opt = opt.valid.inv

-- |Get value from a valid Option, default value otherwise
fromOption :: Bits t => t -> Option t -> t
fromOption dflt opt = opt.valid ? (opt.val, dflt)
