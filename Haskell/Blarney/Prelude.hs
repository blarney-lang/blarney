{- |
Module      : Blarney.Prelude
Description : Hardware description in Haskell
Copyright   : (c) Matthew Naylor, 2024
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

Re-export (most of) the Haskell Prelude and other important standard
Haskell modules.
-}

module Blarney.Prelude (
  module Control.Monad
, module Control.Monad.Fix
, HasField(..)
, Generic(..)
, module GHC.TypeLits
, module P
) where

import Control.Monad hiding (when)
import Control.Monad.Fix
import GHC.Records (HasField(..))
import GHC.Generics (Generic(..))
import GHC.TypeLits
import Prelude as P hiding (truncate)
