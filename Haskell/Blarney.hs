{-|
Module      : Blarney
Description : Hardware description in Haskell
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019-2020
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

This is the top-level of the Blarney library. Note that we re-export the Haskell
'Prelude', claiming the '.' operator for reverse function application (field
selection is common in hardware description).
-}
module Blarney (
  module Blarney.Core
--, module Blarney.Netlist
, module Blarney.Backend
, module Control.Monad
, module Control.Monad.Fix
, module GHC.TypeLits
, module GHC.Generics
, module P
, (.)
) where

import Blarney.Core
--import Blarney.Netlist
import Blarney.Backend
import Control.Monad hiding (when)
import Control.Monad.Fix
import GHC.TypeLits
import GHC.Generics (Generic(..))
import Prelude as P hiding ((.), truncate)

-- | Reverse function application
infixl 9 .
(.) :: a -> (a -> b) -> b
x.f = f x
