{-|
Module      : Blarney
Description : Hardware description in Haskell
Copyright   : (c) Matthew Naylor, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

This is the top-level of library.  Note that we re-export the Prelude,
claiming the '.' operator for reverse function application (field
selection is common in hardware description).
-}
module Blarney
  ( module Blarney.Bit
  , module Blarney.Bits
  , module Blarney.Prelude
  , module Blarney.Module
  , module Blarney.FShow
  , module Blarney.Recipe
  , module Blarney.Verilog
  , module Blarney.Interface
  , module Blarney.IfThenElse
  , module Control.Monad
  , module Control.Monad.Fix
  , module GHC.TypeLits
  , module GHC.Generics
  , module P
  , (.)
  ) where

import Blarney.Bit
import Blarney.Bits
import Blarney.Prelude
import Blarney.Module
import Blarney.FShow
import Blarney.Recipe
import Blarney.Verilog
import Blarney.Interface
import Blarney.IfThenElse
import Control.Monad hiding (when)
import Control.Monad.Fix
import GHC.TypeLits
import GHC.Generics (Generic(..))
import Prelude as P hiding ((.), truncate)

-- |Reverse function application
infixl 9 .
(.) :: a -> (a -> b) -> b
x.f = f x
