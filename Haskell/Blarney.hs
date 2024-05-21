{- |
Module      : Blarney
Description : Hardware description in Haskell
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019-2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

This is the top-level of the Blarney library. It essentially exports a set of
other Blarney modules. Note that it also re-exports the Haskell 'Prelude'.
-}

module Blarney (
-- * Exported Blarney modules
  module Blarney.Core
, module Blarney.Netlist
, module Blarney.Backend
-- * Re-exported Haskell modules
, module Blarney.Prelude
) where

import Blarney.Core
import Blarney.Netlist
import Blarney.Backend
import Blarney.Prelude
