{-|
Module      : Blarney.Netlist.Utils
Description : Blarney netlist common utils
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental
-}

module Blarney.Netlist.Utils (
-- Prim
  Prim(..)
, primIsRoot
, primDontKill
, primInputs
, primOutputs
, primStr
, canInline
, canInlineInput
, Param(..)
, DisplayArg(..)
, RegFileInfo(..)
, NameHint(..)
, NameHints
, OutputName
, InstId
-- Net
, NetInput(..)
, Net(..)
, MNetlist
, Netlist
) where

import Blarney.Core.Prim ( Prim(..)
                         , primIsRoot
                         , primDontKill
                         , primInputs
                         , primOutputs
                         , primStr
                         , canInline
                         , canInlineInput
                         , Param(..)
                         , DisplayArg(..)
                         , RegFileInfo(..)
                         , NameHint(..)
                         , NameHints
                         , OutputName
                         , InstId )
import Blarney.Core.Net ( NetInput (..)
                        , Net(..)
                        , MNetlist
                        , Netlist )
