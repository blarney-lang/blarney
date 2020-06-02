{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Blarney.Core.Net
Description : Net primitive for Netlist construction
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019-2020
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

This module provides types and functions to represent circuits as 'Netlist's
that can then be rendered as Verilog or in other formats...

-}

module Blarney.Core.Net (
  Net(..)        -- 'Net' type to represent 'Netlist' nodes
, WireId         -- 'WireId' type to uniquely identify wires
, NetInput(..)   -- 'NetInput' type to represent inputs to 'Net's
, MNetlist       -- 'MNetlist' type, mutable netlist
, Netlist        -- 'Netlist' type to represent a circuit
) where

import Prelude
import Data.Maybe
import Data.Array
import Data.Array.IO

import Blarney.Core.Prim

-- General type definitions and helpers
--------------------------------------------------------------------------------

-- | 'Net' type representing a 'Netlist' node
data Net = Net { -- | The 'Net' 's 'Prim'itive
                 netPrim         :: Prim
                 -- | The 'Net' 's 'InstId' identifier
               , netInstId       :: InstId
                 -- | The 'Net' 's list of 'NetInput' inputs
               , netInputs       :: [NetInput]
                 -- | The 'Net' 's 'NameHints'
               , netNameHints    :: NameHints
               } deriving Show

-- | A 'WireId' uniquely identify a wire with a 'Net''s instance identifier
--   ('InstId') and an output name ('OutputName')
type WireId = (InstId, OutputName)

-- | A 'Net''s input ('NetInput') can be:
--   - a wire, using the 'InputWire' constructor
--   - a complex expression, using the 'InputTree' constructor
data NetInput = InputWire WireId
              | InputTree Prim [NetInput]
              deriving Show

-- | A helper type for mutable 'Netlist'
type MNetlist = IOArray InstId (Maybe Net)

-- | A 'Netlist', represented as an 'Array InstId (Maybe Net)'
type Netlist = Array InstId (Maybe Net)
