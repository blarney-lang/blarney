{-|
Module      : Blarney.Core.Prim
Description : circuit primitives
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

This module provides a set of circuit primitives.
-}

module Blarney.Core.Prim (
  -- * 'Prim' primitive type and helpers
  Prim(..)       -- Primitive components
, canInline      -- Tell if a 'Prim' can be inlined
, canInlineInput -- Tell if a 'Prim' inputs can be inlined
, primStr        -- helper to get useful name strings out of a 'Prim'
  -- * Other primitive types
, InstId         -- Every component instance has a unique id
, OutputNumber   -- Each output from a component is numbered
, Width          -- Bit vector width
, InputWidth     -- Width of an input to a component
, OutputWidth    -- Width of an output from a component
, BitIndex       -- For indexing a bit vector
, RegFileId      -- Identifier for register file primitiveA
, DisplayArg(..) -- Arguments to display primitive
, Param(..)      -- Compile-time parameters
, NameHint(..)   -- A 'NameHint' type to represent name hints
, NameHints      -- A 'NameHints' type to gather name hints
) where

import Prelude
import Data.Set

-- | Every instance of a component in the circuit has a unique id
type InstId = Int

-- | A 'NameHint' type describing name hints
data NameHint = NmPrefix Int String
              | NmRoot Int String
              | NmSuffix Int String
              deriving (Eq, Ord, Show)
-- | A 'NameHints' type to gather name hints
type NameHints = Set NameHint

-- |Each output from a primitive component is numbered
type OutputNumber = Int

-- | Bit vector width
type Width = Int

-- | For indexing a bit vector
type BitIndex = Int

-- | Identifier for register file primitive
type RegFileId = Int

-- | Width of an input to a primitive
type InputWidth = Int

-- | Width of an output from a primitive
type OutputWidth = Int

-- | Custom components may have compile-time parameters.
--   A parameter has a name and a value, both represented as strings
data Param = String :-> String deriving Show

-- | For the Display primitive:
--   display a string literal or a bit-vector value of a given width
data DisplayArg =
    DisplayArgString String
  | DisplayArgBit InputWidth
instance Show DisplayArg where
  show (DisplayArgString s) = show s
  show (DisplayArgBit w) = show w

-- | Primitive components
data Prim =
    -- | Constant value (0 inputs, 1 output)
    Const OutputWidth Integer

    -- | Don't care value (0 input, 1 output)
  | DontCare OutputWidth

    -- | Adder (2 inputs, 1 output)
  | Add OutputWidth
    -- | Subtractor (2 inputs, 1 output)
  | Sub OutputWidth
    -- | Multiplier (2 inputs, 1 output)
  | Mul OutputWidth
    -- | Quotient (2 inputs, 1 output)
  | Div OutputWidth
    -- | Remainder (2 inputs, 1 output)
  | Mod OutputWidth

    -- | Bitwise not (1 input, 1 output)
  | Not OutputWidth
    -- | Bitwise and (2 inputs, 1 output)
  | And OutputWidth
    -- | Bitwise or (2 inputs, 1 output)
  | Or OutputWidth
    -- | Bitwise xor (2 inputs, 1 output)
  | Xor OutputWidth

    -- | Left shift (2 inputs, 1 output)
  | ShiftLeft OutputWidth
    -- | Logical right shift (2 inputs, 1 output)
  | ShiftRight OutputWidth
    -- | Arithmetic right shift (2 inputs, 1 output)
  | ArithShiftRight OutputWidth

    -- | Equality comparator (2 inputs, 1 bit output)
  | Equal InputWidth
    -- | Disequality comparator (2 inputs, 1 bit output)
  | NotEqual InputWidth
    -- | Less-than comparator (2 inputs, 1 bit output)
  | LessThan InputWidth
    -- | Less-than-or-equal comparator (2 inputs, 1 bit output)
  | LessThanEq InputWidth

    -- | Replicate a bit (1 input, 1 output)
  | ReplicateBit OutputWidth
    -- | Zero-extend a bit vector (1 input, 1 output)
  | ZeroExtend InputWidth OutputWidth
    -- | Sign-extend a bit vector (1 input, 1 output)
  | SignExtend InputWidth OutputWidth

    -- | Bit selection (compile-time range, 1 input, 1 output)
  | SelectBits InputWidth BitIndex BitIndex
    -- | Bit vector concatenation (2 inputs, 1 output)
  | Concat InputWidth InputWidth

    -- | Multiplexer (3 inputs, 1 output)
  | Mux OutputWidth
    -- | Population count (1 input, 1 output)
  | CountOnes OutputWidth
    -- | Identity function (1 input, 1 output)
  | Identity OutputWidth

    -- | Register with initial value (1 input, 1 output)
  | Register Integer InputWidth
    -- | Register with initial value and enable wire (2 inputs, 1 output)
  | RegisterEn Integer InputWidth

    -- | Single-port block RAM
  | BRAM { ramInitFile  :: Maybe String
         , ramAddrWidth :: Width
         , ramDataWidth :: Width }
    -- | True dual-port block RAM
  | TrueDualBRAM { ramInitFile  :: Maybe String
                 , ramAddrWidth :: Width
                 , ramDataWidth :: Width }

    -- | Custom component
  | Custom { customName      :: String          -- component name
           , customInputs    :: [String]        -- input names
           , customOutputs   :: [(String, Int)] -- output names/widths
           , customParams    :: [Param]         -- parameters
           , customIsClocked :: Bool }          -- pass clock and reset ?

    -- | External input (0 inputs, 1 output)
  | Input OutputWidth String
    -- | External output (only used in RTL context, not expression context)
  | Output InputWidth String

    -- | Simulation-time I/O
    --   (only used in RTL context, not expression context)
  | Display [DisplayArg]
    -- | Finish simulation (input: 1-bit enable wire)
  | Finish
    -- | Test presence of plusargs (output: 1-bit boolean)
  | TestPlusArgs String

    -- | Register file declaration
    --   (only used in RTL context, not expression context)
  | RegFileMake String InputWidth InputWidth RegFileId
    -- | Register file lookup (input: index, output: data)
  | RegFileRead OutputWidth RegFileId
    -- | Register file update (inputs: write-enable, address, data)
  | RegFileWrite InputWidth InputWidth RegFileId
  deriving Show

-- | Helper to tell whether a 'Prim' can be inlined during Netlist optimisation
canInline :: Prim -> Bool
canInline p = inlinable (primInfo p)

-- | Helper to tell whether a 'Prim' inputs can be inlined during Netlist
--   optimisation
canInlineInput :: Prim -> Bool
canInlineInput p = inlinableInputs (primInfo p)

-- | Helper to render a primitive name. Used to generate useful names
primStr :: Prim -> String
primStr Const{} = "Const"
primStr DontCare{} = "DontCare"
primStr Add{} = "Add"
primStr Sub{} = "Sub"
primStr Mul{} = "Mul"
primStr Div{} = "Div"
primStr Mod{} = "Mod"
primStr Not{} = "Not"
primStr And{} = "And"
primStr Or{} = "Or"
primStr Xor{} = "Xor"
primStr ShiftLeft{} = "ShiftLeft"
primStr ShiftRight{} = "ShiftRight"
primStr ArithShiftRight{} = "ArithShiftRight"
primStr Equal{} = "Equal"
primStr NotEqual{} = "NotEqual"
primStr LessThan{} = "LessThan"
primStr LessThanEq{} = "LessThanEq"
primStr ReplicateBit{} = "ReplicateBit"
primStr ZeroExtend{} = "ZeroExtend"
primStr SignExtend{} = "SignExtend"
primStr SelectBits{} = "SelectBits"
primStr Concat{} = "Concat"
primStr Mux{} = "Mux"
primStr CountOnes{} = "CountOnes"
primStr Identity{} = "Identity"
primStr Register{} = "Register"
primStr RegisterEn{} = "RegisterEn"
primStr BRAM{} = "BRAM"
primStr TrueDualBRAM{} = "TrueDualBRAM"
primStr Custom{ customName = str } = str
primStr (Input _ str) = str
primStr (Output _ str) = str
primStr Display{} = "Display"
primStr Finish = "Finish"
primStr (TestPlusArgs str) = "PlusArgs_" ++ str
primStr RegFileMake{} = "RegFileMake"
primStr RegFileRead{} = "RegFileRead"
primStr RegFileWrite{} = "RegFileWrite"

-- | A 'Name' type that handles name hints
data Name = Name { nameHints :: Set String } deriving Show
instance Semigroup Name where
  x <> y = Name { nameHints = nameHints x `union` nameHints y }
instance Monoid Name where
  mempty = Name { nameHints = empty }

-- Internal functions
--------------------------------------------------------------------------------

-- | Type to hold metadata about a 'Prim'
data PrimInfo = PrimInfo { -- | The 'inlinable' field specifies whether a 'Prim'
                           --   can be inlined during the netlist optimisation
                           --   passes. When generating verilog, it is useful to
                           --   avoid inlining of certain primitive such as
                           --   'Input' or 'Display' or stateful ones like
                           --   'Register'...
                           inlinable :: Bool
                           -- | The 'inlinableInputs' field specifies whether a
                           --   'Prim' ' inputs can be inlined during the
                           --   netlist optimisation passes. When generating
                           --   verilog, it is useful to avoid inlining inputs
                           --   to 'Prim's involving bit-slicing (specifically,
                           --   'SelectBits' and 'SignExtend') due to lack of
                           --   underlying support.
                         , inlinableInputs :: Bool
                           -- | The 'strRep' field specifies a 'String'
                           --   representation of a 'Prim'.
                         , strRep :: String
                         }
-- | Helper getting general metadata about a 'Prim'
primInfo :: Prim -> PrimInfo
primInfo (Const _ x) = PrimInfo { inlinable = True
                                , inlinableInputs = True
                                , strRep = "Const" ++ show x }
primInfo (DontCare _) = PrimInfo { inlinable = True
                                 , inlinableInputs = True
                                 , strRep = "DontCare" }
primInfo (Add _) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Add" }
primInfo (Sub _) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Sub" }
primInfo (Mul _) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Mul" }
primInfo (Div _) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Div" }
primInfo (Mod _) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Mod" }
primInfo (Not _) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Not" }
primInfo (And _) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "And" }
primInfo (Or _) = PrimInfo { inlinable = True
                           , inlinableInputs = True
                           , strRep = "Or" }
primInfo (Xor _) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Xor" }
primInfo (ShiftLeft _) = PrimInfo { inlinable = True
                                  , inlinableInputs = True
                                  , strRep = "ShiftLeft" }
primInfo (ShiftRight _) = PrimInfo { inlinable = True
                                   , inlinableInputs = True
                                   , strRep = "ShiftRight" }
primInfo (ArithShiftRight _) = PrimInfo { inlinable = True
                                        , inlinableInputs = True
                                        , strRep = "ArithShiftRight" }
primInfo (Equal _) = PrimInfo { inlinable = True
                              , inlinableInputs = True
                              , strRep = "Equal" }
primInfo (NotEqual _) = PrimInfo { inlinable = True
                                 , inlinableInputs = True
                                 , strRep = "NotEqual" }
primInfo (LessThan _) = PrimInfo { inlinable = True
                                 , inlinableInputs = True
                                 , strRep = "LessThan" }
primInfo (LessThanEq _) = PrimInfo { inlinable = True
                                   , inlinableInputs = True
                                   , strRep = "LessThanEq" }
primInfo (ReplicateBit _) = PrimInfo { inlinable = True
                                     , inlinableInputs = True
                                     , strRep = "ReplicateBit" }
primInfo (ZeroExtend _ _) = PrimInfo { inlinable = True
                                     , inlinableInputs = True
                                     , strRep = "ZeroExtend" }
primInfo (SignExtend _ _) = PrimInfo { inlinable = True
                                     , inlinableInputs = False
                                     , strRep = "SignExtend" }
primInfo (SelectBits _ _ _) = PrimInfo { inlinable = True
                                       , inlinableInputs = False
                                       , strRep = "SelectBits" }
primInfo (Concat _ _) = PrimInfo { inlinable = True
                                 , inlinableInputs = True
                                 , strRep = "Concat" }
primInfo (Mux _) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Mux" }
primInfo (CountOnes _) = PrimInfo { inlinable = True
                                  , inlinableInputs = True
                                  , strRep = "CountOnes" }
primInfo (Identity _) = PrimInfo { inlinable = True
                                 , inlinableInputs = True
                                 , strRep = "Identity" }
primInfo (Register _ _) = PrimInfo { inlinable = False
                                   , inlinableInputs = True
                                   , strRep = "Register" }
primInfo (RegisterEn _ _) = PrimInfo { inlinable = False
                                     , inlinableInputs = True
                                     , strRep = "RegisterEn" }
primInfo BRAM{} = PrimInfo { inlinable = False
                           , inlinableInputs = True
                           , strRep = "BRAM" }
primInfo TrueDualBRAM{} = PrimInfo { inlinable = False
                                   , inlinableInputs = True
                                   , strRep = "TrueDualBRAM" }
primInfo Custom{ customName = nm } = PrimInfo { inlinable = False
                                              , inlinableInputs = True
                                              , strRep = nm }
primInfo (Input _ nm) = PrimInfo { inlinable = False
                                 , inlinableInputs = True
                                 , strRep = nm }
primInfo (Output _ nm) = PrimInfo { inlinable = False
                                  , inlinableInputs = True
                                  , strRep = nm }
primInfo (Display _) = PrimInfo { inlinable = False
                                , inlinableInputs = True
                                , strRep = "Display" }
primInfo Finish = PrimInfo { inlinable = False
                           , inlinableInputs = True
                           , strRep = "Finish" }
primInfo (TestPlusArgs arg) = PrimInfo { inlinable = False
                                       , inlinableInputs = True
                                       , strRep = "PlusArgs_" ++ arg }
primInfo RegFileMake{} = PrimInfo { inlinable = False
                                  , inlinableInputs = True
                                  , strRep = "RegFileMake" }
primInfo RegFileRead{} = PrimInfo { inlinable = False
                                  , inlinableInputs = True
                                  , strRep = "RegFileRead" }
primInfo RegFileWrite{} = PrimInfo { inlinable = False
                                   , inlinableInputs = True
                                   , strRep = "RegFileWrite" }
