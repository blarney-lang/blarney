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
, primStr        -- Returns a 'String' representation fo the given 'Prim'
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
, Name(..)       -- A 'Name' type that handles name hints
) where

import Prelude
import Data.Set

-- | Every instance of a component in the circuit has a unique id
type InstId = Int

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
canInline p = inlinable (primMetaData p)

-- | Helper to tell whether a 'Prim' inputs can be inlined during Netlist
--   optimisation
canInlineInput :: Prim -> Bool
canInlineInput p = inlinableInputs (primMetaData p)

-- | Helper to return a 'String' representation of the given 'Prim'
primStr :: Prim -> String
primStr p = strRep (primMetaData p)

-- | A 'Name' type that handles name hints
data Name = Name { nameHints :: Set String } deriving Show
instance Semigroup Name where
  x <> y = Name { nameHints = nameHints x `union` nameHints y }
instance Monoid Name where
  mempty = Name { nameHints = empty }

-- Internal functions
--------------------------------------------------------------------------------

-- | Type to hold metadata about a 'Prim'
data PrimMetaData = PrimMetaData { inlinable :: Bool
                                 , inlinableInputs :: Bool
                                 , strRep :: String
                                 }
-- | Helper getting general metadata about a 'Prim'
primMetaData :: Prim -> PrimMetaData
primMetaData (Const _ x) = PrimMetaData { inlinable = True
                                        , inlinableInputs = True
                                        , strRep = "Const" ++ show x }
primMetaData (DontCare _) = PrimMetaData { inlinable = True
                                         , inlinableInputs = True
                                         , strRep = "DontCare" }
primMetaData (Add _) = PrimMetaData { inlinable = True
                                    , inlinableInputs = True
                                    , strRep = "Add" }
primMetaData (Sub _) = PrimMetaData { inlinable = True
                                    , inlinableInputs = True
                                    , strRep = "Sub" }
primMetaData (Mul _) = PrimMetaData { inlinable = True
                                    , inlinableInputs = True
                                    , strRep = "Mul" }
primMetaData (Div _) = PrimMetaData { inlinable = True
                                    , inlinableInputs = True
                                    , strRep = "Div" }
primMetaData (Mod _) = PrimMetaData { inlinable = True
                                    , inlinableInputs = True
                                    , strRep = "Mod" }
primMetaData (Not _) = PrimMetaData { inlinable = True
                                    , inlinableInputs = True
                                    , strRep = "Not" }
primMetaData (And _) = PrimMetaData { inlinable = True
                                    , inlinableInputs = True
                                    , strRep = "And" }
primMetaData (Or _) = PrimMetaData { inlinable = True
                                   , inlinableInputs = True
                                   , strRep = "Or" }
primMetaData (Xor _) = PrimMetaData { inlinable = True
                                    , inlinableInputs = True
                                    , strRep = "Xor" }
primMetaData (ShiftLeft _) = PrimMetaData { inlinable = True
                                          , inlinableInputs = True
                                          , strRep = "ShiftLeft" }
primMetaData (ShiftRight _) = PrimMetaData { inlinable = True
                                           , inlinableInputs = True
                                           , strRep = "ShiftRight" }
primMetaData (ArithShiftRight _) = PrimMetaData { inlinable = True
                                                , inlinableInputs = True
                                                , strRep = "ArithShiftRight" }
primMetaData (Equal _) = PrimMetaData { inlinable = True
                                      , inlinableInputs = True
                                      , strRep = "Equal" }
primMetaData (NotEqual _) = PrimMetaData { inlinable = True
                                         , inlinableInputs = True
                                         , strRep = "NotEqual" }
primMetaData (LessThan _) = PrimMetaData { inlinable = True
                                         , inlinableInputs = True
                                         , strRep = "LessThan" }
primMetaData (LessThanEq _) = PrimMetaData { inlinable = True
                                           , inlinableInputs = True
                                           , strRep = "LessThanEq" }
primMetaData (ReplicateBit _) = PrimMetaData { inlinable = True
                                             , inlinableInputs = True
                                             , strRep = "ReplicateBit" }
primMetaData (ZeroExtend _ _) = PrimMetaData { inlinable = True
                                             , inlinableInputs = True
                                             , strRep = "ZeroExtend" }
primMetaData (SignExtend _ _) = PrimMetaData { inlinable = True
                                             , inlinableInputs = False
                                             , strRep = "SignExtend" }
primMetaData (SelectBits _ _ _) = PrimMetaData { inlinable = True
                                             , inlinableInputs = False
                                             , strRep = "SelectBits" }
primMetaData (Concat _ _) = PrimMetaData { inlinable = True
                                         , inlinableInputs = True
                                         , strRep = "Concat" }
primMetaData (Mux _) = PrimMetaData { inlinable = True
                                    , inlinableInputs = True
                                    , strRep = "Mux" }
primMetaData (CountOnes _) = PrimMetaData { inlinable = True
                                          , inlinableInputs = True
                                          , strRep = "CountOnes" }
primMetaData (Identity _) = PrimMetaData { inlinable = True
                                         , inlinableInputs = True
                                         , strRep = "Identity" }
primMetaData (Register _ _) = PrimMetaData { inlinable = False
                                           , inlinableInputs = True
                                           , strRep = "Register" }
primMetaData (RegisterEn _ _) = PrimMetaData { inlinable = False
                                             , inlinableInputs = True
                                             , strRep = "RegisterEn" }
primMetaData BRAM{} = PrimMetaData { inlinable = False
                                   , inlinableInputs = True
                                   , strRep = "BRAM" }
primMetaData TrueDualBRAM{} = PrimMetaData { inlinable = False
                                           , inlinableInputs = True
                                           , strRep = "TrueDualBRAM" }
primMetaData Custom{ customName = nm } = PrimMetaData { inlinable = False
                                                      , inlinableInputs = True
                                                      , strRep = nm }
primMetaData (Input _ nm) = PrimMetaData { inlinable = False
                                         , inlinableInputs = True
                                         , strRep = nm }
primMetaData (Output _ nm) = PrimMetaData { inlinable = False
                                          , inlinableInputs = True
                                          , strRep = nm }
primMetaData (Display _) = PrimMetaData { inlinable = False
                                        , inlinableInputs = True
                                        , strRep = "Display" }
primMetaData Finish = PrimMetaData { inlinable = False
                                   , inlinableInputs = True
                                   , strRep = "Finish" }
primMetaData (TestPlusArgs arg) = PrimMetaData { inlinable = False
                                               , inlinableInputs = True
                                               , strRep = "PlusArgs_" ++ arg }
primMetaData RegFileMake{} = PrimMetaData { inlinable = False
                                          , inlinableInputs = True
                                          , strRep = "RegFileMake" }
primMetaData RegFileRead{} = PrimMetaData { inlinable = False
                                          , inlinableInputs = True
                                          , strRep = "RegFileRead" }
primMetaData RegFileWrite{} = PrimMetaData { inlinable = False
                                           , inlinableInputs = True
                                           , strRep = "RegFileWrite" }
