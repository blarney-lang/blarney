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
  Prim(..)        -- Primitive components
, canInline       -- Tell if a 'Prim' can be inlined
, canInlineInput  -- Tell if a 'Prim' inputs can be inlined
, primStr         -- get useful name strings out of a 'Prim'
, primDontKill    -- tell if a 'Prim' can be optimised away
, primIsRoot      -- tell if a 'Prim' is a netlist root
, primInputs      -- get the inputs of a 'Prim'
, primOutputs     -- get the outputs of a 'Prim'
, primOutWidth    -- get 'OutputWidth' for a given named output of a 'Prim'
  -- * Other primitive types
, InstId          -- Every component instance has a unique id
, Width           -- Bit vector width
, InputWidth      -- Width of an input to a component
, OutputWidth     -- Width of an output from a component
, OutputName      -- Reference to a named output of a 'Prim'
, BitIndex        -- For indexing a bit vector
, RegFileInfo(..) -- Register file primitive parameters
, DisplayArg(..)  -- Arguments to display primitive
, Param(..)       -- Compile-time parameters
, NameHint(..)    -- A 'NameHint' type to represent name hints
, NameHints       -- A 'NameHints' type to gather name hints
) where

import Prelude
import Data.Set
import Data.Maybe

import Blarney.Core.Utils

-- | Every instance of a component in the circuit has a unique id
type InstId = Int

-- | A 'NameHint' type describing name hints
data NameHint = NmPrefix Int String
              | NmRoot Int String
              | NmSuffix Int String
              deriving (Eq, Ord, Show)
-- | A 'NameHints' type to gather name hints
type NameHints = Set NameHint

-- | Bit vector width
type Width = Int

-- | Width of an input to a primitive
type InputWidth = Width

-- | Width of an output from a primitive
type OutputWidth = Width

-- | Reference to a named output of a 'Prim'
type OutputName = Maybe String

-- | For indexing a bit vector
type BitIndex = Int

-- | Register file primitive parameters
data RegFileInfo = RegFileInfo { regFileId        :: Int
                               , regFileInitFile  :: String
                               , regFileAddrWidth :: InputWidth
                               , regFileDataWidth :: Width } deriving Show

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
  | ShiftLeft InputWidth OutputWidth
    -- | Logical right shift (2 inputs, 1 output)
  | ShiftRight InputWidth OutputWidth
    -- | Arithmetic right shift (2 inputs, 1 output)
  | ArithShiftRight InputWidth OutputWidth

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
  | SelectBits InputWidth BitIndex BitIndex -- Hi then Low
    -- | Bit vector concatenation (2 inputs, 1 output)
  | Concat InputWidth InputWidth

    -- | Multiplexer (n inputs, 1 output)
  | Mux Int OutputWidth
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
  | Custom { customName      :: String                  -- component name
           , customInputs    :: [(String, InputWidth)]  -- input names
           , customOutputs   :: [(String, OutputWidth)] -- output names/widths
           , customParams    :: [Param]                 -- parameters
           , customIsClocked :: Bool }                  -- pass clock (reset?)

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
  | RegFileMake RegFileInfo
    -- | Register file lookup (input: index, output: data)
  | RegFileRead RegFileInfo
    -- | Register file update (inputs: write-enable, address, data)
  | RegFileWrite RegFileInfo
  deriving Show

-- | Helper to tell whether a 'Prim' can be inlined during Netlist optimisation
canInline :: Prim -> Bool
canInline = isInlineable . primInfo

-- | Helper to tell whether a 'Prim' inputs can be inlined during Netlist
--   optimisation
canInlineInput :: Prim -> Bool
canInlineInput = inputsInlineable . primInfo

-- | Helper to render a primitive name. Used to generate useful names
primStr :: Prim -> String
primStr = strRep . primInfo

-- | Helper to tell if a 'Prim' can be optimized away or not
primDontKill :: Prim -> Bool
primDontKill = dontKill . primInfo

-- | Helper to tell if a 'Prim' is a netlist root
primIsRoot :: Prim -> Bool
primIsRoot = isRoot . primInfo

-- | Helper to get the inputs of a 'Prim'
primInputs :: Prim -> [(String, InputWidth)]
primInputs = inputs . primInfo

-- | Helper to get the outputs of a 'Prim'
primOutputs :: Prim -> [(String, OutputWidth)]
primOutputs = outputs . primInfo

-- | Helper to get the 'OutputWidth' for a given named output of a 'Prim'
primOutWidth :: Prim -> OutputName -> OutputWidth
primOutWidth prim out
  | length outs == 0 = error $ "primOutWidth: " ++
                               primStr prim ++ " has no output"
  | length outs == 1 = (snd . head) outs
  | otherwise = case out of
                  Just nm -> fromMaybe (error $ "primOutWidth: " ++
                                        show out ++ " output does not exist")
                                       (lookup nm outs)
                  _ -> error "Must use primOutWidth with a 'Just name'"
  where outs = primOutputs prim

-- | A 'Name' type that handles name hints
data Name = Name { nameHints :: Set String } deriving Show
instance Semigroup Name where
  x <> y = Name { nameHints = nameHints x `union` nameHints y }
instance Monoid Name where
  mempty = Name { nameHints = empty }

-- Internal functions
--------------------------------------------------------------------------------

-- | Type to hold metadata about a 'Prim'
data PrimInfo = PrimInfo {
  -- | Tells whether a 'Prim' can be inlined during the netlist
  -- optimisation passes. When generating verilog, it is useful to avoid
  -- inlining of certain primitive such as 'Input' or 'Display' or stateful
  -- ones like 'Register'...
  isInlineable :: Bool
  -- | Tells whether a 'Prim' ' inputs can be inlined during the netlist
  -- optimisation passes. When generating verilog, it is useful to avoid
  -- inlining inputs to 'Prim's involving bit-slicing (specifically,
  -- 'SelectBits' and 'SignExtend') due to lack of underlying support.
, inputsInlineable :: Bool
  -- | Tells a 'String' representation of a 'Prim'.
, strRep :: String
  -- | Tells if a 'Prim' can be optimized away or not
, dontKill :: Bool
  -- | Tells if a 'Prim' is a netlist root
, isRoot :: Bool
  -- | Tells the number and widths of inputs to a 'Prim'
, inputs :: [(String, InputWidth)]
  -- | Tells the number and widths of outputs of a 'Prim'
, outputs :: [(String, OutputWidth)]
}
-- | Helper getting general metadata about a 'Prim'
primInfo :: Prim -> PrimInfo
primInfo (Const w x) = PrimInfo { isInlineable = True
                                , inputsInlineable = True
                                , strRep = "Const" ++ show x
                                , dontKill = False
                                , isRoot = False
                                , inputs = []
                                , outputs = [("out", w)] }
primInfo (DontCare w) = PrimInfo { isInlineable = True
                                 , inputsInlineable = True
                                 , strRep = "DontCare"
                                 , dontKill = False
                                 , isRoot = False
                                 , inputs = []
                                 , outputs = [("out", w)] }
primInfo (Add w) = PrimInfo { isInlineable = True
                            , inputsInlineable = True
                            , strRep = "Add"
                            , dontKill = False
                            , isRoot = False
                            , inputs = [("in0", w), ("in1", w)]
                            , outputs = [("out", w)] }
primInfo (Sub w) = PrimInfo { isInlineable = True
                            , inputsInlineable = True
                            , strRep = "Sub"
                            , dontKill = False
                            , isRoot = False
                            , inputs = [("in0", w), ("in1", w)]
                            , outputs = [("out", w)] }
primInfo (Mul w) = PrimInfo { isInlineable = True
                            , inputsInlineable = True
                            , strRep = "Mul"
                            , dontKill = False
                            , isRoot = False
                            , inputs = [("in0", w), ("in1", w)]
                            , outputs = [("out", w)] }
primInfo (Div w) = PrimInfo { isInlineable = True
                            , inputsInlineable = True
                            , strRep = "Div"
                            , dontKill = False
                            , isRoot = False
                            , inputs = [("in0", w), ("in1", w)]
                            , outputs = [("out", w)] }
primInfo (Mod w) = PrimInfo { isInlineable = True
                            , inputsInlineable = True
                            , strRep = "Mod"
                            , dontKill = False
                            , isRoot = False
                            , inputs = [("in0", w), ("in1", w)]
                            , outputs = [("out", w)] }
primInfo (Not w) = PrimInfo { isInlineable = True
                            , inputsInlineable = True
                            , strRep = "Not"
                            , dontKill = False
                            , isRoot = False
                            , inputs = [("in", w)]
                            , outputs = [("out", w)] }
primInfo (And w) = PrimInfo { isInlineable = True
                            , inputsInlineable = True
                            , strRep = "And"
                            , dontKill = False
                            , isRoot = False
                            , inputs = [("in0", w), ("in1", w)]
                            , outputs = [("out", w)] }
primInfo (Or w) = PrimInfo { isInlineable = True
                           , inputsInlineable = True
                           , strRep = "Or"
                           , dontKill = False
                           , isRoot = False
                           , inputs = [("in0", w), ("in1", w)]
                           , outputs = [("out", w)] }
primInfo (Xor w) = PrimInfo { isInlineable = True
                            , inputsInlineable = True
                            , strRep = "Xor"
                            , dontKill = False
                            , isRoot = False
                            , inputs = [("in0", w), ("in1", w)]
                            , outputs = [("out", w)] }
primInfo (ShiftLeft iw ow) = PrimInfo { isInlineable = True
                                      , inputsInlineable = True
                                      , strRep = "ShiftLeft"
                                      , dontKill = False
                                      , isRoot = False
                                      , inputs = [("in0", ow), ("in1", iw)]
                                      , outputs = [("out", ow)] }
primInfo (ShiftRight iw ow) = PrimInfo { isInlineable = True
                                       , inputsInlineable = True
                                       , strRep = "ShiftRight"
                                       , dontKill = False
                                       , isRoot = False
                                       , inputs = [("in0", ow), ("in1", iw)]
                                       , outputs = [("out", ow)] }
primInfo (ArithShiftRight iw ow) = PrimInfo { isInlineable = True
                                            , inputsInlineable = True
                                            , strRep = "ArithShiftRight"
                                            , dontKill = False
                                            , isRoot = False
                                            , inputs = [("in0", ow), ("in1", iw)]
                                            , outputs = [("out", ow)] }
primInfo (Equal w) = PrimInfo { isInlineable = True
                              , inputsInlineable = True
                              , strRep = "Equal"
                              , dontKill = False
                              , isRoot = False
                              , inputs = [("in0", w), ("in1", w)]
                              , outputs = [("out", 1)] }
primInfo (NotEqual w) = PrimInfo { isInlineable = True
                                 , inputsInlineable = True
                                 , strRep = "NotEqual"
                                 , dontKill = False
                                 , isRoot = False
                                 , inputs = [("in0", w), ("in1", w)]
                                 , outputs = [("out", 1)] }
primInfo (LessThan w) = PrimInfo { isInlineable = True
                                 , inputsInlineable = True
                                 , strRep = "LessThan"
                                 , dontKill = False
                                 , isRoot = False
                                 , inputs = [("in0", w), ("in1", w)]
                                 , outputs = [("out", 1)] }
primInfo (LessThanEq w) = PrimInfo { isInlineable = True
                                   , inputsInlineable = True
                                   , strRep = "LessThanEq"
                                   , dontKill = False
                                   , isRoot = False
                                   , inputs = [("in0", w), ("in1", w)]
                                   , outputs = [("out", 1)] }
primInfo (ReplicateBit w) = PrimInfo { isInlineable = True
                                     , inputsInlineable = True
                                     , strRep = "ReplicateBit"
                                     , dontKill = False
                                     , isRoot = False
                                     , inputs = [("in", 1)]
                                     , outputs = [("out", w)] }
primInfo (ZeroExtend iw ow) = PrimInfo { isInlineable = True
                                       , inputsInlineable = True
                                       , strRep = "ZeroExtend"
                                       , dontKill = False
                                       , isRoot = False
                                       , inputs = [("in", iw)]
                                       , outputs = [("out", ow)] }
primInfo (SignExtend iw ow) = PrimInfo { isInlineable = True
                                       , inputsInlineable = False
                                       , strRep = "SignExtend"
                                       , dontKill = False
                                       , isRoot = False
                                       , inputs = [("in", iw)]
                                       , outputs = [("out", ow)] }
primInfo (SelectBits iw hi lo) =
  PrimInfo { isInlineable = True
           , inputsInlineable = False
           , strRep = "SelectBits"
           , dontKill = False
           , isRoot = False
           , inputs = [("in", iw)]
           , outputs = [("out", hi - lo + 1)] }
primInfo (Concat w0 w1) = PrimInfo { isInlineable = True
                                   , inputsInlineable = True
                                   , strRep = "Concat"
                                   , dontKill = False
                                   , isRoot = False
                                   , inputs = [("in0", w0), ("in1", w1)]
                                   , outputs = [("out", w0 + w1)] }
primInfo (Mux n w) = PrimInfo { isInlineable = False
                              , inputsInlineable = True
                              , strRep = "Mux"
                              , dontKill = False
                              , isRoot = False
                              , inputs = ("sel", log2ceil n)
                                         : [("in" ++ show i, w) | i <- [0..n-1]]
                              , outputs = [("out", w)] }
primInfo (Identity w) = PrimInfo { isInlineable = True
                                 , inputsInlineable = True
                                 , strRep = "Identity"
                                 , dontKill = False
                                 , isRoot = False
                                 , inputs = [("in", w)]
                                 , outputs = [("out", w)] }
primInfo (Register _ w) = PrimInfo { isInlineable = False
                                   , inputsInlineable = True
                                   , strRep = "Register"
                                   , dontKill = False
                                   , isRoot = False
                                   , inputs = [("in", w)]
                                   , outputs = [("out", w)] }
primInfo (RegisterEn _ w) = PrimInfo { isInlineable = False
                                     , inputsInlineable = True
                                     , strRep = "RegisterEn"
                                     , dontKill = False
                                     , isRoot = False
                                     , inputs = [("en", 1), ("in", w)]
                                     , outputs = [("out", w)] }
primInfo BRAM{ ramAddrWidth = aw
             , ramDataWidth = dw } =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "BRAM"
           , dontKill = False
           , isRoot = False
           , inputs = [("addr", aw), ("data_in", dw)]
           , outputs = [("data_out", dw)] }
primInfo TrueDualBRAM{ ramAddrWidth = aw
                     , ramDataWidth = dw } =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "TrueDualBRAM"
           , dontKill = False
           , isRoot = False
           , inputs = [("addr", aw), ("data_in", dw)]
           , outputs = [("data_out", dw)] }
primInfo Custom{ customName = custNm
               , customInputs = ins
               , customOutputs = outs } =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = custNm
           , dontKill = False
           , isRoot = False
           , inputs = ins
           , outputs = outs }
primInfo (Input w nm) = PrimInfo { isInlineable = False
                                 , inputsInlineable = True
                                 , strRep = nm
                                 , dontKill = False
                                 , isRoot = True
                                 , inputs = []
                                 , outputs = [("out", w)] }
primInfo (Output w nm) =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = nm
           , dontKill = False
           , isRoot = True
           , inputs = [("in", w)]
           , outputs = [] }
primInfo (Display args) =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "Display"
           , dontKill = True
           , isRoot = True
           , inputs = [ ("in" ++ show i, w)
                      | (i, DisplayArgBit w) <- zip [0..] args ]
           , outputs = [] }
primInfo Finish =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "Finish"
           , dontKill = True
           , isRoot = True
           , inputs = []
           , outputs = [] }
primInfo (TestPlusArgs arg) = PrimInfo { isInlineable = False
                                       , inputsInlineable = True
                                       , strRep = "PlusArgs_" ++ arg
                                       , dontKill = True
                                       , isRoot = True
                                       , inputs = []
                                       , outputs = [("out", 1)] }
primInfo RegFileMake{} =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "RegFileMake"
           , dontKill = True
           , isRoot = True
           , inputs = []
           , outputs = [] }
primInfo (RegFileRead RegFileInfo{ regFileAddrWidth = aw
                                 , regFileDataWidth = dw }) =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "RegFileRead"
           , dontKill = False
           , isRoot = False
           , inputs = [("idx", aw)]
           , outputs = [("data", dw)] }
primInfo (RegFileWrite RegFileInfo{ regFileAddrWidth = aw
                                  , regFileDataWidth = dw }) =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "RegFileWrite"
           , dontKill = False
           , isRoot = True
           , inputs = [("idx", aw), ("data", dw)]
           , outputs = [] }
