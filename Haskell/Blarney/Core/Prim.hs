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
canInline = inlinable . primInfo

-- | Helper to tell whether a 'Prim' inputs can be inlined during Netlist
--   optimisation
canInlineInput :: Prim -> Bool
canInlineInput = inlinableInputs . primInfo

-- | Helper to render a primitive name. Used to generate useful names
primStr :: Prim -> String
primStr = strRep . primInfo

-- | Helper to get the 'OutputWidth' for a given named output of a 'Prim'
primOutWidth :: Prim -> OutputName -> OutputWidth
primOutWidth prim out = outputWidth (primInfo prim) out

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
                           -- | Return the 'OutputWidth' for a named output
                           --   of a 'Prim'. Usually ignores requested
                           --   'OutputName' for single output 'Prim' and return
                           --   the unambiguous output width. For multiple
                           --   output 'Prim', the 'OutputName' argument can be
                           --   used to select the desired output
                         , outputWidth :: OutputName -> OutputWidth
                         }
-- | Helper getting general metadata about a 'Prim'
primInfo :: Prim -> PrimInfo
primInfo (Const w x) = PrimInfo { inlinable = True
                                , inlinableInputs = True
                                , strRep = "Const" ++ show x
                                , outputWidth = \_ -> w }
primInfo (DontCare w) = PrimInfo { inlinable = True
                                 , inlinableInputs = True
                                 , strRep = "DontCare"
                                 , outputWidth = \_ -> w }
primInfo (Add w) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Add"
                            , outputWidth = \_ -> w }
primInfo (Sub w) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Sub"
                            , outputWidth = \_ -> w }
primInfo (Mul w) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Mul"
                            , outputWidth = \_ -> w }
primInfo (Div w) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Div"
                            , outputWidth = \_ -> w }
primInfo (Mod w) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Mod"
                            , outputWidth = \_ -> w }
primInfo (Not w) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Not"
                            , outputWidth = \_ -> w }
primInfo (And w) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "And"
                            , outputWidth = \_ -> w }
primInfo (Or w) = PrimInfo { inlinable = True
                           , inlinableInputs = True
                           , strRep = "Or"
                           , outputWidth = \_ -> w }
primInfo (Xor w) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Xor"
                            , outputWidth = \_ -> w }
primInfo (ShiftLeft w) = PrimInfo { inlinable = True
                                  , inlinableInputs = True
                                  , strRep = "ShiftLeft"
                                  , outputWidth = \_ -> w }
primInfo (ShiftRight w) = PrimInfo { inlinable = True
                                   , inlinableInputs = True
                                   , strRep = "ShiftRight"
                                   , outputWidth = \_ -> w }
primInfo (ArithShiftRight w) = PrimInfo { inlinable = True
                                        , inlinableInputs = True
                                        , strRep = "ArithShiftRight"
                                        , outputWidth = \_ -> w }
primInfo (Equal w) = PrimInfo { inlinable = True
                              , inlinableInputs = True
                              , strRep = "Equal"
                              , outputWidth = \_ -> 1 }
primInfo (NotEqual w) = PrimInfo { inlinable = True
                                 , inlinableInputs = True
                                 , strRep = "NotEqual"
                                 , outputWidth = \_ -> 1 }
primInfo (LessThan w) = PrimInfo { inlinable = True
                                 , inlinableInputs = True
                                 , strRep = "LessThan"
                                 , outputWidth = \_ -> 1 }
primInfo (LessThanEq w) = PrimInfo { inlinable = True
                                   , inlinableInputs = True
                                   , strRep = "LessThanEq"
                                   , outputWidth = \_ -> 1 }
primInfo (ReplicateBit w) = PrimInfo { inlinable = True
                                     , inlinableInputs = True
                                     , strRep = "ReplicateBit"
                                     , outputWidth = \_ -> w }
primInfo (ZeroExtend iw ow) = PrimInfo { inlinable = True
                                       , inlinableInputs = True
                                       , strRep = "ZeroExtend"
                                       , outputWidth = \_ -> ow }
primInfo (SignExtend iw ow) = PrimInfo { inlinable = True
                                       , inlinableInputs = False
                                       , strRep = "SignExtend"
                                       , outputWidth = \_ -> ow }
primInfo (SelectBits iw hi lo) = PrimInfo { inlinable = True
                                          , inlinableInputs = False
                                          , strRep = "SelectBits"
                                          , outputWidth = \_ -> hi - lo + 1 }
primInfo (Concat w0 w1) = PrimInfo { inlinable = True
                                   , inlinableInputs = True
                                   , strRep = "Concat"
                                   , outputWidth = \_ -> w0 + w1 }
primInfo (Mux w) = PrimInfo { inlinable = True
                            , inlinableInputs = True
                            , strRep = "Mux"
                            , outputWidth = \_ -> w }
primInfo (Identity w) = PrimInfo { inlinable = True
                                 , inlinableInputs = True
                                 , strRep = "Identity"
                                 , outputWidth = \_ -> w }
primInfo (Register _ w) = PrimInfo { inlinable = False
                                   , inlinableInputs = True
                                   , strRep = "Register"
                                   , outputWidth = \_ -> w }
primInfo (RegisterEn _ w) = PrimInfo { inlinable = False
                                     , inlinableInputs = True
                                     , strRep = "RegisterEn"
                                     , outputWidth = \_ -> w }
primInfo BRAM{ ramDataWidth = w } = PrimInfo { inlinable = False
                                             , inlinableInputs = True
                                             , strRep = "BRAM"
                                             , outputWidth = \_ -> w }
primInfo TrueDualBRAM{ ramDataWidth = w } =
  PrimInfo { inlinable = False
           , inlinableInputs = True
           , strRep = "TrueDualBRAM"
           , outputWidth = \_ -> w }
primInfo Custom{ customName = custNm
               , customOutputs = outs } =
  PrimInfo { inlinable = False
           , inlinableInputs = True
           , strRep = custNm
           , outputWidth = f
           }
  where f (Just nm) = fromMaybe (error $ nm ++ " is not a valid "
                                            ++ custNm ++ "output")
                                (lookup nm outs)
        f _ = error "OutputName must be specified to call outputWidth on Custom"
primInfo (Input w nm) = PrimInfo { inlinable = False
                                 , inlinableInputs = True
                                 , strRep = nm
                                 , outputWidth = \_ -> w }
primInfo (Output _ nm) = PrimInfo { inlinable = False
                                  , inlinableInputs = True
                                  , strRep = nm
                                  , outputWidth = error "outputWidth not supported on Output" }
primInfo (Display _) = PrimInfo { inlinable = False
                                , inlinableInputs = True
                                , strRep = "Display"
                                , outputWidth = error "outputWidth not supported on Display" }
primInfo Finish = PrimInfo { inlinable = False
                           , inlinableInputs = True
                           , strRep = "Finish"
                           , outputWidth = error "outputWidth not supported on Finish" }
primInfo (TestPlusArgs arg) = PrimInfo { inlinable = False
                                       , inlinableInputs = True
                                       , strRep = "PlusArgs_" ++ arg
                                       , outputWidth = \_ -> 1 }
primInfo RegFileMake{} = PrimInfo { inlinable = False
                                  , inlinableInputs = True
                                  , strRep = "RegFileMake"
                                  , outputWidth = error "outputWidth not supported on RegFileMake" }
primInfo (RegFileRead RegFileInfo{ regFileDataWidth = w }) =
  PrimInfo { inlinable = False
           , inlinableInputs = True
           , strRep = "RegFileRead"
           , outputWidth = \_ -> w }
primInfo RegFileWrite{} = PrimInfo { inlinable = False
                                   , inlinableInputs = True
                                   , strRep = "RegFileWrite"
                                   , outputWidth = error "outputWidth not supported on RegFileWrite" }
