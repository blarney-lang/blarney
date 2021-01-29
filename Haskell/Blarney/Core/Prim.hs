{-|
Module      : Blarney.Core.Prim
Description : circuit primitives
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019-2020
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
, BRAMKind(..)    -- Block RAM kind
  -- * Other primitive types
, InstId          -- Every component instance has a unique id
, Width           -- Bit vector width
, InputWidth      -- Width of an input to a component
, OutputWidth     -- Width of an output from a component
, OutputName      -- Reference to a named output of a 'Prim'
, BitIndex        -- For indexing a bit vector
, RegFileInfo(..) -- Register file primitive parameters
, DisplayArg(..)  -- Arguments to display primitive
, DisplayArgRadix(..) -- Radix of argument to display
, Param(..)       -- Compile-time parameters
, NameHint(..)    -- A 'NameHint' type to represent name hints
, NameHints       -- A 'NameHints' type to gather name hints
) where

import Prelude
import Data.Set
import Data.Maybe

import Blarney.Core.Utils

-- | Unique identifier used to identify every instance of a component in a
--   circuit
type InstId = Int

-- | Hint to generate useful names when working with 'Net's
data NameHint = NmPrefix Int String -- ^ Suggested name prefix
              | NmRoot Int String   -- ^ Suggested name
              | NmSuffix Int String -- ^ Suggested name suffix
              deriving (Eq, Ord, Show)
-- | A set of 'NameHint's
type NameHints = Set NameHint

-- | Bit vector width
type Width = Int

-- | Width of an input to a primitive
type InputWidth = Width

-- | Width of an output from a primitive
type OutputWidth = Width

-- | Reference to a named output of a 'Prim'
type OutputName = Maybe String

-- | Index into a bit vector
type BitIndex = Int

-- | Register file primitive parameters
data RegFileInfo = RegFileInfo {
  regFileId        :: Int        -- ^ Register file unique identifier
, regFileInitFile  :: String     -- ^ Initialisation file
, regFileAddrWidth :: InputWidth -- ^ Address width
, regFileDataWidth :: Width      -- ^ Data width
} deriving Show

-- | Custom components may have compile-time parameters.
--   A parameter has a name and a value, both represented as strings
data Param = String :-> String deriving Show

-- | For the Display primitive: display a string literal or
-- a bit-vector value of a given width
data DisplayArg =
    -- | Display a string
    DisplayArgString String
    -- | Display a bit vector with formatting options
  | DisplayArgBit {
      -- | Radix of bit vector to display
      displayArgWidth :: InputWidth
      -- | Radix of bit vector to display
    , displayArgRadix :: DisplayArgRadix
      -- | Optional padding
    , displayArgPad :: Maybe Int
      -- | Pad with zeros or spaces?
    , displayArgZeroPad :: Bool
    }
  deriving Show

-- | Format for displaying bit vectors
data DisplayArgRadix = Bin -- ^ Binary radix
                     | Dec -- ^ Decimal radix
                     | Hex -- ^ Hexadecimal radix
  deriving Show

-- | The 'Prim' type is used to represent the fundamental operations in blarney.
--   Each 'Prim' constructor can expect some statically known arguments such as
--   desired widths for inputs or outputs, initial values for registers, etc...
--   Each 'Prim' will expect to be used with a list of inputs, and to provide a
--   list of outputs. The shape of these lists is documented here for
--   convenience, but these are /not/ enforced in the 'Prim' type.
--   The 'primInputs' and 'primOutputs' functions can be used to query the shape
--   of the inputs and outpus lists that can be used for a given 'Prim'.
data Prim =
    -- | @Const w n@ represents a constant value
    --
    --   [__inputs__]  no input, i.e. @[]@
    --   [__outputs__] a single output, the @w@-bit value @n@
    Const OutputWidth Integer

    -- | @DontCare w@ represents a don't care value
    --
    --   [__inputs__]  no input, i.e. @[]@
    --   [__outputs__] a single output, a @w@-bit don't care value
  | DontCare OutputWidth

    -- | @Add w@ represents an adder
    --
    --   [__inputs__]  @[x, y]@, two @w@-bit values
    --   [__outputs__] a single output, the @w@-bit sum @x + y@
  | Add OutputWidth

    -- | @Sub w@ represents an subtractor
    --
    --   [__inputs__]  @[x, y]@, two @w@-bit values
    --   [__outputs__] a single output, the @w@-bit difference @x - y@
  | Sub OutputWidth

    -- | @
    --   Mul { primMulInputWidth    = w
    --       , primMulSigned        = isSigned
    --       , primMulFullPrecision = isFullPrecision }
    --   @
    --   represents a multiplier
    --
    --   [__inputs__]  @[x, y]@, two @w@-bit values
    --   [__outputs__] a single output, the @wout@-bit product @x * y@ with
    --                 @wout = 2*w@ if @isFullPrecision@ is @True@ and
    --                 @wout = w@ if @isFullPrecision@ is @False@
  | Mul { primMulInputWidth    :: InputWidth -- ^ Input width
        , primMulSigned        :: Bool       -- ^ Operation is signed
        , primMulFullPrecision :: Bool       -- ^ Operation is full precision
        }

    -- | @Div w@ represents a quotient
    --
    --   [__inputs__]  @[x, y]@, two @w@-bit values
    --   [__outputs__] a single output, the @w@-bit quotient @x \`div\` y@
  | Div OutputWidth

    -- | @Mod w@ represents a remainder
    --
    --   [__inputs__]  @[x, y]@, two @w@-bit values
    --   [__outputs__] a single output, the @w@-bit remainder @x \`mod\` y@
  | Mod OutputWidth

    -- | @Not w@ represents a bitwise not
    --
    --   [__inputs__]  @[x]@, a single @w@-bit value
    --   [__outputs__] a single output, the @w@-bit bitwise not of @x@
  | Not OutputWidth

    -- | @And w@ represents a bitwise and
    --
    --   [__inputs__]  @[x, y]@, two @w@-bit values
    --   [__outputs__] a single output, the @w@-bit bitwise and @x \`&\` y@
  | And OutputWidth

    -- | @Or w@ represents a bitwise or
    --
    --   [__inputs__]  @[x, y]@, two @w@-bit values
    --   [__outputs__] a single output, the @w@-bit bitwise or @x \`|\` y@
  | Or OutputWidth

    -- | @Xor w@ represents a bitwise xor
    --
    --   [__inputs__]  @[x, y]@, two @w@-bit values
    --   [__outputs__] a single output, the @w@-bit bitwise xor @x \`^\` y@
  | Xor OutputWidth

    -- | @ShiftLeft iw ow@ represents a logical left shifter
    --
    --   [__inputs__]  @[x, y]@, with @x@ an @ow@-bit value and @y@ an @iw@-bit
    --                 value
    --   [__outputs__] a single output, the @ow@-bit logical
    --                (i.e. zero-injecting) left shift of @x@ by @y@
  | ShiftLeft InputWidth OutputWidth

    -- | @ShiftRight iw ow@ represents a logical right shifter
    --
    --   [__inputs__]  @[x, y]@, with @x@ an @ow@-bit value and @y@ an @iw@-bit
    --                 value
    --   [__outputs__] a single output, the @ow@-bit logical
    --                (i.e. zero-injecting) right shift of @x@ by @y@
  | ShiftRight InputWidth OutputWidth

    -- | @ArithShiftRight iw ow@ represents an arithmetic right shifter
    --
    --   [__inputs__]  @[x, y]@, with @x@ an @ow@-bit value and @y@ an @iw@-bit
    --                 value
    --   [__outputs__] a single output, the @ow@-bit arithmetic
    --                 (i.e. sign-preserving) right shift of @x@ by @y@
  | ArithShiftRight InputWidth OutputWidth

    -- | @Equal w@ represents an equality comparator
    --
    --   [__inputs__]  @[x, y]@, two @w@-bit values
    --   [__outputs__] a single output, the single-bit result of @x == y@
  | Equal InputWidth

    -- | @NotEqual w@ represents a difference comparator
    --
    --   [__inputs__]  @[x, y]@, two @w@-bit values
    --   [__outputs__] a single output, the single-bit result of @x /= y@
  | NotEqual InputWidth

    -- | @LessThan w@ represents a less-than comparator
    --
    --   [__inputs__]  @[x, y]@, two @w@-bit values
    --   [__outputs__] a single output, the single-bit result of @x < y@
  | LessThan InputWidth

    -- | @LessThanEq w@ represents a less-than-or-equal comparator
    --
    --   [__inputs__]  @[x, y]@, two @w@-bit values
    --   [__outputs__] a single output, the single-bit result of @x <= y@
  | LessThanEq InputWidth

    -- | @ReplicateBit w@ represents the replication of a single bit
    --
    --   [__inputs__]  @[x]@, a single 1-bit value
    --   [__outputs__] a single output, the @w@-bit value obtained with @w@
    --                 replications of @x@
  | ReplicateBit OutputWidth

    -- | @ZeroExtend iw ow@ represents zero-extension
    --
    --   [__inputs__]  @[x]@, a single @iw@-bit value
    --   [__outputs__] a single output, the @ow@-bit zero-extension of @x@
  | ZeroExtend InputWidth OutputWidth

    -- | @SignExtend iw ow@ represents sign-extension
    --
    --   [__inputs__]  @[x]@, a single @iw@-bit value
    --   [__outputs__] a single output, the @ow@-bit sign-extension of @x@
  | SignExtend InputWidth OutputWidth

    -- | @SelectBits iw hi lo@ represents bit selection (compile-time range)
    --
    --   [__inputs__]  @[x]@, a single @iw@-bit value
    --   [__outputs__] a single output, the @(hi - lo + 1)@-bit slice @x[hi:lo]@
  | SelectBits InputWidth BitIndex BitIndex -- Hi then Low

    -- | @Concat w0 w1@ represents bit vector concatenation
    --
    --   [__inputs__]  @[x, y]@, with @x@ a @w0@-bit value and @y@ an @w1@-bit
    --                 value
    --   [__outputs__] a single output, the @(w0 + w1)@-bit concatenation
    --                 @{x, y}@
  | Concat InputWidth InputWidth

    -- | @Mux n w@ represents an @n@-inputs multiplexer
    --
    --   [__inputs__]  @sel:inpts@, with @sel@ a @(log2 n)@-bit value and
    --                 @inpts@ an @n@-sized list of @w@-bit values
    --   [__outputs__] a single output, the @w@-bit value at position @sel@ in
    --                 @inpts@
  | Mux Int OutputWidth

    -- | @Identity w@ represents an identity function
    --
    --   [__inputs__]  @[x]@, a single @w@-bit value
    --   [__outputs__] a single output, the @w@-bit input value @x@
  | Identity OutputWidth

    -- | @Register initial w@ represents a register with an initial value
    --
    --   [__inputs__]  @[x]@, a single @w@-bit value
    --   [__outputs__] a single output, the @w@-bit value @initial@ or the last
    --                 written input value @x@
  | Register Integer InputWidth

    -- | @RegisterEn initial w@ represents a register with an initial value and
    --   an enable signal
    --
    --   [__inputs__]  @[en, x]@, with @en@ a 1-bit value and @x@ a @w@-bit
    --                 value
    --   [__outputs__] a single output, the @w@-bit value @initial@ or the last
    --                 written input value @x@ when @en@ was asserted
  | RegisterEn Integer InputWidth

    -- | @Input w name@ represents a named external input
    --
    --   [__inputs__]  no input, i.e. @[]@
    --   [__outputs__] a single output, the @w@-bit value received as an input
    --                 from outside the circuit
  | Input OutputWidth String

    -- | @Output w name@ represents a named external output
    --
    --   [__inputs__]  @[x]@, a single @w@-bit value
    --   [__outputs__] no output, as this primitive stands for the node that
    --                 exports @x@ outside of the circuit
  | Output InputWidth String

    -- TODO document
    -- | Block RAM
  | BRAM { ramKind      :: BRAMKind
         , ramInitFile  :: Maybe String
         , ramAddrWidth :: Width
         , ramDataWidth :: Width
         , ramHasByteEn :: Bool }

    -- TODO document
    -- | Custom component
  | Custom { customName      :: String                  -- component name
           , customInputs    :: [(String, InputWidth)]  -- input names
           , customOutputs   :: [(String, OutputWidth)] -- output names/widths
           , customParams    :: [Param]                 -- parameters
           , customIsClocked :: Bool }                  -- pass clock (reset?)

    -- TODO document
    -- | Register file declaration
    --   (only used in RTL context, not expression context)
  | RegFileMake RegFileInfo
    -- TODO document
    -- | Register file lookup (input: index, output: data)
  | RegFileRead RegFileInfo
    -- TODO document
    -- | Register file update (inputs: write-enable, address, data)
  | RegFileWrite RegFileInfo

    -- | /Not for synthesis/
    --
    --   @Display args@ outputs messages, useful for simulation
    --
    --   [__inputs__]  @en:inpts@, with @en@ a 1-bit value, and @inpts@ a list
    --                 of values to be used in conjunction with @args@
    --   [__outputs__] no output, this primitive captures displaying of
    --                 information provided through @args@ and @inpt@ when
    --                 @en@ is asserted
  | Display [DisplayArg]

    -- | /Not for synthesis/
    --
    --   @Finish@ asks for termination, useful to end simulation
    --
    --   [__inputs__]  @en@, a single 1-bit value
    --   [__outputs__] no output, this primitive simply signals termination when
    --                 @en@ is asserted
  | Finish

    -- | /Not for synthesis/
    --
    --   @TestPlusArgs plusArg@ tests for a plus-args command line argument,
    --                          useful to dynamically parameterise simulation
    --
    --   [__inputs__]  no input, i.e. @[]@
    --   [__outputs__] a single output, a 1-bit value signifying the presence or
    --                 absence of the queried @plusArg@ string on the command
    --                 line
  | TestPlusArgs String


    -- | /Not for synthesis/
    --
    --   @Assert msg@ asserts that a predicate holds
    --
    --   [__inputs__]  @[en, pred]@, two 1-bit values
    --   [__outputs__] no output, this primitive asserts that @pred@ holds when
    --                 @en@ is asserted, and may display additional information
    --                 together with @msg@
  | Assert String

  deriving Show

-- | Kind of block RAM
data BRAMKind =
    BRAMSinglePort
  | BRAMDualPort
  | BRAMTrueDualPort
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
  -- | Tells whether a 'Prim' inputs can be inlined during the netlist
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
primInfo (Mul w isSigned isFull) =
  PrimInfo { isInlineable = not isFull
           , inputsInlineable = True
           , strRep = "Mul"
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", w), ("in1", w)]
           , outputs = [("out", wout)] }
  where
    wout = case isFull of { True -> 2*w; False -> w }
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
             , ramDataWidth = dw
             , ramHasByteEn = hasBE
             , ramKind      = kind } =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = str ++ (case hasBE of True -> "BE"; False -> "")
           , dontKill = False
           , isRoot = False
           , inputs = ins
           , outputs = outs }
  where
    str  = case kind of
             BRAMSinglePort   -> "BlockRAM"
             BRAMDualPort     -> "BlockRAMDual"
             BRAMTrueDualPort -> "BlockRAMTrueDual"
    ins  = case kind of
             BRAMSinglePort   -> [("ADDR", aw), ("DI", dw),
                                  ("WE", 1), ("RE", 1)]
                              ++ [("BE", dw `div` 8) | hasBE]
             BRAMDualPort     -> [("RD_ADDR", aw), ("WR_ADDR", aw)]
                              ++ [("DI", dw), ("WE", 1), ("RE", 1)]
                              ++ [("BE", dw `div` 8) | hasBE]
             BRAMTrueDualPort -> [("ADDR_A", aw), ("ADDR_B", aw)]
                              ++ [("DI_A", dw), ("DI_B", dw)]
                              ++ [("WE_A", 1), ("WE_B", 1)]
                              ++ [("RE_A", 1), ("RE_B", 1)]
                              ++ [("BE_A", dw `div` 8) | hasBE]
                              ++ [("BE_B", dw `div` 8) | hasBE]
    outs = case kind of
             BRAMTrueDualPort -> [("DO_A", dw), ("DO_B", dw)]
             other            -> [("DO", dw)]
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
           , inputs = ("en", 1) :
                      [ ("in" ++ show i, w)
                      | (i, DisplayArgBit w _ _ _) <- zip [0..] args ]
           , outputs = [] }
primInfo Finish =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "Finish"
           , dontKill = True
           , isRoot = True
           , inputs = [("en", 1)]
           , outputs = [] }
primInfo (TestPlusArgs arg) = PrimInfo { isInlineable = False
                                       , inputsInlineable = True
                                       , strRep = "PlusArgs_" ++ arg
                                       , dontKill = True
                                       , isRoot = True
                                       , inputs = []
                                       , outputs = [("out", 1)] }
primInfo (Assert _) = PrimInfo { isInlineable = False
                               , inputsInlineable = True
                               , strRep = "Assert"
                               , dontKill = True
                               , isRoot = True
                               , inputs = [("cond", 1), ("pred", 1)]
                               , outputs = [] }
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
