{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE NoRebindableSyntax #-}

{- |
Module      : Blarney.Core.Prim
Description : circuit primitives
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019-2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

This module provides a set of circuit primitives.
-}

module Blarney.Core.Prim (
  -- * 'Prim' primitive type and helpers
  Prim(..)            -- Primitive components
, canInline           -- Tell if a 'Prim' can be inlined
, canInlineInput      -- Tell if a 'Prim' inputs can be inlined
, clamp
, primStr             -- get useful name strings out of a 'Prim'
, primSemEvalRaw
, primSemEval
, primDontKill        -- tell if a 'Prim' can be optimised away
, primIsRoot          -- tell if a 'Prim' is a netlist root
, primInputs          -- get the inputs of a 'Prim'
, primOutputs         -- get the outputs of a 'Prim'
, primOutIndex        -- get output index from name
, primOutWidth        -- get 'OutputWidth' for a given named output of a 'Prim'
, BRAMKind(..)        -- Block RAM kind
  -- * Other primitive types
, InstId              -- Every component instance has a unique id
, Width               -- Bit vector width
, InputWidth          -- Width of an input to a component
, OutputWidth         -- Width of an output from a component
, OutputName          -- Reference to a named output of a 'Prim'
, BitIndex            -- For indexing a bit vector
, MergeStrategy(..)   -- Merging strategy for a 'Merge' 'Prim'
, RegFileInfo(..)     -- Register file primitive parameters
, DisplayArg(..)      -- Arguments to display primitive
, DisplayArgRadix(..) -- Radix of argument to display
, Param(..)           -- Compile-time parameters
, NameHint(..)        -- A 'NameHint' type to represent name hints
, NameHints           -- A 'NameHints' type to gather name hints
  -- * Netlists
, Net(..)             -- 'Net' type to represent 'Netlist' nodes
, WireId              -- 'WireId' type to uniquely identify wires
, NetInput(..)        -- 'NetInput' type to represent inputs to 'Net's
, Netlist             -- 'Netlist' type to represent a circuit
, CustomNetlist(..)   -- Wrapper type to represent the Netlist for a Custom Prim
) where

import Prelude
import Data.Set ( Set, union, empty )
import Data.List ( elemIndex )
import Data.Bits ( Bits, (.&.), (.|.), shiftL, shiftR, testBit, bit
                 , complement, zeroBits, xor )
import Data.Maybe
import Data.Array

import Blarney.Core.Utils

-- module-local "error" helper
err str = error $ "Blarney.Core.prim: " ++ str

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

-- | Merging Strategy
data MergeStrategy = MStratOr deriving (Eq, Show)

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
    -- | Subsequences of arguments of a display can be conditionally displayed
  | DisplayCondBlockBegin
  | DisplayCondBlockEnd
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

    -- | @Sub w@ represents a subtractor
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

    -- | @Mux n wsel w@ represents an @n@-input multiplexer
    --
    --   [__inputs__]  @sel:inpts@, with @sel@ a @(log2 n)@-bit value and
    --                 @inpts@ an @n@-sized list of @w@-bit values
    --   [__outputs__] a single output, the @w@-bit value at position @sel@ in
    --                 @inpts@ (the width of @sel@ is @wsel@)
  | Mux Int InputWidth OutputWidth

    -- | @Identity w@ represents an identity function
    --
    --   [__inputs__]  @[x]@, a single @w@-bit value
    --   [__outputs__] a single output, the @w@-bit input value @x@
  | Identity OutputWidth

    -- | @Register initial w@ represents a register with an initial value.
    --   An initial value of 'Nothing' denotes "dont care".
    --
    --   [__inputs__]  @[x]@, a single @w@-bit value
    --   [__outputs__] a single output, the @w@-bit value @initial@ or the last
    --                 written input value @x@
  | Register (Maybe Integer) InputWidth

    -- | @RegisterEn initial w@ represents a register with an initial value
    --   and an enable signal. An initial value of 'Nothing' denotes
    --   "dont care"
    --
    --   [__inputs__]  @[en, x]@, with @en@ a 1-bit value and @x@ a @w@-bit
    --                 value
    --   [__outputs__] a single output, the @w@-bit value @initial@ or the last
    --                 written input value @x@ when @en@ was asserted
  | RegisterEn (Maybe Integer) InputWidth

    -- | @MergeWrites mStrat n w@ represents a merging primitive with the
    --   @mStrat@ merging strategy, @n@ pairs of 1-bit enables and associated
    --   @w@-wide inputs, and one @w@-wide output
    --
    --   [__inputs__]  @[en0, in0, en1, in1, ...]@, @n@ pairs of @enN@ 1-bit
    --                 enables and @inN@ @w@-bit values
    --   [__outputs__] a single output, a @w@-bit value result of the merging of
    --                 the @n@ inputs according to @mStrat@
  | MergeWrites MergeStrategy Int Width

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
  | Custom { customName      :: String                  -- ^ Component name
           , customInputs    :: [(String, InputWidth)]  -- ^ Input names
           , customOutputs   :: [(String, OutputWidth)] -- ^ Output names/widths
           , customParams    :: [Param]                 -- ^ Parameters
           , customIsClocked :: Bool                    -- ^ Pass clock?
           , customResetable :: Bool                    -- ^ Pass reset?
           , customNetlist   :: Maybe CustomNetlist
             -- ^ Optional netlist for this component
           }

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

-- | constraint on samples that can be fed to primitives
type PrimSample a = (Num a, Ord a, Integral a, Bits a)

-- | Helper to retrieve a primitive's semantic evaluation function or lack
--   thereof
primSemEvalRaw :: PrimSample a => Prim -> Maybe ([a] -> [a])
primSemEvalRaw p = semEval (primInfo p)

-- | Helper to retrieve a primitive's semantic evaluation function
primSemEval :: PrimSample a => Prim -> [a] -> [a]
primSemEval prim = fromMaybe (err $ "no semEval for " ++ show prim)
                             (primSemEvalRaw prim)

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

-- | Helper to get the index of a given named output of a 'Prim'
primOutIndex :: Prim -> OutputName -> Maybe Int
primOutIndex prim (Just nm) = elemIndex nm (fmap fst $ primOutputs prim)
primOutIndex prim Nothing = if length (primOutputs prim) == 1 then Just 0
                                                              else Nothing

-- | Helper to get the 'OutputWidth' for a given named output of a 'Prim'
primOutWidth :: Prim -> OutputName -> OutputWidth
primOutWidth prim out
  | length outs == 0 = err $ "primOutWidth: " ++
                             primStr prim ++ " has no output"
  | length outs == 1 = (snd . head) outs
  | otherwise = case out of
                  Just nm -> fromMaybe (err $ "primOutWidth: " ++
                                        show out ++ " output does not exist")
                                       (lookup nm outs)
                  _ -> err "Must use primOutWidth with a 'Just name'"
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
  -- | Gives a 'String' representation of a 'Prim'.
, strRep :: String
  -- | if possible, provides a semantic evaluation function for primitives
, semEval :: forall a. PrimSample a => Maybe ([a] -> [a])
  -- | Tells if a 'Prim' can be optimized away or not
, dontKill :: Bool
  -- | Tells if a 'Prim' is a netlist root
, isRoot :: Bool
  -- | Tells the number and widths of inputs to a 'Prim'
, inputs :: [(String, InputWidth)]
  -- | Tells the number and widths of outputs of a 'Prim'
, outputs :: [(String, OutputWidth)] }

-- | clamp a given sample to the provided width
clamp :: (Num a, Bits a) => Width -> a -> a
clamp w x = (bit w - 1) .&. x

toSigned :: (Ord a, Num a) => Width -> a -> a
toSigned w x | x >= 0 = if x >= 2^(w-1) then x - 2^w else x
             | otherwise = err $ "cannot toSigned on negative number"

fromSigned :: (Ord a, Num a) => Width -> a -> a
fromSigned w x = if x < 0 then x + 2^w else x

-- | Helper getting general metadata about a 'Prim'
primInfo :: Prim -> PrimInfo
primInfo (Const w x) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "Const" ++ show x
           , semEval = Just \_ -> [clamp w (fromIntegral x)]
           , dontKill = False
           , isRoot = False
           , inputs = []
           , outputs = [("out", w)] }
primInfo (DontCare w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "DontCare"
           , semEval = Just \_ -> [0]
           , dontKill = False
           , isRoot = False
           , inputs = []
           , outputs = [("out", w)] }
primInfo (Add w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "Add"
           , semEval = Just \[i0, i1] -> [clamp w $ i0 + i1]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", w), ("in1", w)]
           , outputs = [("out", w)] }
primInfo (Sub w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "Sub"
           , semEval = Just \[i0, i1] -> [clamp w $ i0 - i1]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", w), ("in1", w)]
           , outputs = [("out", w)] }
primInfo (Mul w isSigned isFull) =
  PrimInfo { isInlineable = not isFull
           , inputsInlineable = True
           , strRep = "Mul"
           , semEval = Just \[i0, i1] ->
               let ow = if isFull then 2 * w else w
                   x  = toSigned w i0
                   y  = toSigned w i1
                   r  = if isSigned then fromSigned ow (x * y) else i0 * i1
               in [clamp ow r]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", w), ("in1", w)]
           , outputs = [("out", wout)] }
  where
    wout = case isFull of { True -> 2*w; False -> w }
primInfo (Div w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "Div"
           , semEval = Just \[i0, i1] -> [clamp w $ i0 `div` i1]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", w), ("in1", w)]
           , outputs = [("out", w)] }
primInfo (Mod w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "Mod"
           , semEval = Just \[i0, i1] -> [clamp w $ i0 `mod` i1]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", w), ("in1", w)]
           , outputs = [("out", w)] }
primInfo (Not w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "Not"
           , semEval = Just \[i0] -> [clamp w $ complement i0]
           , dontKill = False
           , isRoot = False
           , inputs = [("in", w)]
           , outputs = [("out", w)] }
primInfo (And w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "And"
           , semEval = Just \[i0, i1] -> [clamp w $ i0 .&. i1]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", w), ("in1", w)]
           , outputs = [("out", w)] }
primInfo (Or w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "Or"
           , semEval = Just \[i0, i1] -> [clamp w $ i0 .|. i1]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", w), ("in1", w)]
           , outputs = [("out", w)] }
primInfo (Xor w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "Xor"
           , semEval = Just \[i0, i1] -> [clamp w $ i0 `xor` i1]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", w), ("in1", w)]
           , outputs = [("out", w)] }
primInfo (ShiftLeft iw ow) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "ShiftLeft"
           , semEval =
               Just \[i0, i1] -> [clamp ow $ i0 `shiftL` fromIntegral i1]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", ow), ("in1", iw)]
           , outputs = [("out", ow)] }
primInfo (ShiftRight iw ow) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "ShiftRight"
           , semEval =
               Just \[i0, i1] -> [clamp ow $ i0 `shiftR` fromIntegral i1]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", ow), ("in1", iw)]
           , outputs = [("out", ow)] }
primInfo (ArithShiftRight iw ow) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "ArithShiftRight"
           , semEval = Just \[i0, i1] ->
               let x = toSigned iw i0
                   r = fromSigned ow (x `shiftR` fromIntegral i1)
               in [clamp ow r]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", ow), ("in1", iw)]
           , outputs = [("out", ow)] }
primInfo (Equal w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "Equal"
           , semEval = Just \[i0, i1] -> [if i0 == i1 then 1 else 0]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", w), ("in1", w)]
           , outputs = [("out", 1)] }
primInfo (NotEqual w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "NotEqual"
           , semEval = Just \[i0, i1] -> [if i0 /= i1 then 1 else 0]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", w), ("in1", w)]
           , outputs = [("out", 1)] }
primInfo (LessThan w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "LessThan"
           , semEval = Just \[i0, i1] -> [if i0 < i1 then 1 else 0]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", w), ("in1", w)]
           , outputs = [("out", 1)] }
primInfo (LessThanEq w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "LessThanEq"
           , semEval = Just \[i0, i1] -> [if i0 <= i1 then 1 else 0]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", w), ("in1", w)]
           , outputs = [("out", 1)] }
primInfo (ReplicateBit w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "ReplicateBit"
           , semEval =
               Just \[i0] -> [clamp w if i0 == 0 then zeroBits
                                                 else complement zeroBits]
           , dontKill = False
           , isRoot = False
           , inputs = [("in", 1)]
           , outputs = [("out", w)] }
primInfo (ZeroExtend iw ow) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "ZeroExtend"
           , semEval = Just \[i0] -> [clamp iw i0]
           , dontKill = False
           , isRoot = False
           , inputs = [("in", iw)]
           , outputs = [("out", ow)] }
primInfo (SignExtend iw ow) =
  PrimInfo { isInlineable = True
           , inputsInlineable = False
           , strRep = "SignExtend"
           , semEval =
               Just \[i0] -> let sgn = testBit i0 (iw - 1)
                                 sgnMask = (bit (ow-iw+1) - 1) `shiftL` iw
                                 sgnExt = i0 .|. sgnMask
                             in [if sgn then sgnExt else clamp ow i0]
           , dontKill = False
           , isRoot = False
           , inputs = [("in", iw)]
           , outputs = [("out", ow)] }
primInfo (SelectBits iw hi lo) =
  PrimInfo { isInlineable = True
           , inputsInlineable = False
           , strRep = "SelectBits"
           , semEval = Just \[i0] -> [clamp (hi + 1) i0 `shiftR` lo]
           , dontKill = False
           , isRoot = False
           , inputs = [("in", iw)]
           , outputs = [("out", hi - lo + 1)] }
primInfo (Concat w0 w1) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "Concat"
           , semEval = Just \[i0, i1] -> [clamp (w0 + w1) $ i0 `shiftL` w1 + i1]
           , dontKill = False
           , isRoot = False
           , inputs = [("in0", w0), ("in1", w1)]
           , outputs = [("out", w0 + w1)] }
primInfo (Mux n wsel w) =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "Mux"
           , semEval = Just \(s:is) ->
               [is !! (fromIntegral s `mod` n)]
           , dontKill = False
           , isRoot = False
           , inputs = ("sel", wsel)
                      : [("in" ++ show i, w) | i <- [0..n-1]]
           , outputs = [("out", w)] }
primInfo (Identity w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "Identity"
           , semEval = Just id
           , dontKill = False
           , isRoot = False
           , inputs = [("in", w)]
           , outputs = [("out", w)] }
primInfo prim@(Register _ w) =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "Register"
           , semEval = Nothing
           , dontKill = False
           , isRoot = False
           , inputs = [("in", w)]
           , outputs = [("out", w)] }
primInfo prim@(RegisterEn _ w) =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "RegisterEn"
           , semEval = Nothing
           , dontKill = False
           , isRoot = False
           , inputs = [("en", 1), ("in", w)]
           , outputs = [("out", w)] }
primInfo prim@(MergeWrites mStrat n w) =
  PrimInfo { isInlineable = True
           , inputsInlineable = True
           , strRep = "MergeWrites{" ++ show mStrat ++ "}"
           , semEval = Just \ins ->
               let stratOr acc [] = acc
                   stratOr acc (en:x:rest) =
                     stratOr (if en == 1 then acc .|. x else acc) rest
                   stratOr _ _ = err "malformed MergeWrites inputs"
               in [clamp w $ case mStrat of _ -> stratOr 0 ins]
           , dontKill = False
           , isRoot = False
           , inputs = concat [ [("en" ++ show i, 1), ("in" ++ show i, w)]
                             | i <- [0..n-1] ]
           , outputs = [("out", w)] }
primInfo prim@BRAM{ ramAddrWidth = aw
                  , ramDataWidth = dw
                  , ramHasByteEn = hasBE
                  , ramKind      = kind } =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = str ++ (case hasBE of True -> "BE"; False -> "")
           , semEval = Nothing
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
primInfo prim@Custom{ customName = custNm
                    , customInputs = ins
                    , customOutputs = outs } =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = custNm
           , semEval = Nothing
           , dontKill = False
           , isRoot = False
           , inputs = ins
           , outputs = outs }
primInfo prim@(Input w nm) =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = nm
           , semEval = Nothing
           , dontKill = False
           , isRoot = True
           , inputs = []
           , outputs = [("out", w)] }
primInfo prim@(Output w nm) =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = nm
           , semEval = Nothing
           , dontKill = False
           , isRoot = True
           , inputs = [("in", w)]
           , outputs = [] }
primInfo prim@(Display args) =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "Display"
           , semEval = Nothing
           , dontKill = True
           , isRoot = True
           , inputs = ("en", 1) :
                      [ ("in" ++ show i, w)
                      | (i, DisplayArgBit w _ _ _) <- zip [0..] args ]
           , outputs = [] }
primInfo prim@Finish =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "Finish"
           , semEval = Nothing
           , dontKill = True
           , isRoot = True
           , inputs = [("en", 1)]
           , outputs = [] }
primInfo prim@(TestPlusArgs arg) =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "PlusArgs_" ++ arg
           , semEval = Nothing
           , dontKill = True
           , isRoot = True
           , inputs = []
           , outputs = [("out", 1)] }
primInfo prim@(Assert _) =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "Assert"
           , semEval = Nothing
           , dontKill = True
           , isRoot = True
           , inputs = [("cond", 1), ("pred", 1)]
           , outputs = [] }
primInfo prim@RegFileMake{} =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "RegFileMake"
           , semEval = Nothing
           , dontKill = True
           , isRoot = True
           , inputs = []
           , outputs = [] }
primInfo prim@(RegFileRead RegFileInfo{ regFileAddrWidth = aw
                                      , regFileDataWidth = dw }) =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "RegFileRead"
           , semEval = Nothing
           , dontKill = False
           , isRoot = False
           , inputs = [("idx", aw)]
           , outputs = [("data", dw)] }
primInfo prim@(RegFileWrite RegFileInfo{ regFileAddrWidth = aw
                                       , regFileDataWidth = dw }) =
  PrimInfo { isInlineable = False
           , inputsInlineable = True
           , strRep = "RegFileWrite"
           , semEval = Nothing
           , dontKill = False
           , isRoot = True
           , inputs = [("idx", aw), ("data", dw)]
           , outputs = [] }

-- Nets and netlists
-- =================

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

-- | A 'Netlist', represented as an 'Array InstId Net'
type Netlist = Array InstId Net

-- | Netlist for a Custom Prim. We use a new type here so that we can keep
--   automatic deriving of the 'Show' class for 'Prim'.
newtype CustomNetlist = CustomNetlist Netlist

instance Show CustomNetlist where
  show _ = "CustomNetlist"
