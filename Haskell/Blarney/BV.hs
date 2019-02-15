-- For Observable Sharing
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

{-|
Module      : Blarney.BV
Description : Untyped bit vectors and circuit primitives
Copyright   : (c) Matthew Naylor, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

This module represents the core of the Blarney HDL, upon which all
hardware description features are built.  It provides untyped bit
vectors, i.e. bit vectors whose width is not specified in the type,
and a set of associated circuit primitives.  Hardware developers
should not use these untyped primitives directly (unless they know
what they are doing), because width-mistmatches are not checked.  Any
bit-vector can be evaluated symbolically, using a feature similar to
Observable Sharing [1], to produce a netlist, i.e. a graph whose nodes
are primitive component instances and whose edges are connections.

1. K. Claessen, D. Sands.  Observable Sharing for Functional Circuit
Description, ASIAN 1999.
-}
module Blarney.BV 
  (
    -- * Primitive component types
    InstId         -- Every component instance has a unique id
  , OutputNumber   -- Each output from a component is numbered
  , Width          -- Bit vector width
  , InputWidth     -- Width of an input to a component
  , OutputWidth    -- Width of an output from a component
  , BitIndex       -- For indexing a bit vector
  , RegFileId      -- Identifier for register file primitiveA
  , Prim(..)       -- Primitive components
  , ConstBit(..)   -- Constant bit (0, 1, or don't care)
  , DisplayArg(..) -- Arguments to display primitive
  , Param(..)      -- Compile-time parameters
  , lookupParam    -- Given a parameter name, return the parameter value


    -- * Untyped bit vectors
  , BV(..)         -- Untyped bit vector
  , makePrim       -- Create instance of primitive component
  , makePrim1      -- Common case: single-output components

    -- * Netlists
  , Net(..)        -- Netlists are lists of Nets
  , WireId         -- Nets are connected by wires
  , Flatten(..)    -- Monad for flattening a circuit (BV) to a netlist
  , freshId        -- Obtain a fresh instance id
  , addNet         -- Add a net to the netlist
  , flatten        -- Flatten a bit vector to a netlist

    -- * Bit-vector primitives
  , constBV        -- :: Width -> Integer -> BV
  , constBitsBV    -- :: Width -> ConstBit -> BV
  , addBV          -- :: BV -> BV -> BV
  , subBV          -- :: BV -> BV -> BV
  , mulBV          -- :: BV -> BV -> BV
  , divBV          -- :: BV -> BV -> BV
  , modBV          -- :: BV -> BV -> BV
  , invBV          -- :: BV -> BV
  , andBV          -- :: BV -> BV -> BV
  , orBV           -- :: BV -> BV -> BV
  , xorBV          -- :: BV -> BV -> BV
  , leftBV         -- :: BV -> BV -> BV
  , rightBV        -- :: BV -> BV -> BV
  , equalBV        -- :: BV -> BV -> BV
  , notEqualBV     -- :: BV -> BV -> BV
  , lessThanBV     -- :: BV -> BV -> BV
  , lessThanEqBV   -- :: BV -> BV -> BV
  , replicateBV    -- :: Integer -> BV -> BV
  , zeroExtendBV   -- :: OutputWidth -> BV -> BV
  , signExtendBV   -- :: OutputWidth -> BV -> BV
  , selectBV       -- :: (BitIndex, BitIndex) -> BV -> BV
  , concatBV       -- :: BV -> BV -> BV
  , muxBV          -- :: BV -> (BV, BV) -> BV
  , countOnesBV    -- :: OutputWidth -> BV -> BV
  , idBV           -- :: BV -> BV
  , inputPinBV     -- :: String -> BV
  , regBV          -- :: Width -> Integer -> BV -> BV
  , regEnBV        -- :: Width -> Integer -> BV -> BV -> BV
  , ramBV          -- :: OutputWidth -> Maybe String -> (BV, BV, BV) -> BV
  , dualRamBV      -- :: OutputWidth -> Maybe String
  , regFileReadBV  -- :: RegFileId -> OutputWidth -> BV -> BV
  , getInitBV      -- :: BV -> Integer
  ) where 

import Prelude

-- For join lists
import qualified Blarney.JList as JL

-- For Observable Sharing
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

-- Standard imports
import Control.Monad
import qualified Data.Bits as B

-- |Every instance of a component in the circuit has a unique id
type InstId = Int

-- |Each output from a primitive component is numbered
type OutputNumber = Int

-- |Bit vector width
type Width = Int

-- |Width of an input to a primitive
type InputWidth = Int

-- |Width of an output from a primitive
type OutputWidth = Int

-- |For indexing a bit vector
type BitIndex = Int

-- |Identifier for register file primitive
type RegFileId = Int

-- |Primitive components
data Prim =
    -- |Constant value (0 inputs, 1 output)
    Const OutputWidth Integer

    -- |Vector of constant bits
  | ConstBits OutputWidth ConstBit

    -- |Adder (2 inputs, 1 output)
  | Add OutputWidth
    -- |Subtractor (2 inputs, 1 output)
  | Sub OutputWidth
    -- |Multiplier (2 inputs, 1 output)
  | Mul OutputWidth
    -- |Quotient (2 inputs, 1 output)
  | Div OutputWidth
    -- |Remainder (2 inputs, 1 output)
  | Mod OutputWidth

    -- |Bitwise not (1 input, 1 output)
  | Not OutputWidth
    -- |Bitwise and (2 inputs, 1 output)
  | And OutputWidth
    -- |Bitwise or (2 inputs, 1 output)
  | Or OutputWidth
    -- |Bitwise xor (2 inputs, 1 output)
  | Xor OutputWidth

    -- |Left shift (2 inputs, 1 output)
  | ShiftLeft OutputWidth
    -- |Right shift (2 inputs, 1 output)
  | ShiftRight OutputWidth

    -- |Equality comparator (2 inputs, 1 bit output)
  | Equal InputWidth
    -- |Disequality comparator (2 inputs, 1 bit output)
  | NotEqual InputWidth
    -- |Less-than comparator (2 inputs, 1 bit output)
  | LessThan InputWidth
    -- |Less-than-or-equal comparator (2 inputs, 1 bit output)
  | LessThanEq InputWidth

    -- |Replicate a bit (1 input, 1 output)
  | ReplicateBit OutputWidth
    -- |Zero-extend a bit vector (1 input, 1 output)
  | ZeroExtend InputWidth OutputWidth
    -- |Sign-extend a bit vector (1 input, 1 output)
  | SignExtend InputWidth OutputWidth

    -- |Bit selection (compile-time range, 1 input, 1 output)
  | SelectBits InputWidth BitIndex BitIndex
    -- |Bit vector concatenation (2 inputs, 1 output)
  | Concat InputWidth InputWidth

    -- |Multiplexer (3 inputs, 1 output)
  | Mux OutputWidth
    -- |Population count (1 input, 1 output)
  | CountOnes OutputWidth
    -- |Identity function (1 input, 1 output)
  | Identity OutputWidth

    -- |Register with initial value (1 input, 1 output)
  | Register Integer InputWidth
    -- |Register with initial value and enable wire (2 inputs, 1 output)
  | RegisterEn Integer InputWidth

    -- |Single-port block RAM
  | BRAM { ramInitFile  :: Maybe String
         , ramAddrWidth :: Width
         , ramDataWidth :: Width }
    -- |True dual-port block RAM
  | TrueDualBRAM { ramInitFile  :: Maybe String
                 , ramAddrWidth :: Width
                 , ramDataWidth :: Width }

    -- |Custom component
    -- (component name, input names, output names/widths, parameters)
  | Custom String [String] [(String, Int)] [Param]

    -- |External input (0 inputs, 1 output)
  | Input OutputWidth String
    -- |External output (only used in RTL context, not expression context)
  | Output InputWidth String

    -- |Simulation-time I/O
    -- (only used in RTL context, not expression context)
  | Display [DisplayArg]
    -- |Finish simulation (input: 1-bit enable wire)
  | Finish

    -- |Register file declaration
    -- (only used in RTL context, not expression context)
  | RegFileMake String InputWidth InputWidth RegFileId
    -- |Register file lookup (input: index, output: data)
  | RegFileRead OutputWidth RegFileId
    -- |Register file update (inputs: write-enable, address, data)
  | RegFileWrite InputWidth InputWidth RegFileId
  deriving Show

-- |A constant bit is a 0, 1, or don't care
data ConstBit = Zero | One | DontCare
  deriving (Eq, Show)

-- |For the Display primitive:
-- display a string literal or a bit-vector value of a given width
data DisplayArg =
    DisplayArgString String
  | DisplayArgBit InputWidth

instance Show DisplayArg where
  show (DisplayArgString s) = show s
  show (DisplayArgBit w) = show w

-- |Custom components may have compile-time parameters.
-- A parameter has a name and a value, both represented as strings
data Param = String :-> String deriving Show

-- |Given a parameter name, return the parameter value
lookupParam :: [Param] -> String -> String
lookupParam ps p = case [v | (k :-> v) <- ps, p == k] of
                     [] -> error ("Unrecognised parameter '" ++ p ++ "'")
                     v:vs -> v

-- | An untyped bit vector output wire from a primitive component instance
data BV = 
  BV {
    -- |What kind of primitive produced this bit vector?
    bvPrim :: Prim
    -- |Unique id of primitive instance
  , bvInstRef :: IORef (Maybe InstId)
    -- |Inputs to the primitive instance
  , bvInputs :: [BV]
    -- |Output pin number
  , bvOutNum :: OutputNumber
    -- |Width of this output pin
  , bvWidth :: Width
    -- |Width of each output pin
  , bvWidths :: [Width]
  }

{-# NOINLINE makePrim #-}
-- |Helper function for creating an instance of a primitive component
makePrim :: Prim -> [BV] -> [Int] -> [BV]
makePrim prim ins outWidths =
  [ BV {
        bvPrim    = prim
      , bvInstRef = ref
      , bvInputs  = ins
      , bvOutNum  = i
      , bvWidth   = w
      , bvWidths  = outWidths
    }
  | (i, w) <- zip [0..] outWidths ]
  where
    {-# NOINLINE ref #-}
    ref = newRef Nothing

{-# NOINLINE newRef #-}
-- |For Observable Sharing.
newRef :: Maybe InstId -> IORef (Maybe InstId)
newRef x = unsafePerformIO (newIORef x)

-- |Create instance of primitive component which has one output
makePrim1 :: Prim -> [BV] -> Int -> BV
makePrim1 prim ins width = head (makePrim prim ins [width])

-- |Netlists are lists of nets
data Net =
  Net {
      netPrim         :: Prim
    , netInstId       :: InstId
    , netInputs       :: [WireId]
    , netOutputWidths :: [Width]
  } deriving Show

-- |A wire is uniquely identified by an instance id and an output number
type WireId = (InstId, OutputNumber)

-- |A reader/writer monad for accumulating the netlist
newtype Flatten a = Flatten { runFlatten :: FlattenR -> IO (FlattenW, a) }

-- |The reader component contains an IORef containing the next unique net id
type FlattenR = IORef Int

-- |The writer component is the netlist
type FlattenW = JL.JList Net

instance Monad Flatten where
  return a = Flatten (\r -> return (JL.Zero, a))
  m >>= f  = Flatten (\r -> do (w0, a) <- runFlatten m r
                               (w1, b) <- runFlatten (f a) r
                               return (w0 JL.:+: w1, b))

instance Applicative Flatten where
  pure = return
  (<*>) = ap

instance Functor Flatten where
  fmap = liftM

-- |Obtain a fresh instance id
freshId :: Flatten Int
freshId = Flatten $ \r -> do
  id <- readIORef r
  writeIORef r (id+1)
  return (JL.Zero, id)

-- |Add a net to the netlist
addNet :: Net -> Flatten ()
addNet net = Flatten $ \r -> return (JL.One net, ())

-- |Lift an IO computation to a Flatten computation
doIO :: IO a -> Flatten a
doIO m = Flatten $ \r -> do
  a <- m
  return (JL.Zero, a)

-- |Flatten bit vector to netlist
flatten :: BV -> Flatten WireId
flatten b =
  do val <- doIO (readIORef (bvInstRef b))
     case val of
       Nothing -> do
         id <- freshId
         doIO (writeIORef (bvInstRef b) (Just id))
         ins <- mapM flatten (bvInputs b)
         let net = Net { netPrim         = bvPrim b
                       , netInstId       = id
                       , netInputs       = ins
                       , netOutputWidths = (bvWidths b)
                       }
         addNet net
         return ((id, bvOutNum b))
       Just id -> return ((id, bvOutNum b))

-- |Constant bit vector of given width
constBV :: Width -> Integer -> BV
constBV w i = makePrim1 (Const w i) [] w

-- |Bit vector of constant bits
constBitsBV :: Width -> ConstBit -> BV
constBitsBV w b = makePrim1 (ConstBits w b) [] w

-- |Adder
addBV :: BV -> BV -> BV
addBV a b = makePrim1 (Add w) [a, b] w
  where w = bvWidth a

-- |Subtractor
subBV :: BV -> BV -> BV
subBV a b = makePrim1 (Sub w) [a, b] w
  where w = bvWidth a

-- |Multiplier
mulBV :: BV -> BV -> BV
mulBV a b = makePrim1 (Mul w) [a, b] w
  where w = bvWidth a

-- |Quotient
divBV :: BV -> BV -> BV
divBV a b = makePrim1 (Div w) [a, b] w
  where w = bvWidth a

-- |Remainder
modBV :: BV -> BV -> BV
modBV a b = makePrim1 (Mod w) [a, b] w
  where w = bvWidth a

-- |Bitwise not
invBV :: BV -> BV
invBV a = makePrim1 (Not w) [a] w
  where w = bvWidth a

-- |Bitwise and
andBV :: BV -> BV -> BV
andBV a b = makePrim1 (And w) [a, b] w
  where w = bvWidth a

-- |Bitwise or
orBV :: BV -> BV -> BV
orBV a b = makePrim1 (Or w) [a, b] w
  where w = bvWidth a

-- |Bitwise xor
xorBV :: BV -> BV -> BV
xorBV a b = makePrim1 (Xor w) [a, b] w
  where w = bvWidth a

-- |Left shift
leftBV :: BV -> BV -> BV
leftBV a b = makePrim1 (ShiftLeft w) [a, b] w
  where w = bvWidth a

-- |Right shift
rightBV :: BV -> BV -> BV
rightBV a b = makePrim1 (ShiftRight w) [a, b] w
  where w = bvWidth a

-- |Equality comparator
equalBV :: BV -> BV -> BV
equalBV a b = makePrim1 (Equal w) [a, b] 1
  where w = bvWidth a

-- |Disequality comparator
notEqualBV :: BV -> BV -> BV
notEqualBV a b = makePrim1 (NotEqual w) [a, b] 1
  where w = bvWidth a

-- |Less-than comparator
lessThanBV :: BV -> BV -> BV
lessThanBV a b = makePrim1 (LessThan w) [a, b] 1
  where w = bvWidth a

-- |Less-than-or-equal comparator
lessThanEqBV :: BV -> BV -> BV
lessThanEqBV a b = makePrim1 (LessThanEq w) [a, b] 1
  where w = bvWidth a

-- |Replicate a bit
replicateBV :: Int -> BV -> BV
replicateBV w a = makePrim1 (ReplicateBit w) [a] w

-- |Zero-extend a bit vector
zeroExtendBV :: OutputWidth -> BV -> BV
zeroExtendBV ow a = makePrim1 (ZeroExtend iw ow) [a] ow
  where iw = bvWidth a

-- |Sign-extend a bit vector
signExtendBV :: OutputWidth -> BV -> BV
signExtendBV ow a = makePrim1 (SignExtend iw ow) [a] ow
  where iw = bvWidth a

-- |Bit selection
selectBV :: (BitIndex, BitIndex) -> BV -> BV
selectBV (upper, lower) a =
    makePrim1 (SelectBits w upper lower) [a] (upper+1-lower)
  where w = bvWidth a

-- |Bit vector concatenation
concatBV :: BV -> BV -> BV
concatBV a b = makePrim1 (Concat wa wb) [a, b] (wa+wb)
  where
    wa = bvWidth a
    wb = bvWidth b

-- |Multiplexer
muxBV :: BV -> (BV, BV) -> BV
muxBV sel (a, b) = makePrim1 (Mux w) [sel, a, b] w
  where w = bvWidth a

-- |Population count
countOnesBV :: OutputWidth -> BV -> BV
countOnesBV w a = makePrim1 (CountOnes w) [a] w

-- |Identity function
idBV :: BV -> BV
idBV a = makePrim1 (Identity w) [a] w
  where w = bvWidth a

-- |Input pin (named Verilog pin)
inputPinBV :: Width -> String -> BV
inputPinBV w s = makePrim1 (Input w s) [] w

-- |Register of given width with initial value 
regBV :: Width -> BV -> BV -> BV
regBV w i a = makePrim1 (Register (getInitBV i) w) [a] w

-- |Register of given width with initial value and enable wire
regEnBV :: Width -> BV -> BV -> BV -> BV
regEnBV w i en a = makePrim1 (RegisterEn (getInitBV i) w) [en, a] w

-- |Single-port block RAM.
-- Input: one triple (address, data, write-enable)
ramBV :: OutputWidth -> Maybe String -> (BV, BV, BV) -> BV
ramBV dw initFile (addr, dataIn, weIn) =
    makePrim1 prim [addr, dataIn, weIn] dw
  where
    prim = BRAM { ramInitFile  = initFile
                , ramAddrWidth = bvWidth addr
                , ramDataWidth = dw }

-- |True dual-port block RAM.
-- Input: two triples (address, data, write-enable)
dualRamBV :: OutputWidth -> Maybe String
          -> (BV, BV, BV)
          -> (BV, BV, BV)
          -> (BV, BV)
dualRamBV dw initFile (addrA, dataInA, weInA) (addrB, dataInB, weInB) =
    (outs !! 0, outs !! 1)
  where
    outs = makePrim prim [addrA, dataInA, weInA, addrB, dataInB, weInB] [dw,dw]
    prim = TrueDualBRAM { ramInitFile  = initFile
                        , ramAddrWidth = bvWidth addrA
                        , ramDataWidth = dw }

-- |Read from register file
regFileReadBV :: RegFileId -> OutputWidth -> BV -> BV
regFileReadBV id dw a = makePrim1 (RegFileRead dw id) [a] dw

-- |Get the value of a constant bit vector,
-- which may involve bit manipulations.
-- Used to determine the initial value of a register.
getInitBV :: BV -> Integer
getInitBV = eval
  where
    eval a = 
      case bvPrim a of
        Const _ i -> i
        ConstBits w One -> (2^w) - 1
        ConstBits w Zero -> 0
        ConstBits w DontCare -> 0  -- TODO: do better here
        Concat wx wy ->
          let x = eval (bvInputs a !! 0)
              y = eval (bvInputs a !! 1)
          in  x `B.shiftL` wy + y
        SelectBits w hi lo ->
          let x = eval (bvInputs a !! 0)
              mask = (1 `B.shiftL` hi) - 1
          in  (x B..&. mask) `B.shiftR` lo
        ReplicateBit w ->
          let b = eval (bvInputs a !! 0)
          in  b * ((2^w) - 1)
        other -> error $ "Register initialiser must be a constant: " ++
                         (show other)
