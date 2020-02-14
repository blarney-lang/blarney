-- For Observable Sharing
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

{-|
Module      : Blarney.Core.BV
Description : Untyped bit vectors and circuit primitives
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

This module represents the core of the Blarney HDL, upon which all
hardware description features are built.  It provides untyped bit
vectors, i.e. bit vectors whose width is not specified in the type.
Hardware developers should not use these untyped primitives directly
(unless they know what they are doing), because width-mistmatches are
not checked.  Any bit-vector can be evaluated symbolically, using a
feature similar to Observable Sharing [1], to produce a netlist,
i.e. a graph whose nodes are primitive component instances and whose
edges are connections.

1. K. Claessen, D. Sands.  Observable Sharing for Functional Circuit
Description, ASIAN 1999.
-}

module Blarney.Core.BV (
  -- * Untyped bit vectors
  BV(..)         -- Untyped bit vector
, makePrim       -- Create instance of primitive component
, makePrim1      -- Common case: single-output components
, makePrimRoot   -- Instance of primitive component that is a root

  -- * Bit-vector primitives
, constBV        -- :: Width -> Integer -> BV
, dontCareBV     -- :: Width -> BV
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
, arithRightBV   -- :: BV -> BV -> BV
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
, testPlusArgsBV -- :: String -> BV
, inputPinBV     -- :: String -> BV
, regBV          -- :: Width -> Integer -> BV -> BV
, regEnBV        -- :: Width -> Integer -> BV -> BV -> BV
, ramBV          -- :: OutputWidth -> Maybe String -> (BV, BV, BV) -> BV
, dualRamBV      -- :: OutputWidth -> Maybe String
, regFileReadBV  -- :: RegFileId -> OutputWidth -> BV -> BV
, getInitBV      -- :: BV -> Integer

-- * Other misc helpers
, lookupParam    -- Given a parameter name, return the parameter value
, addBVNameHint  -- Add a name hint to a 'BV'
, addBVNameHints -- Add name hints to a 'BV'
) where

import Prelude

-- For Observable Sharing
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

-- For name hints
import Data.Set (insert, empty)

-- Standard imports
import qualified Data.Bits as B

-- Blarney imports
import Blarney.Core.Prim

-- |Given a parameter name, return the parameter value
lookupParam :: [Param] -> String -> String
lookupParam ps p = case [v | (k :-> v) <- ps, p == k] of
                     [] -> error ("Unrecognised parameter '" ++ p ++ "'")
                     v:vs -> v

-- | An untyped bit vector output wire from a primitive component instance
data BV = BV {
               -- | What kind of primitive produced this bit vector?
               bvPrim      :: Prim
               -- | Inputs to the primitive instance
             , bvInputs    :: [BV]
               -- | Output pin number
             , bvOutNum    :: OutputNumber
               -- | Width of this output pin
             , bvWidth     :: Width
               -- | Width of each output pin
             , bvWidths    :: [Width]
               -- | Name (hints) characterising the 'BV'
             , bvNameHints :: NameHints
               -- | Unique id of primitive instance
             , bvInstRef   :: IORef (Maybe InstId)
             }

-- | Add a name hint to the 'BV'
addBVNameHint :: BV -> NameHint -> BV
addBVNameHint bv@BV{ bvNameHints = hints } hint =
  bv { bvNameHints = insert hint hints }

-- | Add name hints to the 'BV'
addBVNameHints :: BV -> NameHints -> BV
addBVNameHints bv@BV{ bvNameHints = hints } newHints =
  bv { bvNameHints = hints <> newHints }

{-# NOINLINE makePrim #-}
-- |Helper function for creating an instance of a primitive component
makePrim :: Prim -> [BV] -> [Int] -> [BV]
makePrim prim ins outWidths
  | Prelude.null outWidths = [bv]
  | otherwise = [ bv { bvOutNum = i, bvWidth = w }
                | (i, w) <- zip [0..] outWidths ]
  where bv = BV { bvPrim      = prim
                , bvInputs    = ins
                , bvOutNum    = 0
                , bvWidth     = 0
                , bvWidths    = outWidths
                , bvNameHints = empty
                , bvInstRef   = instIdRef
                }
        -- |For Observable Sharing.
        instIdRef = unsafePerformIO (newIORef Nothing)

-- |Create instance of primitive component which has one output
makePrim1 :: Prim -> [BV] -> Int -> BV
makePrim1 prim ins width = head (makePrim prim ins [width])

-- |Create instance of primitive component which is a root
makePrimRoot :: Prim -> [BV] -> BV
makePrimRoot prim ins = head (makePrim prim ins [])

-- |Constant bit vector of given width
constBV :: Width -> Integer -> BV
constBV w i = makePrim1 (Const w i) [] w

-- |Don't care of given width
dontCareBV :: Width -> BV
dontCareBV w = makePrim1 (DontCare w) [] w

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

-- |Right shift
arithRightBV :: BV -> BV -> BV
arithRightBV a b = makePrim1 (ArithShiftRight w) [a, b] w
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

-- |Test plusargs
testPlusArgsBV :: String -> BV
testPlusArgsBV str = makePrim1 (TestPlusArgs str) [] 1

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
        DontCare w -> 0  -- TODO: do better here
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
