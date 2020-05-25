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
, makePrim0      -- Common case: no-output components
, makePrim1      -- Common case: single-output components

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
, idBV           -- :: BV -> BV
, testPlusArgsBV -- :: String -> BV
, inputPinBV     -- :: String -> BV
, regBV          -- :: Width -> Integer -> BV -> BV
, regEnBV        -- :: Width -> Integer -> BV -> BV -> BV
, ramBV          -- :: OutputWidth -> Maybe String -> (BV, BV, BV) -> BV
, dualRamBV      -- :: OutputWidth -> Maybe String
, regFileReadBV  -- :: RegFileInfo -> BV -> BV
, getInitBV      -- :: BV -> Integer

-- * Other misc helpers
, bvPrimOutWidth -- get the 'OutputWidth' of the 'Prim' of a 'BV'
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
               -- | Output
             , bvOutput    :: OutputName
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
-- | Helper function for creating an instance of a primitive component
makePrim :: Prim -> [BV] -> [OutputName] -> [BV]
makePrim prim ins outs
  | Prelude.null outs = [bv]
  | otherwise = [ bv { bvOutput = o } | o <- outs ]
  where bv = BV { bvPrim      = prim
                , bvInputs    = ins
                , bvOutput    = Nothing
                , bvNameHints = empty
                , bvInstRef   = instIdRef
                }
        -- | For Observable Sharing.
        instIdRef = unsafePerformIO (newIORef Nothing)

-- | helper to get the 'OutputWidth' of the 'Prim' of a 'BV'
bvPrimOutWidth :: BV -> OutputWidth
bvPrimOutWidth BV{bvPrim=prim,bvOutput=out} = primOutWidth prim out

-- | Create instance of primitive component with no output
makePrim0 :: Prim -> [BV] -> BV
makePrim0 prim ins = head $ makePrim prim ins []

-- | Create instance of primitive component which has one output
makePrim1 :: Prim -> [BV] -> BV
makePrim1 prim ins = head $ makePrim prim ins [Nothing]

-- |Constant bit vector of given width
constBV :: Width -> Integer -> BV
constBV w i = makePrim1 (Const w i) []

-- |Don't care of given width
dontCareBV :: Width -> BV
dontCareBV w = makePrim1 (DontCare w) []

-- | Adder
addBV :: BV -> BV -> BV
addBV a b = makePrim1 (Add w) [a, b]
  where w = bvPrimOutWidth a

-- |Subtractor
subBV :: BV -> BV -> BV
subBV a b = makePrim1 (Sub w) [a, b]
  where w = bvPrimOutWidth a

-- |Multiplier
mulBV :: BV -> BV -> BV
mulBV a b = makePrim1 (Mul w) [a, b]
  where w = bvPrimOutWidth a

-- |Quotient
divBV :: BV -> BV -> BV
divBV a b = makePrim1 (Div w) [a, b]
  where w = bvPrimOutWidth a

-- |Remainder
modBV :: BV -> BV -> BV
modBV a b = makePrim1 (Mod w) [a, b]
  where w = bvPrimOutWidth a

-- |Bitwise not
invBV :: BV -> BV
invBV a = makePrim1 (Not w) [a]
  where w = bvPrimOutWidth a

-- |Bitwise and
andBV :: BV -> BV -> BV
andBV a b = makePrim1 (And w) [a, b]
  where w = bvPrimOutWidth a

-- |Bitwise or
orBV :: BV -> BV -> BV
orBV a b = makePrim1 (Or w) [a, b]
  where w = bvPrimOutWidth a

-- |Bitwise xor
xorBV :: BV -> BV -> BV
xorBV a b = makePrim1 (Xor w) [a, b]
  where w = bvPrimOutWidth a

-- |Left shift
leftBV :: BV -> BV -> BV
leftBV a b = makePrim1 (ShiftLeft iw ow) [a, b]
  where ow = bvPrimOutWidth a
        iw = bvPrimOutWidth b

-- |Right shift
rightBV :: BV -> BV -> BV
rightBV a b = makePrim1 (ShiftRight iw ow) [a, b]
  where ow = bvPrimOutWidth a
        iw = bvPrimOutWidth b

-- |Right shift
arithRightBV :: BV -> BV -> BV
arithRightBV a b = makePrim1 (ArithShiftRight iw ow) [a, b]
  where ow = bvPrimOutWidth a
        iw = bvPrimOutWidth b

-- |Equality comparator
equalBV :: BV -> BV -> BV
equalBV a b = makePrim1 (Equal w) [a, b]
  where w = bvPrimOutWidth a

-- |Disequality comparator
notEqualBV :: BV -> BV -> BV
notEqualBV a b = makePrim1 (NotEqual w) [a, b]
  where w = bvPrimOutWidth a

-- |Less-than comparator
lessThanBV :: BV -> BV -> BV
lessThanBV a b = makePrim1 (LessThan w) [a, b]
  where w = bvPrimOutWidth a

-- |Less-than-or-equal comparator
lessThanEqBV :: BV -> BV -> BV
lessThanEqBV a b = makePrim1 (LessThanEq w) [a, b]
  where w = bvPrimOutWidth a

-- |Replicate a bit
replicateBV :: Int -> BV -> BV
replicateBV w a = makePrim1 (ReplicateBit w) [a]

-- |Zero-extend a bit vector
zeroExtendBV :: OutputWidth -> BV -> BV
zeroExtendBV ow a = makePrim1 (ZeroExtend iw ow) [a]
  where iw = bvPrimOutWidth a

-- |Sign-extend a bit vector
signExtendBV :: OutputWidth -> BV -> BV
signExtendBV ow a = makePrim1 (SignExtend iw ow) [a]
  where iw = bvPrimOutWidth a

-- |Bit selection
selectBV :: (BitIndex, BitIndex) -> BV -> BV
selectBV (upper, lower) a =
    makePrim1 (SelectBits w upper lower) [a]
  where w = bvPrimOutWidth a

-- |Bit vector concatenation
concatBV :: BV -> BV -> BV
concatBV a b = makePrim1 (Concat wa wb) [a, b]
  where
    wa = bvPrimOutWidth a
    wb = bvPrimOutWidth b

-- |Multiplexer
muxBV :: BV -> [BV] -> BV
muxBV sel xs = makePrim1 (Mux n w) (sel:xs)
               where n = length xs
                     w = bvPrimOutWidth $ head xs

-- |Identity function
idBV :: BV -> BV
idBV a = makePrim1 (Identity w) [a]
  where w = bvPrimOutWidth a

-- |Test plusargs
testPlusArgsBV :: String -> BV
testPlusArgsBV str = makePrim1 (TestPlusArgs str) []

-- |Input pin (named Verilog pin)
inputPinBV :: Width -> String -> BV
inputPinBV w s = makePrim1 (Input w s) []

-- |Register of given width with initial value
regBV :: Width -> BV -> BV -> BV
regBV w i a = makePrim1 (Register (getInitBV i) w) [a]

-- |Register of given width with initial value and enable wire
regEnBV :: Width -> BV -> BV -> BV -> BV
regEnBV w i en a = makePrim1 (RegisterEn (getInitBV i) w) [en, a]

-- |Single-port block RAM.
-- Input: one triple (address, data, write-enable)
ramBV :: OutputWidth -> Maybe String -> (BV, BV, BV) -> BV
ramBV dw initFile (addr, dataIn, weIn) =
    makePrim1 prim [addr, dataIn, weIn]
  where
    prim = BRAM { ramInitFile  = initFile
                , ramAddrWidth = bvPrimOutWidth addr
                , ramDataWidth = dw }

-- |True dual-port block RAM.
-- Input: two triples (address, data, write-enable)
dualRamBV :: OutputWidth -> Maybe String
          -> (BV, BV, BV)
          -> (BV, BV, BV)
          -> (BV, BV)
dualRamBV dw initFile (addrA, dataInA, weInA) (addrB, dataInB, weInB) =
    (bvOuts !! 0, bvOuts !! 1)
  where
    outs = [Just "dataA", Just "dataB"]
    bvOuts = makePrim prim [addrA, dataInA, weInA, addrB, dataInB, weInB] outs
    prim = TrueDualBRAM { ramInitFile  = initFile
                        , ramAddrWidth = bvPrimOutWidth addrA
                        , ramDataWidth = dw }

-- | Read from register file
regFileReadBV :: RegFileInfo -> BV -> BV
regFileReadBV inf a = makePrim1 (RegFileRead inf) [a]

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
              mask = (1 `B.shiftL` (hi+1)) - 1
          in  (x B..&. mask) `B.shiftR` lo
        ReplicateBit w ->
          let b = eval (bvInputs a !! 0)
          in  b * ((2^w) - 1)
        other -> error $ "Register initialiser must be a constant: " ++
                         (show other)
