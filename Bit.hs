-- For Observable Sharing
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

-- For type-level naturals
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies, GADTs #-}

module Blarney.Bit
  ( Bit      -- "Bit n" is a bit vector of n bits
  , (.&.)    -- Bitwise and
  , (.|.)    -- Bitwise or
  , (.^.)    -- Bitwise xor
  , (?)      -- Mux
  , (.==.)   -- Equality
  , (.!=.)   -- Disequality
  , (.<.)    -- Less than
  , (.>.)    -- Greater than
  , (.<=.)   -- Less than or equal
  , (.>=.)   -- Greater than or equal
  , reg      -- Register
  , regEn    -- Register with enable
  , (#)      -- Concatenation
  , bit      -- Bit selection
  , (!)      -- Sub-range selection
  -- TODO: div, mod, shift left, shift right,
  -- upper, lower, zeroExtend, signExtend
  ) where

-- For type-level literals
import GHC.TypeLits

-- For type-level indices
import TypeIndex

-- For Observable Sharing
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

-- Every instance of a component in the circuit has a unique id
type InstId = Int

-- Primitive component name
type PrimName = String

-- Primitive components may have compile-time parameters
-- A parameter has a name and a value, both represented as strings
data Param = String :-> String deriving Show

-- An output pin from a primitive component instance
data Pin = 
  Pin {
    -- What kind of primitive produced this pin?
    pinPrim :: PrimName
    -- Compile-time parameters
  , pinParams :: [Param]
    -- Unique id of primitive instance that produced it
  , pinInstRef :: IORef (Maybe InstId)
    -- Inputs to the primitive instance
  , pinInputs :: [Pin]
    -- Output pin number
  , pinOutNum :: Int
    -- Bit width of pin
  , pinWidth :: Int
  }

-- Phantom type for a pin, capturing the bit width
newtype Bit (n :: Nat) = Bit { unBit :: Pin }

-- Determine bit width
width :: KnownNat n => Bit n -> Int
width a = fromInteger (natVal a)

-- Helper function for creating instance of a primitive component
{-# NOINLINE primInst #-}
primInst :: PrimName -> [Param] -> [Pin] -> [Int] -> [Pin]
primInst prim params ins outWidths = map outPin (zip [0..] outWidths)
  where
    outPin (i, w) = Pin {
                        pinPrim    = prim
                      , pinParams  = params
                      , pinInstRef = ref
                      , pinInputs  = ins
                      , pinOutNum  = i
                      , pinWidth   = w
                    }

    {-# NOINLINE ref #-}
    ref = newRef Nothing

-- Use of unsafePerformIO to implement Observable Sharing
{-# NOINLINE newRef #-}
newRef :: Maybe InstId -> IORef (Maybe InstId)
newRef x = unsafePerformIO (newIORef x)

-- Create instance of primitive component which has one output
primInst1 :: PrimName -> [Param] -> [Pin] -> Int -> Pin
primInst1 prim params ins outWidth =
  head (primInst prim params ins [outWidth])

-- Unary arithmetic primitive
primArith1 :: KnownNat n => PrimName -> [Param] -> Bit n -> Bit n
primArith1 prim params a = Bit (primInst1 prim params [unBit a] (width a))

-- Binary arithmetic primitive
primArith2 :: KnownNat n => PrimName -> [Param] -> Bit n -> Bit n -> Bit n
primArith2 prim params a b =
  Bit (primInst1 prim params [unBit a, unBit b] (width a))

-- Bitwise invert
inv :: KnownNat n => Bit n -> Bit n
inv = primArith1 "~" []

-- Bitwise and
infixl 7 .&.
(.&.) :: KnownNat n => Bit n -> Bit n -> Bit n
(.&.) = primArith2 "&" []

-- Bitwise or
infixl 5 .|.
(.|.) :: KnownNat n => Bit n -> Bit n -> Bit n
(.|.) = primArith2 "|" []

-- Bitwise xor
infixl 6 .^.
(.^.) :: KnownNat n => Bit n -> Bit n -> Bit n
(.^.) = primArith2 "^" []

-- Mux
(?) :: KnownNat n => Bit 1 -> (Bit n, Bit n) -> Bit n
sel ? (a, b) =
  Bit (primInst1 "?" [] [unBit sel, unBit a, unBit b] (width a))

-- Binary comparison helper
primCmp2 :: KnownNat n => PrimName -> [Param] -> Bit n -> Bit n -> Bit 1
primCmp2 prim params a b =
  Bit (primInst1 prim params [unBit a, unBit b] 1)

-- Equality
(.==.) :: KnownNat n => Bit n -> Bit n -> Bit 1
(.==.) = primCmp2 "==" []

-- Disequality
(.!=.) :: KnownNat n => Bit n -> Bit n -> Bit 1
(.!=.) = primCmp2 "!=" []

-- Less than
(.<.) :: KnownNat n => Bit n -> Bit n -> Bit 1
(.<.) = primCmp2 "<" []

-- Less than or equal
(.<=.) :: KnownNat n => Bit n -> Bit n -> Bit 1
(.<=.) = primCmp2 "<=" []

-- Greater than
(.>.) :: KnownNat n => Bit n -> Bit n -> Bit 1
(.>.) = primCmp2 ">" []

-- Greater than or equal
(.>=.) :: KnownNat n => Bit n -> Bit n -> Bit 1
(.>=.) = primCmp2 ">=" []

-- Arithmetic
instance KnownNat n => Num (Bit n) where
  (+)           = primArith2 "+" []
  (-)           = primArith2 "-" []
  (*)           = primArith2 "*" []
  negate        = primArith1 "-" []
  abs           = id
  signum a      = (a .==. 0) ? (0, 1)
  fromInteger i = result
   where
     result     = Bit (primInst1 "const" ["val" :-> show i] [] w)
     w          = width result

-- Register
reg :: KnownNat n => Integer -> Bit n -> Bit n
reg init = primArith1 "reg" ["init" :-> show init]

-- Register with enable
regEn :: KnownNat n => Integer -> Bit 1 -> Bit n -> Bit n
regEn init en a = 
  Bit (primInst1 "regEn" ["init" :-> show init]
        [unBit en, unBit a] (width a))

-- Concatenation
(#) :: (KnownNat n, KnownNat m) => Bit n -> Bit m -> Bit (n+m)
a # b = Bit (primInst1 "#" [] [unBit a, unBit b] (width a + width b))

-- Bit selection
bit :: (KnownNat i, (i+1) <= n) => I i -> Bit n -> Bit 1
bit i a = 
  Bit (primInst1 "bit" ["index" :-> show (natVal i)] [unBit a] 1)

-- Sub-range selection
(!) :: (KnownNat hi, KnownNat lo, (hi+1) <= n, lo <= hi, (lo+m)~hi) =>
       Bit n -> (I hi, I lo) -> Bit m
a ! (hi, lo) =
  Bit (primInst1 "range" ["high" :-> show (natVal hi),
                          "low"  :-> show (natVal lo)]
      [unBit a] (fromInteger (natVal hi - natVal lo)))
