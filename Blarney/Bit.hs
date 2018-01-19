-- For Observable Sharing
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

-- For type-level naturals
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies, GADTs #-}

module Blarney.Bit
  ( Bit        -- "Bit n" is a bit vector of n bits
  , (.&.)      -- Bitwise and
  , (.|.)      -- Bitwise or
  , (.^.)      -- Bitwise xor
  , (?)        -- Mux
  , (.==.)     -- Equality
  , (.!=.)     -- Disequality
  , (.<.)      -- Less than
  , (.>.)      -- Greater than
  , (.<=.)     -- Less than or equal
  , (.>=.)     -- Greater than or equal
  , shl        -- Shift left
  , shr        -- Shift right
  , reg        -- Register
  , regEn      -- Register with enable
  , (#)        -- Bit concatenation
  , bit        -- Bit selection
  , (!)        -- Bit range selection
  , zeroExtend -- Zero extend
  , signExtend -- Sign extend
  , upper      -- Extract most significant bits
  , lower      -- Extract least significant bits
  ) where

-- For type-level literals
import GHC.TypeLits

-- For type-level indices
import Blarney.TypeIndex

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

-- Determine bit width from type
widthOf :: KnownNat n => Bit n -> Int
widthOf a = fromInteger (natVal a)

-- Determine bit width from pin
width :: Bit n -> Int
width (Bit pin) = pinWidth pin

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
primArith1 :: PrimName -> [Param] -> Bit n -> Bit n
primArith1 prim params a = Bit (primInst1 prim params [unBit a] (width a))

-- Binary arithmetic primitive
primArith2 :: PrimName -> [Param] -> Bit n -> Bit n -> Bit n
primArith2 prim params a b =
  Bit (primInst1 prim params [unBit a, unBit b] (width a))

-- Bitwise invert
inv :: Bit n -> Bit n
inv = primArith1 "~" []

-- Bitwise and
infixl 7 .&.
(.&.) :: Bit n -> Bit n -> Bit n
(.&.) = primArith2 "&" []

-- Bitwise or
infixl 5 .|.
(.|.) :: Bit n -> Bit n -> Bit n
(.|.) = primArith2 "|" []

-- Bitwise xor
infixl 6 .^.
(.^.) :: Bit n -> Bit n -> Bit n
(.^.) = primArith2 "^" []

-- Mux
(?) :: Bit 1 -> (Bit n, Bit n) -> Bit n
sel ? (a, b) =
  Bit (primInst1 "?" [] [unBit sel, unBit a, unBit b] (width a))

-- Binary comparison helper
primCmp2 :: PrimName -> [Param] -> Bit n -> Bit n -> Bit 1
primCmp2 prim params a b =
  Bit (primInst1 prim params [unBit a, unBit b] 1)

-- Equality
(.==.) :: Bit n -> Bit n -> Bit 1
(.==.) = primCmp2 "==" []

-- Disequality
(.!=.) :: Bit n -> Bit n -> Bit 1
(.!=.) = primCmp2 "!=" []

-- Less than
(.<.) :: Bit n -> Bit n -> Bit 1
(.<.) = primCmp2 "<" []

-- Less than or equal
(.<=.) :: Bit n -> Bit n -> Bit 1
(.<=.) = primCmp2 "<=" []

-- Greater than
(.>.) :: Bit n -> Bit n -> Bit 1
(.>.) = primCmp2 ">" []

-- Greater than or equal
(.>=.) :: Bit n -> Bit n -> Bit 1
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
     w          = widthOf result

-- Shift left
shl :: n ~ (2^m) => Bit n -> Bit m -> Bit n
shl x y = Bit (primInst1 "shl" [] [] (width x))

-- Shift right
shr :: n ~ (2^m) => Bit n -> Bit m -> Bit n
shr x y = Bit (primInst1 "shr" [] [] (width x))

-- Register
reg :: Integer -> Bit n -> Bit n
reg init = primArith1 "reg" ["init" :-> show init]

-- Register with enable
regEn :: Integer -> Bit 1 -> Bit n -> Bit n
regEn init en a = 
  Bit (primInst1 "regEn" ["init" :-> show init]
        [unBit en, unBit a] (width a))

-- Concatenation
(#) :: Bit n -> Bit m -> Bit (n+m)
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

-- Zero extend
zeroExtend :: (KnownNat m, n <= m) => Bit n -> Bit m
zeroExtend a = result
   where
     params     = ["ext" :-> show (w - width a)]
     result     = Bit (primInst1 "zeroExtend" params [unBit a] w)
     w          = widthOf result

-- Sign extend
signExtend :: (KnownNat m, n <= m) => Bit n -> Bit m
signExtend a = result
   where
     params     = ["ext" :-> show (w - width a),
                   "msb" :-> show (width a - 1)]
     result     = Bit (primInst1 "signExtend" params [unBit a] w)
     w          = widthOf result

-- Extract most significant bits
upper :: (KnownNat m, m <= n) => Bit n -> Bit m
upper a = result
   where
     params     = ["hi" :-> show (width a - 1),
                   "lo" :-> show (width a - w)]
     result     = Bit (primInst1 "range" params [unBit a] w)
     w          = widthOf result

-- Extract least significant bits
lower :: (KnownNat m, m <= n) => Bit n -> Bit m
lower a = result
   where
     params     = ["hi" :-> show (w - 1),
                   "lo" :-> "0"]
     result     = Bit (primInst1 "range" params [unBit a] w)
     w          = widthOf result
