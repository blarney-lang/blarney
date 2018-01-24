-- For type-level naturals
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies #-}

module Blarney.Bit
  ( Bit(..)      -- "Bit n" is a bit vector of n bits
  , replicateBit -- Replicate bit to produce bit vector
  , low          -- Bit vector of all 0's
  , high         -- Bit vector of all 1's
  , inv          -- Bitwise invert
  , (.&.)        -- Bitwise and
  , (.|.)        -- Bitwise or
  , (.^.)        -- Bitwise xor
  , (?)          -- Mux
  , (.==.)       -- Equality
  , (.!=.)       -- Disequality
  , (.<.)        -- Less than
  , (.>.)        -- Greater than
  , (.<=.)       -- Less than or equal
  , (.>=.)       -- Greater than or equal
  , (.+.)        -- Add
  , (.-.)        -- Subtract
  , (.*.)        -- Multiply
  , (.<<.)       -- Shift left
  , (.>>.)       -- Shift right
  , reg          -- Register
  , regEn        -- Register with enable
  , (#)          -- Bit concatenation
  , bit          -- Bit selection
  , bits         -- Bit range selection
  , zeroExtend   -- Zero extend
  , signExtend   -- Sign extend
  , upper        -- Extract most significant bits
  , lower        -- Extract least significant bits
  ) where

import Blarney.Unbit
import GHC.TypeLits

-- Phantom type wrapping an untyped bit vector, capturing the bit width
newtype Bit (n :: Nat) = Bit { unbit :: Unbit }

-- Determine bit width from type
widthOf :: KnownNat n => Bit n -> Int
widthOf a = fromInteger (natVal a)

-- Determine bit width
width :: Bit n -> Int
width (Bit b) = unbitWidth b

-- Unary arithmetic primitive
primArith1 :: PrimName -> [Param] -> Bit n -> Bit n
primArith1 prim params a = Bit (primInst1 prim params [unbit a] (width a))

-- Binary arithmetic primitive
primArith2 :: PrimName -> [Param] -> Bit n -> Bit n -> Bit n
primArith2 prim params a b =
  Bit (primInst1 prim params [unbit a, unbit b] (width a))

-- Replicate bit
replicateBit :: KnownNat n => Bit 1 -> Bit n
replicateBit a = result
  where result = Bit (primInst1 "replicate" [] [unbit a] (widthOf result))

-- All 0's
low :: KnownNat n => Bit n
low = replicateBit 0

-- All 1's
high :: KnownNat n => Bit n
high = replicateBit 1

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
  Bit (primInst1 "?" [] [unbit sel, unbit a, unbit b] (width a))

-- Binary comparison helper
primCmp2 :: PrimName -> [Param] -> Bit n -> Bit n -> Bit 1
primCmp2 prim params a b =
  Bit (primInst1 prim params [unbit a, unbit b] 1)

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

-- Add
infixl 6 .+.
(.+.) :: Bit n -> Bit n -> Bit n
(.+.) = primArith2 "+" []

-- Subtract
infixl 6 .-.
(.-.) :: Bit n -> Bit n -> Bit n
(.-.) = primArith2 "-" []

-- Multiply
infixl 7 .*.
(.*.) :: Bit n -> Bit n -> Bit n
(.*.) = primArith2 "*" []

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
(.<<.) :: n ~ (2^m) => Bit n -> Bit m -> Bit n
x .<<. y = Bit (primInst1 "<<" [] [] (width x))

-- Shift right
(.>>.) :: n ~ (2^m) => Bit n -> Bit m -> Bit n
x .>>. y = Bit (primInst1 ">>" [] [] (width x))

-- Register
reg :: Bit n -> Bit n -> Bit n
reg = primArith2 "reg" []

-- Register with enable
regEn :: Bit n -> Bit 1 -> Bit n -> Bit n
regEn init en a = 
  Bit (primInst1 "regEn" []
        [unbit init, unbit en, unbit a] (width a))

-- Concatenation
(#) :: Bit n -> Bit m -> Bit (n+m)
a # b = Bit (primInst1 "#" [] [unbit a, unbit b] (width a + width b))

-- Bit selection
bit :: (KnownNat n, 1 <= n) => Bit n -> Int -> Bit 1
bit a i = Bit (primInst1 "bit" params [unbit a] 1)
  where
    params = if i >= fromInteger (natVal a) then
               error "Blarney.Bit.bit: index out of range"
             else
               ["index" :-> show i]

-- Sub-range selection
bits :: (KnownNat m, m <= n) => Bit n -> (Int, Int) -> Bit m
bits a (hi, lo) = result
  where
    result = Bit (primInst1 "range" params [unbit a] (hi-lo))
    params = if lo > hi || (hi-lo) /= widthOf result then
               error "Blarney.Bit.bits: sub-range does not match bit width"
             else
               ["high" :-> show hi, "low"  :-> show lo]

-- Zero extend
zeroExtend :: (KnownNat m, n <= m) => Bit n -> Bit m
zeroExtend a = result
   where
     params     = ["ext" :-> show (w - width a)]
     result     = Bit (primInst1 "zeroExtend" params [unbit a] w)
     w          = widthOf result

-- Sign extend
signExtend :: (KnownNat m, n <= m) => Bit n -> Bit m
signExtend a = result
   where
     params     = ["ext" :-> show (w - width a),
                   "msb" :-> show (width a - 1)]
     result     = Bit (primInst1 "signExtend" params [unbit a] w)
     w          = widthOf result

-- Extract most significant bits
upper :: (KnownNat m, m <= n) => Bit n -> Bit m
upper a = result
   where
     params     = ["hi" :-> show (width a - 1),
                   "lo" :-> show (width a - w)]
     result     = Bit (primInst1 "range" params [unbit a] w)
     w          = widthOf result

-- Extract least significant bits
lower :: (KnownNat m, m <= n) => Bit n -> Bit m
lower a = result
   where
     params     = ["hi" :-> show (w - 1),
                   "lo" :-> "0"]
     result     = Bit (primInst1 "range" params [unbit a] w)
     w          = widthOf result
