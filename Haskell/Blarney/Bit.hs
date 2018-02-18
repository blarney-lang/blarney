-- For type-level naturals
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies, GADTs #-}

module Blarney.Bit
  ( Bit(..)      -- "Bit n" is a bit vector of n bits
  , replicateBit -- Replicate bit to produce bit vector
  , low          -- Bit vector of all 0's
  , high         -- Bit vector of all 1's
  , inv          -- Bitwise invert
  , (.&.)        -- Bitwise and
  , (.|.)        -- Bitwise or
  , (.^.)        -- Bitwise xor
  , mux          -- Mux
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
  , countOnes    -- Population count
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
import Blarney.Util
import GHC.TypeLits

-- Phantom type wrapping an untyped bit vector, capturing the bit width
newtype Bit (n :: Nat) = Bit { unbit :: Unbit }

-- Extract width from bit vector
width :: Bit n -> Int
width = unbitWidth . unbit

-- Replicate bit
replicateBit :: KnownNat n => Bit 1 -> Bit n
replicateBit a = result
  where
    n = fromInteger (natVal result)
    result = Bit (makePrim1 (ReplicateBit n) [unbit a] n)

-- All 0's
low :: KnownNat n => Bit n
low = replicateBit 0

-- All 1's
high :: KnownNat n => Bit n
high = replicateBit 1

-- Bitwise invert
inv :: Bit n -> Bit n
inv a = Bit (makePrim1 (Not wa) [unbit a] wa)
  where wa = width a

-- Bitwise and
infixl 7 .&.
(.&.) :: Bit n -> Bit n -> Bit n
a .&. b = Bit (makePrim1 (And wa) [unbit a, unbit b] wa)
  where wa = width a

-- Bitwise or
infixl 5 .|.
(.|.) :: Bit n -> Bit n -> Bit n
a .|. b = Bit (makePrim1 (Or wa) [unbit a, unbit b] wa)
  where wa = width a

-- Bitwise xor
infixl 6 .^.
(.^.) :: Bit n -> Bit n -> Bit n
a .^. b = Bit (makePrim1 (Xor wa) [unbit a, unbit b] wa)
  where wa = width a

-- Equality
(.==.) :: Bit n -> Bit n -> Bit 1
a .==. b = Bit (makePrim1 (Equal wa) [unbit a, unbit b] 1)
  where wa = width a

-- Disequality
(.!=.) :: Bit n -> Bit n -> Bit 1
a .!=. b = Bit (makePrim1 (NotEqual wa) [unbit a, unbit b] 1)
  where wa = width a

-- Less than
(.<.) :: Bit n -> Bit n -> Bit 1
a .<. b = Bit (makePrim1 (LessThan wa) [unbit a, unbit b] 1)
  where wa = width a

-- Less than or equal
(.<=.) :: Bit n -> Bit n -> Bit 1
a .<=. b = Bit (makePrim1 (LessThanEq wa) [unbit a, unbit b] 1)
  where wa = width a

-- Greater than
(.>.) :: Bit n -> Bit n -> Bit 1
a .>. b = b .<. a

-- Greater than or equal
(.>=.) :: Bit n -> Bit n -> Bit 1
a .>=. b = b .<=. a

-- Add
infixl 6 .+.
(.+.) :: Bit n -> Bit n -> Bit n
a .+. b = Bit (makePrim1 (Add wa) [unbit a, unbit b] wa)
  where wa = width a

-- Subtract
infixl 6 .-.
(.-.) :: Bit n -> Bit n -> Bit n
a .-. b = Bit (makePrim1 (Sub wa) [unbit a, unbit b] wa)
  where wa = width a

-- Multiply
infixl 7 .*.
(.*.) :: Bit n -> Bit n -> Bit n
a .*. b = Bit (makePrim1 (Mul wa) [unbit a, unbit b] wa)
  where wa = width a

-- Mux
mux :: Bit 1 -> Bit n -> Bit n -> Bit n
mux c a b = Bit (makePrim1 (Mux wa) [unbit c, unbit a, unbit b] wa)
  where wa = width a

-- Arithmetic
instance KnownNat n => Num (Bit n) where
  (+)           = (.+.)
  (-)           = (.-.)
  (*)           = (.*.)
  negate a      = inv a .+. 1
  abs           = id
  signum a      = mux (a .==. 0) 0 1
  fromInteger i = result
   where
     result     = Bit (makePrim1 (Const w i) [] w)
     w          = fromInteger (natVal result)

-- Division
instance KnownNat n => Fractional (Bit n) where
  a / b        = Bit (makePrim1 (Div wa) [unbit a, unbit b] wa)
    where wa   = width a
  recip        = error "Undefined function: 'recip' for bit vectors"
  fromRational = error "Undefined function: 'fromRational' for bit vectors"

-- Modulus
infixl 7 %
(%) :: Bit n -> Bit n -> Bit n
a % b = Bit (makePrim1 (Mod wa) [unbit a, unbit b] wa)
  where wa = width a

-- Shift left
(.<<.) :: Bit n -> Bit n -> Bit n
a .<<. b = Bit (makePrim1 (ShiftLeft wa) [unbit a, unbit b] wa)
  where wa = width a

-- Shift right
(.>>.) :: Bit n -> Bit n -> Bit n
a .>>. b = Bit (makePrim1 (ShiftRight wa) [unbit a, unbit b] wa)
  where wa = width a

-- Shift right
countOnes :: Bit (2^n) -> Bit (n+1)
countOnes a = Bit (makePrim1 (CountOnes wr) [unbit a] wr)
  where wr = log2 (width a) + 1

-- Register
reg :: Bit n -> Bit n -> Bit n
reg init a = Bit (makePrim1 (Register (getInit (unbit init)) w) [unbit a] w)
  where w = width init

-- Determine initialiser
getInit :: Unbit -> Integer
getInit a =
  case unbitPrim a of
    Const _ i -> i
    other -> error "Register initialiser must be a constant"

-- Register with enable
regEn :: Bit n -> Bit 1 -> Bit n -> Bit n
regEn init en a =
    Bit (makePrim1 (RegisterEn (getInit (unbit init)) w) [unbit en, unbit a] w)
  where w = width init

-- Concatenation
(#) :: Bit n -> Bit m -> Bit (n+m)
a # b = Bit (makePrim1 (Concat wa wb) [unbit a, unbit b] (wa+wb))
  where
    wa = width a
    wb = width b

-- Bit selection
bit :: Bit n -> Int -> Bit 1
bit a i = Bit (makePrim1 p [unbit a] 1)
  where
    p = if i >= width a then
          error ("Blarney.Bit.bit: index " ++ show i ++
                   " out of range [" ++ show (width a) ++ ":0]")
        else
          SelectBits (width a) i i

-- Sub-range selection
bits :: (KnownNat m, m <= n) => Bit n -> (Int, Int) -> Bit m
bits a (hi, lo) = result
  where
    result = Bit (makePrim1 p [unbit a] wr)
    wr = fromInteger (natVal result)
    p = if lo > hi || (hi-lo) /= wr then
          error "Blarney.Bit.bits: sub-range does not match bit width"
        else
          SelectBits (width a) hi lo

-- Zero extend
zeroExtend :: (KnownNat m, n <= m) => Bit n -> Bit m
zeroExtend a = result
   where
     result = Bit (makePrim1 (ZeroExtend (width a) wr) [unbit a] wr)
     wr     = fromInteger (natVal result)

-- Sign extend
signExtend :: (KnownNat m, n <= m) => Bit n -> Bit m
signExtend a = result
   where
     result = Bit (makePrim1 (SignExtend (width a) wr) [unbit a] wr)
     wr     = fromInteger (natVal result)

-- Extract most significant bits
upper :: (KnownNat m, m <= n) => Bit n -> Bit m
upper a = result
   where
     result = Bit (makePrim1 (SelectBits wa (wa-1) (wa-wr)) [unbit a] wr)
     wa     = width a
     wr     = fromInteger (natVal result)

-- Extract least significant bits
lower :: (KnownNat m, m <= n) => Bit n -> Bit m
lower a = result
   where
     result = Bit (makePrim1 (SelectBits wa (wr-1) 0) [unbit a] wr)
     wa     = width a
     wr     = fromInteger (natVal result)