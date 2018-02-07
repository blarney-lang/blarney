-- For type-level naturals
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, TypeFamilies, GADTs #-}

module Blarney.Bit
  ( Bit(..)      -- "Bit n" is a bit vector of n bits
  , widthOf      -- Obtain width using type
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
import GHC.TypeLits

-- Phantom type wrapping an untyped bit vector, capturing the bit width
data Bit (n :: Nat) where
  Bit :: KnownNat n => Unbit -> Bit n

-- Get back the untyped representation
unbit :: Bit n -> Unbit
unbit (Bit b) = b

-- Determine bit width from type
widthOf :: Bit n -> Int
widthOf a@(Bit _) = fromInteger (natVal a)

-- Replicate bit
replicateBit :: KnownNat n => Bit 1 -> Bit n
replicateBit a = result
  where result = Bit (makePrim1 (ReplicateBit (widthOf result) [unbit a]))

-- All 0's
low :: KnownNat n => Bit n
low = replicateBit 0

-- All 1's
high :: KnownNat n => Bit n
high = replicateBit 1

-- Bitwise invert
inv :: Bit n -> Bit n
inv a@(Bit ua) = Bit (makePrim1 (Not (widthOf a)) [ua])

-- Bitwise and
infixl 7 .&.
(.&.) :: KnownNat n => Bit n -> Bit n -> Bit n
a@(Bit ua) .&. Bit ub = Bit (makePrim1 (And (natVal a)) [ua, ub])
--a@(Bit ua) .&. Bit ub = Bit (makePrim1 (And (widthOf a)) [ua, ub])

-- Bitwise or
infixl 5 .|.
(.|.) :: Bit n -> Bit n -> Bit n
a@(Bit ua) .|. Bit ub = Bit (makePrim1 (Or (widthOf a)) [ua, ub])

-- Bitwise xor
infixl 6 .^.
(.^.) :: Bit n -> Bit n -> Bit n
a@(Bit ua) .^. Bit ub = Bit (makePrim1 (Xor (widthOf a)) [ua, ub])

-- Equality
(.==.) :: Bit n -> Bit n -> Bit 1
a@(Bit ua) .==. Bit ub = Bit (makePrim1 (Equal (widthOf a)) [ua, ub])

-- Disequality
(.!=.) :: Bit n -> Bit n -> Bit 1
a@(Bit ua) .!=. Bit ub = Bit (makePrim1 (NotEqual (widthOf a)) [ua, ub])

-- Less than
(.<.) :: Bit n -> Bit n -> Bit 1
a@(Bit ua) .<. Bit ub = Bit (makePrim1 (LessThan (widthOf a)) [ua, ub])

-- Less than or equal
(.<=.) :: Bit n -> Bit n -> Bit 1
a@(Bit ua) .<=. Bit ub = Bit (makePrim1 (LessThanEq (widthOf a)) [ua, ub])

-- Greater than
(.>.) :: Bit n -> Bit n -> Bit 1
a@(Bit ua) .>. Bit ub = Bit (makePrim1 (GreaterThan (widthOf a)) [ua, ub])

-- Greater than or equal
(.>=.) :: Bit n -> Bit n -> Bit 1
a@(Bit ua) .>=. Bit ub = Bit (makePrim1 (GreaterThanEq (widthOf a)) [ua, ub])

-- Add
infixl 6 .+.
(.+.) :: Bit n -> Bit n -> Bit n
a@(Bit ua) .+. Bit ub = Bit (makePrim1 (Add (widthOf a)) [ua, ub])

-- Subtract
infixl 6 .-.
(.-.) :: Bit n -> Bit n -> Bit n
a@(Bit ua) .-. Bit ub = Bit (makePrim1 (Sub (widthOf a)) [ua, ub])

-- Multiply
infixl 7 .*.
(.*.) :: Bit n -> Bit n -> Bit n
a@(Bit ua) .*. Bit ub = Bit (makePrim1 (Mul (widthOf a)) [ua, ub])

-- Mux
mux :: Bit 1 -> Bit n -> Bit n -> Bit n
mux (Bit uc) a@(Bit ua) (Bit ub) =
  Bit (makePrim1 (Mux (widthOf a)) [uc, ua, ub])

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
     result     = Bit (makePrim1 (Const w i) [])
     w          = widthOf result

-- Division
instance KnownNat n => Fractional (Bit n) where
  a@(Bit ua) / Bit ub = Bit (makePrim1 (Div (widthOf a)) [ua, ub])
  recip        = error "Undefined function: 'recip' for bit vectors"
  fromRational = error "Undefined function: 'fromRational' for bit vectors"

-- Modulus
infixl 7 %
(%) :: Bit n -> Bit n -> Bit n
a@(Bit ua) / Bit ub = Bit (makePrim1 (Mod (widthOf a)) [ua, ub])

-- Shift left
(.<<.) :: Bit n -> Bit n -> Bit n
a@(Bit ua) .<<. Bit ub = Bit (makePrim1 (ShiftLeft (widthOf a)) [ua, ub])

-- Shift right
(.>>.) :: Bit n -> Bit n -> Bit n
a@(Bit ua) .>>. Bit ub = Bit (makePrim1 (ShiftRight (widthOf a)) [ua, ub])

-- Shift right
countOnes :: Bit (2^n) -> Bit (n+1)
countOnes a@(Bit ua) = Bit (makePrim1 (CountOnes w) [ua])
  where
    w = log2 (widthOf a) + 1
    log2 1 = 0
    log2 n = 1 + log2 (n `div` 2)

-- Register
reg :: KnownNat n => Bit n -> Bit n -> Bit n
reg init a = 
  Bit (primInst1 "reg" ["init" :-> getInit (unbit init)]
        [unbit a] (widthOf init))

-- Determine initialiser
getInit :: Unbit -> String
getInit b
  | unbitPrim b == "const" = lookupParam (unbitParams b) "val"
  | otherwise = error "Initialiser must be a constant"

-- Register with enable
regEn :: KnownNat n => Bit n -> Bit 1 -> Bit n -> Bit n
regEn init en a = 
  Bit (primInst1 "regEn" ["init" :-> getInit (unbit init)]
        [unbit en, unbit a] (widthOf init))

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
               ["hi" :-> show hi, "lo"  :-> show lo]

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
