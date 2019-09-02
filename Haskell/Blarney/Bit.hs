{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Blarney.Bit
Description : Typed bit-vectors and circuit primitives
Copyright   : (c) Matthew Naylor, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

This module provides size-typed bit vectors and circuit primitives,
on top of Blarney's untyped bit vectors and circuit primitives.
Hardware developers should always use the typed versions!
-}
module Blarney.Bit where

-- Untyped bit-vectors
import Blarney.BV

-- Utils
import Blarney.Util
import Blarney.IfThenElse

-- Standard imports
import Prelude
import GHC.TypeLits
import Data.Proxy

-- * Typed bit-vectors

-- |Phantom type wrapping an untyped bit vector,
-- capturing the bit-vector width.  All bit vectors
-- are members of the 'Num' and 'Cmp' classes.
newtype Bit (n :: Nat) = FromBV { toBV :: BV }

-- |Determine width of bit-vector from type
widthOf :: KnownNat n => Bit n -> Int
widthOf v = fromInteger (natVal v)

-- |Constant bit-vector
constant :: KnownNat n => Integer -> Bit n
constant i = result
  where
    result = FromBV $ constBV w i
    w = widthOf result

-- | Give a name to a 'Bit n' signal
nameBit :: String -> Bit n -> Bit n
nameBit nm = FromBV . (flip addBVNameHint $ nm) . toBV

-- |Test plusargs
testPlusArgs :: String -> Bit 1
testPlusArgs = FromBV . testPlusArgsBV

-- |True
true :: Bit 1
true = 1

-- |False
false :: Bit 1
false = 0

-- * Bit-vector arithmetic

-- |Adder
infixl 6 .+.
(.+.) :: Bit n -> Bit n -> Bit n
a .+. b = FromBV $ addBV (toBV a) (toBV b)

-- |Subtractor
infixl 6 .-.
(.-.) :: Bit n -> Bit n -> Bit n
a .-. b = FromBV $ subBV (toBV a) (toBV b)

-- |Multiplier
infixl 7 .*.
(.*.) :: Bit n -> Bit n -> Bit n
a .*. b = FromBV $ mulBV (toBV a) (toBV b)

-- |Quotient
infixl 7 ./.
(./.) :: Bit n -> Bit n -> Bit n
a ./. b = FromBV $ divBV (toBV a) (toBV b)

-- |Remainder
infixl 7 .%.
(.%.) :: Bit n -> Bit n -> Bit n
a .%. b = FromBV $ modBV (toBV a) (toBV b)

-- Arithmetic
instance KnownNat n => Num (Bit n) where
  (+)         = (.+.)
  (-)         = (.-.)
  (*)         = (.*.)
  negate a    = inv a .+. 1
  abs a       = mux (a .<. 0) (negate a, a)
  signum a    = mux (a .==. 0) (0, mux (a .<. 0) (-1, 1))
  fromInteger = constant

-- * Bitwise operations on bit-vectors

-- |Bitwise invert
inv :: Bit n -> Bit n
inv = FromBV . invBV . toBV

-- |Bitwise and
infixl 7 .&.
(.&.) :: Bit n -> Bit n -> Bit n
a .&. b = FromBV $ andBV (toBV a) (toBV b)

-- |Bitwise or
infixl 5 .|.
(.|.) :: Bit n -> Bit n -> Bit n
a .|. b = FromBV $ orBV (toBV a) (toBV b)

-- |Bitwise xor
infixl 6 .^.
(.^.) :: Bit n -> Bit n -> Bit n
a .^. b = FromBV $ xorBV (toBV a) (toBV b)

-- |Shift left
infixl 8 .<<.
(.<<.) :: Bit n -> Bit m -> Bit n
a .<<. b = FromBV $ leftBV (toBV a) (toBV b)

-- |Shift right
infixl 8 .>>.
(.>>.) :: Bit n -> Bit m -> Bit n
a .>>. b = FromBV $ rightBV (toBV a) (toBV b)

-- |Arithmetic shift right
infixl 8 .>>>.
(.>>>.) :: Bit n -> Bit m -> Bit n
a .>>>. b = FromBV $ arithRightBV (toBV a) (toBV b)

-- * Bit-vector comparison primitives

-- Comparison operators
class Cmp a where
  (.<.)  :: a -> a -> Bit 1
  (.<=.) :: a -> a -> Bit 1
  (.==.) :: a -> a -> Bit 1
  (.>.)  :: a -> a -> Bit 1
  (.>=.) :: a -> a -> Bit 1
  (.!=.) :: a -> a -> Bit 1

infix 4 .<.
infix 4 .<=.
infix 4 .>=.
infix 4 .>.
infix 4 .==.
infix 4 .!=.

instance Cmp (Bit n) where
  a .<.  b = FromBV $ lessThanBV (toBV a) (toBV b)
  a .>.  b = FromBV $ lessThanBV (toBV b) (toBV a)
  a .<=. b = FromBV $ lessThanEqBV (toBV a) (toBV b)
  a .>=. b = FromBV $ lessThanEqBV (toBV b) (toBV a)
  a .==. b = FromBV $ equalBV (toBV a) (toBV b)
  a .!=. b = FromBV $ notEqualBV (toBV a) (toBV b)

-- * Bit-vector width adjustment

-- |Replicate bit
rep :: KnownNat n => Bit 1 -> Bit n
rep a = result
  where
    result = FromBV $ replicateBV wr (toBV a)
    wr = fromInteger (natVal result)

-- |Zero extension
zeroExtend :: (KnownNat m, n <= m) => Bit n -> Bit m
zeroExtend a = result
   where
     result = FromBV $ zeroExtendBV wr (toBV a)
     wr = fromInteger (natVal result)

-- |Sign extension
signExtend :: (KnownNat m, n <= m) => Bit n -> Bit m
signExtend a = result
   where
     result = FromBV $ signExtendBV wr (toBV a)
     wr = fromInteger (natVal result)

-- |Bit-vector concatenation
(#) :: Bit n -> Bit m -> Bit (n+m)
a # b = FromBV $ concatBV (toBV a) (toBV b)

-- |Extract most significant bits
upper :: (KnownNat m, m <= n) => Bit n -> Bit m
upper a = result
   where
     result = unsafeBits (wa-1, wa-wr) a
     wa = bvWidth (toBV a)
     wr = fromInteger (natVal result)

-- |Extract most significant bits
truncateLSB :: (KnownNat m, m <= n) => Bit n -> Bit m
truncateLSB = upper

-- |Extract least significant bits
lower :: (KnownNat m, m <= n) => Bit n -> Bit m
lower a = result
   where
     result = unsafeBits (wr-1, 0) a
     wa = bvWidth (toBV a)
     wr = fromInteger (natVal result)

-- |Extract least significant bits
truncate :: (KnownNat m, m <= n) => Bit n -> Bit m
truncate = lower

-- |Split bit vector
split :: KnownNat n => Bit (n+m) -> (Bit n, Bit m)
split a = (a0, a1)
  where
    wa = bvWidth (toBV a)
    w0 = fromInteger (natVal a0)
    a0 = unsafeBits (wa-1, wa-w0) a
    a1 = unsafeBits (wa-w0-1, 0) a

-- |Drop most significant bits
dropBits :: forall d n. KnownNat d => Bit (d+n) -> Bit n
dropBits a = c
  where (b, c) = split a

-- |Drop least significant bits
dropBitsLSB :: forall d n. KnownNat n => Bit (n+d) -> Bit n
dropBitsLSB a = b
  where (b, c) = split a

-- |Invert most significant bit
invMSB :: Bit (1+n) -> Bit (1+n)
invMSB a = inv top # bot
  where (top :: Bit 1, bot) = split a

-- * Bit-vector selection primitives

-- |Dynamically-typed bit selection
bits :: KnownNat m => (Int, Int) -> Bit n -> Bit m
bits (hi, lo) a =
  case lo > hi || (hi+1-lo) /= wr of
    True -> error "Blarney: sub-range does not match bit width"
    False -> result
  where
    result = FromBV $ selectBV (hi, lo) (toBV a)
    wr = fromInteger (natVal result)

-- |Untyped bit selection (try to avoid!)
unsafeBits :: (Int, Int) -> Bit n -> Bit m
unsafeBits (hi, lo) a = FromBV $ selectBV (hi, lo) (toBV a)

-- |Statically-typed bit selection.  Use type application to specify
-- upper and lower indices.
range :: forall (hi :: Nat) (lo :: Nat) i o.
           (KnownNat hi, KnownNat lo, (lo+o) ~ (hi+1), (hi+1) <= i, o <= i)
      => Bit i -> Bit o
range a = unsafeBits (hiVal, loVal) a
  where
    hiVal = fromInteger $ natVal (Proxy :: Proxy hi)
    loVal = fromInteger $ natVal (Proxy :: Proxy lo)

-- |Dynamically-typed bit indexing
bit :: Int -> Bit n -> Bit 1
bit i a =
  case i >= wa of
    True -> error ("Bit index " ++ show i ++ " out of range ["
                                ++ show (wa-1) ++ ":0]")
    False -> result
  where
    wa = bvWidth (toBV a)
    result = FromBV $ selectBV (i, i) (toBV a)

-- |Statically-typed bit indexing.  Use type application to specify index.
index :: forall (i :: Nat) n. (KnownNat i, (i+1) <= n)
      => Bit n -> Bit 1
index a = bit idx a
  where idx = fromInteger $ natVal (Proxy :: Proxy i)

-- * Bit-vector registers

-- |Register
reg :: Bit n -> Bit n -> Bit n
reg init a = FromBV $ regBV w (toBV init) (toBV a)
  where w = bvWidth (toBV init)

-- |Register with enable wire
regEn :: Bit n -> Bit 1 -> Bit n -> Bit n
regEn init en a =
    FromBV $ regEnBV w (toBV init) (toBV en) (toBV a)
  where w = bvWidth (toBV init)

-- * Misc. bit-vector operations

-- |Multiplexer
mux :: Bit 1 -> (Bit n, Bit n) -> Bit n
mux c (a, b) = FromBV $ muxBV (toBV c) (toBV a, toBV b)

-- |Overloaded if-then-else
instance IfThenElse (Bit 1) (Bit n) where
  ifThenElse cond a b = mux cond (a, b)

-- |Population count
countOnes :: Bit n -> Bit (Log2 n + 1)
countOnes a = FromBV $ countOnesBV wr (toBV a)
  where
    wa = bvWidth (toBV a)
    wr = log2 wa + 1

-- |Lift integer value to type-level natural
liftNat :: Int -> (forall n. KnownNat n => Proxy n -> a) -> a
liftNat nat k =
  case someNatVal (toInteger nat) of
    Just (SomeNat (x :: Proxy n)) -> do
      k x

-- |Convert list of bits to bit vector
fromBitList :: KnownNat n => [Bit 1] -> Bit n
fromBitList [] = error "fromList: applied to empty list"
fromBitList (x:xs)
  | length (x:xs) == n = result
  | otherwise =
     error ("fromList: bit vector width mismatch: " ++ show (n, length (x:xs)))
  where
    n       = widthOf result
    result  = FromBV (snd $ join (x:xs))
    join [x] = (1, toBV x)
    join (x:xs) =
      let (n, y) = join xs in
        (n+1, concatBV y (toBV x))

-- |Convert bit vector to list of bits
toBitList :: KnownNat n => Bit n -> [Bit 1]
toBitList vec = [bit i vec | i <- [0..n-1]]
  where n = widthOf vec
