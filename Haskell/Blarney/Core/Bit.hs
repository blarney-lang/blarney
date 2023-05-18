{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Blarney.Core.Bit
Description : Typed bit-vectors and circuit primitives
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019-2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

This module provides size-typed bit vectors and circuit primitives,
on top of Blarney's untyped bit vectors and circuit primitives.
Hardware developers should always use the typed versions!
-}
module Blarney.Core.Bit where

-- Untyped bit-vectors
import Blarney.Core.BV

-- Utils
import Blarney.Core.Prim
import Blarney.Core.Utils

-- Standard imports
import Prelude
import Data.Proxy
import GHC.TypeLits
import GHC.Generics

-- * Typed bit-vectors

-- |Phantom type wrapping an untyped bit vector,
-- capturing the bit-vector width.  All bit vectors
-- are members of the 'Num' and 'Cmp' classes.
newtype Bit (n :: Nat) = FromBV { toBV :: BV }

-- |Determine width of bit-vector from type
widthOf :: KnownNat n => Bit n -> Int
widthOf v = fromInteger (natVal v)

-- |Determine width of bit-vector from underlying 'BV'
unsafeWidthOf :: Bit n -> Int
unsafeWidthOf = bvPrimOutWidth . toBV

-- |Convert type Nat to Ingeter value
valueOf :: forall n. (KnownNat n) => Int
valueOf = fromInteger (natVal @n Proxy)

-- |Constant bit-vector
constant :: KnownNat n => Integer -> Bit n
constant i = result
  where
    result = FromBV $ constBV w i
    w = widthOf result

-- | Give a name to a 'Bit n' signal
nameBit :: String -> Bit n -> Bit n
nameBit nm = FromBV . (flip addBVNameHint $ NmRoot 0 nm) . toBV

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

-- |Multiplier (full precision)
fullMul :: Bool -> Bit n -> Bit n -> Bit (2*n)
fullMul isSigned a b = FromBV $ fullMulBV isSigned (toBV a) (toBV b)

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
  abs a       = mux (a `sLT` 0) [a, negate a]
  signum a    = mux (a .==. 0) [mux (a `sLT` 0) [1, -1], 0]
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

-- |Logical and
infixr 3 .&&.
(.&&.) :: Bit 1 -> Bit 1 -> Bit 1
a .&&. b = a .&. b

-- |Logical or
infixr 2 .||.
(.||.) :: Bit 1 -> Bit 1 -> Bit 1
a .||. b = a .|. b

-- |Logical implication
infixr 1 .==>.
(.==>.) :: Bit 1 -> Bit 1 -> Bit 1
a .==>. b = inv a .|. b

-- |Logical equivalence
infixr 1 .<==>.
(.<==>.) :: Bit 1 -> Bit 1 -> Bit 1
a .<==>. b = a .==. b

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

-- |Rotate left
rotl :: Bit n -> Bit m -> Bit n
rotl a b = FromBV $
  orBV (selectBV (wa-1, 0) ashft)
       (selectBV (wa*2-1, wa) ashft)
  where
    wa = unsafeWidthOf a
    ashft = leftBV (concatBV (constBV wa 0) (toBV a)) (toBV b)

-- |Rotate right
rotr :: Bit n -> Bit m -> Bit n
rotr a b = FromBV $
  orBV (selectBV (wa-1, 0) ashft)
       (selectBV (wa*2-1, wa) ashft)
  where
    wa = unsafeWidthOf a
    ashft = rightBV (concatBV (toBV a) (constBV wa 0)) (toBV b)

-- * Bit-vector comparison primitives

-- Comparison operators
class Cmp a where
  (.<.)  :: a -> a -> Bit 1
  (.<=.) :: a -> a -> Bit 1
  (.==.) :: a -> a -> Bit 1
  (.>.)  :: a -> a -> Bit 1
  (.>=.) :: a -> a -> Bit 1
  (.!=.) :: a -> a -> Bit 1

  a .>. b = b .<. a
  a .>=. b = b .<=. a
  a .!=. b = inv (a .==. b)

  -- For generic deriving
  default (.<.) :: (Generic a, GCmp (Rep a)) => a -> a -> Bit 1
  a .<. b = gCmpLT (from a) (from b)
  default (.<=.) :: (Generic a, GCmp (Rep a)) => a -> a -> Bit 1
  a .<=. b = gCmpLTE (from a) (from b)
  default (.==.) :: (Generic a, GCmp (Rep a)) => a -> a -> Bit 1
  a .==. b = gCmpEQ (from a) (from b)

infix 4 .<.
infix 4 .<=.
infix 4 .>=.
infix 4 .>.
infix 4 .==.
infix 4 .!=.

instance Cmp (Bit n) where
  a .<.  b = FromBV $ lessThanBV (toBV a) (toBV b)
  a .<=. b = FromBV $ lessThanEqBV (toBV a) (toBV b)
  a .==. b = FromBV $ equalBV (toBV a) (toBV b)
  a .!=. b = FromBV $ notEqualBV (toBV a) (toBV b)

-- |Signed less than
infixl 8 `sLT`
sLT :: Bit n -> Bit n -> Bit 1
sLT x y = invMSB x .<. invMSB y

-- |Signed greater than
infixl 8 `sGT`
sGT :: Bit n -> Bit n -> Bit 1
sGT x y = invMSB x .>. invMSB y

-- |Signed less than or equal
infixl 8 `sLTE`
sLTE :: Bit n -> Bit n -> Bit 1
sLTE x y = invMSB x .<=. invMSB y

-- |Signed greater than or equal
infixl 8 `sGTE`
sGTE :: Bit n -> Bit n -> Bit 1
sGTE x y = invMSB x .>=. invMSB y

-- * Bit-vector width adjustment

-- |Replicate bit
rep :: KnownNat n => Bit 1 -> Bit n
rep a = result
  where
    result = FromBV $ replicateBV wr (toBV a)
    wr = widthOf result

-- |Zero extension
zeroExtend :: (KnownNat m, n <= m) => Bit n -> Bit m
zeroExtend a = result
   where
     result = FromBV $ zeroExtendBV wr (toBV a)
     wr = widthOf result

-- |Sign extension
signExtend :: (KnownNat m, n <= m) => Bit n -> Bit m
signExtend a = result
   where
     result = FromBV $ signExtendBV wr (toBV a)
     wr = widthOf result

-- |Bit-vector concatenation
infixr 8 #
(#) :: Bit n -> Bit m -> Bit (n+m)
a # b = FromBV $ concatBV (toBV a) (toBV b)

-- |Extract most significant bits
upper :: (KnownNat m, m <= n) => Bit n -> Bit m
upper a = result
   where
     result = unsafeSlice (wa-1, wa-wr) a
     wa = unsafeWidthOf a
     wr = widthOf result

-- |Extract most significant bits
truncateLSB :: forall m n. (KnownNat m, m <= n) => Bit n -> Bit m
truncateLSB = upper

-- |Extract least significant bits
lower :: (KnownNat m, m <= n) => Bit n -> Bit m
lower a = result
   where
     result = unsafeSlice (wr-1, 0) a
     wa = unsafeWidthOf a
     wr = widthOf result

-- |Extract least significant bits
truncate :: forall m n. (KnownNat m, m <= n) => Bit n -> Bit m
truncate = lower

-- |Split bit vector
split :: KnownNat n => Bit (n+m) -> (Bit n, Bit m)
split a = (a0, a1)
  where
    wa = unsafeWidthOf a
    w0 = widthOf a0
    a0 = unsafeSlice (wa-1, wa-w0) a
    a1 = unsafeSlice (wa-w0-1, 0) a

-- |Drop most significant bits
dropBits :: forall d n. KnownNat d => Bit (d+n) -> Bit n
dropBits a = c
  where (b, c) = split a

-- |Drop least significant bits
dropBitsLSB :: forall d n. KnownNat n => Bit (n+d) -> Bit n
dropBitsLSB a = b
  where (b, c) = split a

-- |Invert most significant bit
invMSB :: Bit n -> Bit n
invMSB x = twiddle `onBitList` x
  where twiddle bs = init bs ++ [inv (last bs)]

-- * Bit-vector selection primitives

-- | Statically-typed bit selection. Use type application to specify
--   upper and lower indices.
slice :: forall (hi :: Nat) (lo :: Nat) i o.
           (KnownNat hi, KnownNat lo, (lo+o) ~ (hi+1), (hi+1) <= i, o <= i)
      => Bit i -> Bit o
slice a = unsafeSlice (valueOf @hi, valueOf @lo) a

-- | Dynamically-typed bit selection
unsafeCheckedSlice :: KnownNat m => (Int, Int) -> Bit n -> Bit m
unsafeCheckedSlice (hi, lo) a =
  case lo > hi || (hi+1-lo) /= wr of
    True -> error "Blarney: sub-range does not match bit width"
    False -> result
  where
    result = FromBV $ selectBV (hi, lo) (toBV a)
    wr = widthOf result

-- | Untyped bit selection (try to avoid!)
unsafeSlice :: (Int, Int) -> Bit n -> Bit m
unsafeSlice (hi, lo) a = FromBV $ selectBV (hi, lo) (toBV a)

-- |Statically-typed bit indexing. Use type application to specify index.
at :: forall (i :: Nat) n. (KnownNat i, (i+1) <= n)
      => Bit n -> Bit 1
at a = unsafeAt (valueOf @i) a

-- | Dynamically-typed bit indexing
unsafeAt :: Int -> Bit n -> Bit 1
unsafeAt i a =
  case i >= wa of
    True -> error ("Bit index " ++ show i ++ " out of range ["
                                ++ show (wa-1) ++ ":0]")
    False -> result
  where
    wa = unsafeWidthOf a
    result = FromBV $ selectBV (i, i) (toBV a)

-- * Bit-vector registers

-- |Register
reg :: Bit n -> Bit n -> Bit n
reg init a = FromBV $ regBV w (toBV init) (toBV a)
  where w = unsafeWidthOf init

-- |Register with enable wire
regEn :: Bit n -> Bit 1 -> Bit n -> Bit n
regEn init en a =
    FromBV $ regEnBV w (toBV init) (toBV en) (toBV a)
  where w = unsafeWidthOf init

-- | Merge inputs together according to a merging strategy
mergeWritesBit :: MergeStrategy -> Width -> [(Bit 1, Bit n)] -> Bit n
mergeWritesBit strat w ins = FromBV $ mergeWritesBV strat w ins'
  where ins' = map (\(en, x) -> (toBV en, toBV x)) ins

-- * Misc. bit-vector operations

-- | Multiplexer using a selector signal to index a list of input signals.
--   Raises a circuit generation time error on empty list of inputs
mux :: Bit w -> [Bit n] -> Bit n
mux _ [] = error "cannot mux an empty list"
mux sel xs = FromBV $ muxBV (toBV sel) (toBV <$> xs)

-- |Lift integer value to type-level natural
liftNat :: Int -> (forall n. KnownNat n => Proxy n -> a) -> a
liftNat nat k =
  case someNatVal (toInteger nat) of
    Just (SomeNat (x :: Proxy n)) -> do
      k x

-- |Convert list of bits to bit vector
fromBitList :: KnownNat n => [Bit 1] -> Bit n
fromBitList xs
  | length xs == n = result
  | otherwise = error ("fromBitList: width mismatch: " ++
                          show (length xs, n))
  where
    n = widthOf result
    result = unsafeFromBitList xs

-- |Convert bit vector to list of bits
toBitList :: KnownNat n => Bit n -> [Bit 1]
toBitList vec = [unsafeAt i vec | i <- [0..n-1]]
  where n = widthOf vec

-- | One-hot bit list
newtype OneHotList = OneHotList [Bit 1]

-- | Collapse a '[Bit 1]' of size n to a single 'Bit n'
unsafeFromBitList :: [Bit 1] -> Bit n
unsafeFromBitList [] = FromBV $ constBV 0 0
unsafeFromBitList bs = FromBV $ foldr1 concatBV (reverse (fmap toBV bs))

-- | Expand a single 'Bit n' to a '[Bit 1]' of size n
unsafeToBitList :: Bit n -> [Bit 1]
unsafeToBitList bs = [unsafeAt i bs | i <- [0..size-1]]
  where size = unsafeWidthOf bs

-- | Apply bit-list transformation on bit-vector
onBitList :: ([Bit 1] -> [Bit 1]) -> Bit n -> Bit n
onBitList f x
  | null list = x
  | length list /= length list' =
      error "onBitList: transformation did not preserve length"
  | otherwise = unsafeFromBitList list'
  where
    list = unsafeToBitList x
    list' = f list

-- | Cast one size of vector to another (dangerous)
unsafeBitCast :: Bit n -> Bit m
unsafeBitCast = FromBV . toBV

-- | Similar to 'truncate' but width check done at elaboration time
truncateCast :: (KnownNat n, KnownNat m) => Bit n -> Bit m
truncateCast x
  | wx >= wy = y
  | otherwise = error "truncateCast: output larger than input"
  where
    y = unsafeSlice (wy-1, 0) x
    wx = widthOf x
    wy = widthOf y

-- | Similar to 'truncateLSB' but width check done at elaboration time
truncateLSBCast :: (KnownNat n, KnownNat m) => Bit n -> Bit m
truncateLSBCast x
  | wx >= wy = y
  | otherwise = error "truncateLSBCast: output larger than input"
  where
    y = unsafeSlice (wx-1, wx-wy) x
    wx = widthOf x
    wy = widthOf y

-- | Similar to 'zeroExtend' but width check done at elaboration time
zeroExtendCast :: (KnownNat n, KnownNat m) => Bit n -> Bit m
zeroExtendCast x
  | wx <= wy = y
  | otherwise = error "zeroExtendCast: input larger than output"
  where
    y = FromBV $ zeroExtendBV wy (toBV x)
    wx = widthOf x
    wy = widthOf y

-- | Zero extend or truncate input to give output
cast :: (KnownNat n, KnownNat m) => Bit n -> Bit m
cast x = y
  where
    y = case wx >= wy of
          True  -> unsafeSlice (wy-1, 0) x
          False -> FromBV $ zeroExtendBV wy (toBV x)
    wx = widthOf x
    wy = widthOf y

-- | For generic deriving of Cmp class
class GCmp f where
  gCmpLT :: f a -> f a -> Bit 1
  gCmpLTE :: f a -> f a -> Bit 1
  gCmpEQ :: f a -> f a -> Bit 1

instance GCmp U1 where
  gCmpLT U1 U1 = false
  gCmpLTE U1 U1 = true
  gCmpEQ U1 U1 = true

instance (GCmp a, GCmp b) => GCmp (a :*: b) where
  gCmpLT (x0 :*: y0) (x1 :*: y1) =
    gCmpLT x0 x1 .||. (gCmpEQ x0 x1 .&&. gCmpLT y0 y1)
  gCmpLTE (x0 :*: y0) (x1 :*: y1) =
    gCmpLT x0 x1 .||. (gCmpEQ x0 x1 .&&. gCmpLTE y0 y1)
  gCmpEQ (x0 :*: y0) (x1 :*: y1) = gCmpEQ x0 x1 .&&. gCmpEQ y0 y1

instance GCmp a => GCmp (M1 i c a) where
  gCmpLT (M1 x) (M1 y) = gCmpLT x y
  gCmpLTE (M1 x) (M1 y) = gCmpLTE x y
  gCmpEQ (M1 x) (M1 y) = gCmpEQ x y

instance Cmp a => GCmp (K1 i a) where
  gCmpLT (K1 x) (K1 y) = x .<. y
  gCmpLTE (K1 x) (K1 y) = x .<=. y
  gCmpEQ (K1 x) (K1 y) = x .==. y

instance Cmp ()
instance (Cmp a, Cmp b) => Cmp (a, b)
instance (Cmp a, Cmp b, Cmp c) => Cmp (a, b, c)
instance (Cmp a, Cmp b, Cmp c, Cmp d) => Cmp (a, b, c, d)
instance (Cmp a, Cmp b, Cmp c, Cmp d, Cmp e) => Cmp (a, b, c, d, e)
instance (Cmp a, Cmp b, Cmp c, Cmp d, Cmp e, Cmp f) =>
         Cmp (a, b, c, d, e, f)
instance (Cmp a, Cmp b, Cmp c, Cmp d, Cmp e, Cmp f, Cmp g) =>
         Cmp (a, b, c, d, e, f, g)

-- * Signed typed bit-vectors

-- | Signed bit vectors
newtype Signed (n :: Nat) = Signed (Bit n)

-- | Convert to signed bit-vector
toSigned :: Bit n -> Signed n
toSigned = Signed

-- | Convert from signed bit-vector
fromSigned :: Signed n -> Bit n
fromSigned (Signed bv) = bv

instance Cmp (Signed n) where
  Signed a .<.  Signed b = a `sLT` b
  Signed a .<=. Signed b = a `sLTE` b
  Signed a .==. Signed b = a .==. b
  Signed a .!=. Signed b = a .!=. b
