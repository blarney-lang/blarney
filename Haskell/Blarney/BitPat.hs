-- Bit-string pattern matching
-- Inspired by Morten Rhiger's "Type-Safe Pattern Combinators"

module Blarney.BitPat where

import Blarney

-- Continuation combinators
success k = (1, k)
failure k = (0, k)
one v k = (1, k v)
app m n k = (b0 .&. b1, k1)
  where
    (b0, k0) = m k
    (b1, k1) = n k0

-- Bit patterns
type BP n t0 t1 = Bit n -> t0 -> (Bit 1, t1)

-- Bit pattern to match literal
numBP :: KnownNat n => Bit n -> BP n t t
numBP a = \b k -> (a .==. b, k)

-- Bit pattern to bind a variable
varBP :: BP n (Bit n -> t) t
varBP = one

-- Sequentially combine bit patterns
infixl 9 <>
(<>) :: KnownNat n0 => BP n0 t0 t1 -> BP n1 t1 t2 -> BP (n0+n1) t0 t2
p0 <> p1 = \n -> 
  let (upper, lower) = split n in app (p0 upper) (p1 lower)

-- Pattern plus right-hand-side
infix 7 ==>
(==>) :: BP n t (RTL ()) -> t -> Bit n -> RTL ()
p ==> rhs = \sub -> let (b, rtl) = p sub rhs in when b rtl

match :: Bit n -> [Bit n -> RTL ()] -> RTL ()
match sub alts = sequence_ [ alt sub | alt <- alts ]
