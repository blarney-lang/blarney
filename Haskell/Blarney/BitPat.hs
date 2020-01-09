{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : Blarney.BitPat
Description : Bit-string pattern matching combinators
Copyright   : (c) Matthew Naylor, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

Inspired by Morten Rhiger's "Type-Safe Pattern Combinators".
-}
module Blarney.BitPat
  ( BP        -- Bit patterns
  , lit       -- Literal pattern
  , var       -- Variable pattern
  , (<#>)     -- Sequential composition of patterns
  , (==>)     -- Case alternative
  , match     -- Match case subject against set of a case alternatives
  ) where

import Blarney

-- |Continuation combinators
success k = (1, k)
failure k = (0, k)
one v k = (1, k v)
app m n k = (b0 .&. b1, k1)
  where
    (b0, k0) = m k
    (b1, k1) = n k0

-- |Bit pattern
type BP n t0 t1 = Bit n -> t0 -> (Bit 1, t1)

-- |Bit pattern to match literal
lit :: forall n t. KnownNat n => Bit n -> BP n t t
lit a = \b k -> (a .==. b, k)

-- |Bit pattern to bind a variable
var :: forall n t. BP n (Bit n -> t) t
var = one

-- |Sequentially combine bit patterns
infixl 9 <#>
(<#>) :: KnownNat n0 => BP n0 t0 t1 -> BP n1 t1 t2 -> BP (n0+n1) t0 t2
p0 <#> p1 = \n ->
  let (upper, lower) = split n in app (p0 upper) (p1 lower)

-- |A case alternative (pattern plus a right-hand-side)
infix 7 ==>
(==>) :: BP n t (Action ()) -> t -> Bit n -> Action ()
p ==> rhs = \sub -> let (b, act) = p sub rhs in when b act

-- |Match case subject against set of a case alternatives
match :: Bit n -> [Bit n -> Action ()] -> Action ()
match sub alts = sequence_ [ alt sub | alt <- alts ]
