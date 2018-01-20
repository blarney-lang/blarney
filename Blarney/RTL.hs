-- For overriding if/then/else
{-# LANGUAGE RebindableSyntax #-}

module Blarney.RTL where

import Blarney.Bit

-- Each RTL variable has a unique id
type VarId = Int

-- The RTL monad is a reader/writer/state monad
-- The state component is the next unique variable id
type RTLS = VarId

-- The writer component is a list of variable assignments made so far
type RTLW = [Assign]

-- The reader component is a bit defining the current condition and a
-- list of all assigments made in the RTL block.  The list of
-- assignments is obtained by circular programming, passing the
-- writer assignments of the monad as the reader assignments.
type RTLR = (Bit 1, [Assign])

-- A conditional assignment
type Assign = (Bit 1, VarId, Pin)

-- The RTL monad
data RTL a =
  RTL { runRTL :: RTLR -> RTLS -> (RTLS, RTLW, a)

instance Monad RTL where
  return a = RTL (\r s -> (s, [], a))
  m >>= f = RTL (\r s -> let (s0, w0, a) = runRTL m r s
                             (s1, w1, b) = runRTL (f a) r s0
                         in  (s1, w0 ++ w1, b))

get :: RTL RTLS
get = RTL (\r s -> (s, [], s))

set :: RTLS -> RTL ()
set s' = RTL (\r s -> (s', [], ()))

ask :: RTL RTLR
ask = RTL (\r s -> (s, [], r))

local :: RTLR -> RTL a -> RTL a
local r m = RTL (\_ s -> runRTL m r s)

write :: RTLW -> RTL ()
write w = RTL (\r s -> (s, [w], ()))

fresh :: RTL VarId
fresh = do
  v <- get
  set (v+1)
  return v

-- Mutable variables
class Var v where
  val :: v n -> Bit n
  (<==) :: v n -> Bit n -> RTL ()

-- Register variables
data Reg n = Reg { regId :: VarId, regVal :: Bit n }

-- Wire variables
data Wire n = Sig { wireId :: VarId, wireVal :: Bit n }

-- Register assignment
instance Var (Reg n) where
  val r = regVal r
  r <== x = do
    (cond, as) <- ask
    write (cond, regId r, unBit x)

-- Wire assignment
instance Var (Wire n) where
  val r = regVal r
  r <== x = do
    (cond, as) <- ask
    write (cond, wireId r, unBit x)

-- RTL if/then/else
class IfThenElse b where
  ifThenElse :: b -> a -> a -> a

instance IfThenElse RTL (Bit 1) where
  ifThenElse :: Bit 1 -> RTL () -> RTL () -> RTL ()
  ifThenElse c a b =
    do (cond, as) <- ask
       local (cond <&> c, as) a
       local (inv cond <&> c, as) a

-- Obtain only the assignments to the given variable
assigns :: VarId -> [Assign] ->

-- Create register
makeReg :: KnownNat n => Integer -> RTL (Reg (Bit n))
makeReg init =
  do v <- fresh
     (cond, as) <- ask
     let en  = orList [b | (b, w, p) <- as, v == w]
     let inp = pick [(b, Bit p :: Bit n) | (b, w, p) <- as, v == w]
     let out = regEn init en inp
     return (Reg v out)

-- Create Wire
makeWire :: KnownNat n => Integer -> RTL (Wire (Bit n))
makeWire def =
  do v <- fresh
     (cond, as) <- ask
     let none = inv (orList [b | (b, w, p) <- as, v == w])
     let asNew = as ++ [(none, v, fromInteger def]
     let out = pick [(b, Bit p :: Bit n) | (b, w, p) <- asNew, v == w]
     return (Wire v out)
