-- For overriding if/then/else
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators,
      TypeFamilies, RebindableSyntax, MultiParamTypeClasses,
        FlexibleContexts, ScopedTypeVariables #-}

module Blarney.RTL where

import Prelude
import Blarney.Bit
import Blarney.Bits
import Blarney.Pin
import Blarney.Prelude
import Blarney.Format
import Control.Monad
import GHC.TypeLits

-- Each RTL variable has a unique id
type VarId = Int

-- The RTL monad is a reader/writer/state monad
-- The state component is the next unique variable id
type RTLS = VarId

-- The writer component is a list of RTL actions
type RTLW = [RTLAction]

-- RTL actions
data RTLAction =
    RTLAssign Assign
  | RTLDisplay (Bit 1, Format)

-- The reader component is a bit defining the current condition and a
-- list of all assigments made in the RTL block.  The list of
-- assignments is obtained by circular programming, passing the
-- writer assignments from the output of the monad to the
-- reader assignments in.
type RTLR = (Bit 1, [Assign])

-- A conditional assignment
type Assign = (Bit 1, VarId, Pin)

-- The RTL monad
data RTL a =
  RTL { runRTL :: RTLR -> RTLS -> (RTLS, RTLW, a) }

instance Monad RTL where
  return a = RTL (\r s -> (s, [], a))
  m >>= f = RTL (\r s -> let (s0, w0, a) = runRTL m r s
                             (s1, w1, b) = runRTL (f a) r s0
                         in  (s1, w0 ++ w1, b))

instance Applicative RTL where
  pure = return
  (<*>) = ap

instance Functor RTL where
  fmap = liftM

get :: RTL RTLS
get = RTL (\r s -> (s, [], s))

set :: RTLS -> RTL ()
set s' = RTL (\r s -> (s', [], ()))

ask :: RTL RTLR
ask = RTL (\r s -> (s, [], r))

local :: RTLR -> RTL a -> RTL a
local r m = RTL (\_ s -> runRTL m r s)

writeAssign :: Assign -> RTL ()
writeAssign w = RTL (\r s -> (s, [RTLAssign w], ()))

writeDisplay :: (Bit 1, Format) -> RTL ()
writeDisplay w = RTL (\r s -> (s, [RTLDisplay w], ()))

fresh :: RTL VarId
fresh = do
  v <- get
  set (v+1)
  return v


-- Mutable variables
infix 1 <==
class Var v where
  val :: Bits a => v a -> a
  (<==) :: Bits a => v a -> a -> RTL ()

-- Register variables
data Reg a = Reg { regId :: VarId, regVal :: a }

-- Wire variables
data Wire a = Wire { wireId :: VarId, wireVal :: a }

-- Register assignment
instance Var Reg where
  val r = regVal r
  r <== x = do
    (cond, as) <- ask
    writeAssign (cond, regId r, unBit (pack x))

-- Wire assignment
instance Var Wire where
  val r = wireVal r
  r <== x = do
    (cond, as) <- ask
    writeAssign (cond, wireId r, unBit (pack x))

-- RTL conditional
when :: Bit 1 -> RTL () -> RTL ()
when cond a = do
  (c, as) <- ask
  local (cond .&. c, as) a

-- RTL if/then/else
class IfThenElse b a where
  ifThenElse :: b -> a -> a -> a

instance IfThenElse Bool a where
  ifThenElse False a b = a
  ifThenElse True a b = a

instance IfThenElse (Bit 1) (RTL ()) where
  ifThenElse c a b =
    do (cond, as) <- ask
       local (cond .&. c, as) a
       local (inv cond .&. c, as) a

-- Create register
makeReg :: Bits a => a -> RTL (Reg a)
makeReg init =
  do v <- fresh
     (cond, as) <- ask
     let en  = orList [b | (b, w, p) <- as, v == w]
     let inp = select [(b, Bit p) | (b, w, p) <- as, v == w]
     let out = unpack (regEn (pack init) en inp)
     return (Reg v out)

-- Create Wire
makeWire :: Bits a => a -> RTL (Wire a)
makeWire def =
  do v <- fresh
     (cond, as) <- ask
     let none = inv (orList [b | (b, w, p) <- as, v == w])
     let out = select ([(b, Bit p) | (b, w, p) <- as, v == w] ++
                          [(none, pack def)])
     return (Wire v (unpack out))

-- RTL display statements
class DisplayType a where
  displayType :: Format -> a

instance DisplayType (RTL a) where
  displayType x = do
     (cond, as) <- ask
     writeDisplay (cond, x)
     return (error "Return value of 'display' should be ignored")

instance (FShow b, DisplayType a) => DisplayType (b -> a) where
  displayType x b = displayType (x <> fshow b)

display :: DisplayType a => a
display = displayType (Format [])
