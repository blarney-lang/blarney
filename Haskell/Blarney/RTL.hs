-- For overriding if/then/else
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators,
      TypeFamilies, RebindableSyntax, MultiParamTypeClasses,
        FlexibleContexts, ScopedTypeVariables, FlexibleInstances #-}

module Blarney.RTL where

import Prelude
import Blarney.Bit
import Blarney.Bits
import Blarney.Unbit
import Blarney.Prelude
import Blarney.Format
import qualified Blarney.JList as JL
import Control.Monad
import GHC.TypeLits
import Data.IORef
import Data.IntMap (IntMap, findWithDefault, fromListWith)

-- Each RTL variable has a unique id
type VarId = Int

-- The RTL monad is a reader/writer/state monad
-- The state component is the next unique variable id
type RTLS = VarId

-- The writer component is a list of RTL actions
type RTLW = JL.JList RTLAction

-- RTL actions
data RTLAction =
    RTLAssign Assign
  | RTLDisplay (Bit 1, Format)
  | RTLFinish (Bit 1)
  | RTLOutput (Width, String, Unbit)

-- The reader component is a bit defining the current condition and a
-- list of all assigments made in the RTL block.  The list of
-- assignments is obtained by circular programming, passing the
-- writer assignments from the output of the monad to the
-- reader assignments in.
type RTLR = (Bit 1, IntMap [Assign])

-- A conditional assignment
type Assign = (Bit 1, VarId, Unbit)

-- The RTL monad
newtype RTL a =
  RTL { runRTL :: RTLR -> RTLS -> (RTLS, RTLW, a) }

instance Monad RTL where
  return a = RTL (\r s -> (s, JL.Zero, a))
  m >>= f = RTL (\r s -> let (s0, w0, a) = runRTL m r s
                             (s1, w1, b) = runRTL (f a) r s0
                         in  (s1, w1 JL.++ w0, b))

instance Applicative RTL where
  pure = return
  (<*>) = ap

instance Functor RTL where
  fmap = liftM

get :: RTL RTLS
get = RTL (\r s -> (s, JL.Zero, s))

set :: RTLS -> RTL ()
set s' = RTL (\r s -> (s', JL.Zero, ()))

ask :: RTL RTLR
ask = RTL (\r s -> (s, JL.Zero, r))

local :: RTLR -> RTL a -> RTL a
local r m = RTL (\_ s -> runRTL m r s)

writeAssign :: Assign -> RTL ()
writeAssign w = RTL (\r s -> (s, JL.One (RTLAssign w), ()))

writeDisplay :: (Bit 1, Format) -> RTL ()
writeDisplay w = RTL (\r s -> (s, JL.One (RTLDisplay w), ()))

writeFinish :: Bit 1 -> RTL ()
writeFinish w = RTL (\r s -> (s, JL.One (RTLFinish w), ()))

writeOutput :: (Width, String, Unbit) -> RTL ()
writeOutput w =
  RTL (\r s -> (s, JL.One (RTLOutput w), ()))

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
    writeAssign (cond, regId r, unbit (pack x))

-- Wire assignment
instance Var Wire where
  val r = wireVal r
  r <== x = do
    (cond, as) <- ask
    writeAssign (cond, wireId r, unbit (pack x))

-- RTL conditional
when :: Bit 1 -> RTL () -> RTL ()
when cond a = do
  (c, as) <- ask
  local (cond .&. c, as) a

-- RTL if/then/else
class IfThenElse b a where
  ifThenElse :: b -> a -> a -> a

instance IfThenElse Bool a where
  ifThenElse False a b = b
  ifThenElse True a b = a

instance IfThenElse (Bit 1) (RTL ()) where
  ifThenElse c a b =
    do (cond, as) <- ask
       local (cond .&. c, as) a
       local (inv cond .&. c, as) a

-- Create register with initial value
makeRegInit :: Bits a => a -> RTL (Reg a)
makeRegInit init =
  do v <- fresh
     (cond, assignMap) <- ask
     let as = findWithDefault [] v assignMap
     let en = orList [b | (b, _, p) <- as]
     let w = unbitWidth (unbit (pack init))
     let bit w p = Bit (p { unbitWidth = w })
     let inp = case as of
                 [(b, _, p)] -> bit w p
                 other -> select [(b, bit w p) | (b, _, p) <- as]
     let out = unpack (regEn (pack init) en inp)
     return (Reg v out)

-- Create register
makeReg :: Bits a => RTL (Reg a)
makeReg = makeRegInit (unpack 0)

-- Create wire with given default
makeWireDefault :: Bits a => a -> RTL (Wire a)
makeWireDefault def =
  do v <- fresh
     (cond, assignMap) <- ask
     let w = unbitWidth (unbit (pack def))
     let bit w p = Bit (p { unbitWidth = w })
     let as = findWithDefault [] v assignMap
     let none = inv (orList [b | (b, _, p) <- as])
     let out = select ([(b, bit w p) | (b, _, p) <- as] ++
                          [(none, pack def)])
     return (Wire v (unpack out))

-- Create wire
makeWire :: Bits a => RTL (Wire a)
makeWire = makeWireDefault (unpack 0)

-- RTL finish statements
finish :: RTL ()
finish = do
  (cond, as) <- ask
  writeFinish cond

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

-- RTL output statements
output :: String -> Bit n -> RTL ()
output str v = do
  let u = unbit v
  writeOutput (unbitWidth u, str, u)

-- Add display primitive to netlist
addDisplayPrim :: (Bit 1, [FormatItem]) -> Flatten ()
addDisplayPrim (cond, items) = do
    c <- flatten (unbit cond)
    ins <- mapM flatten [b | FormatBit w b <- items]
    id <- freshId
    let net = Net {
                  netPrim = Display args
                , netInstId = id
                , netInputs = c:ins
                , netOutputWidths = []
              }
    addNet net
  where
    args = map toDisplayArg items
    toDisplayArg (FormatString s) = DisplayArgString s
    toDisplayArg (FormatBit w b) = DisplayArgBit w

-- Add finish primitive to netlist
addFinishPrim :: Bit 1 -> Flatten ()
addFinishPrim cond = do
  c <- flatten (unbit cond)
  id <- freshId
  let net = Net {
                netPrim = Finish
              , netInstId = id
              , netInputs = [c]
              , netOutputWidths = []
            }
  addNet net

-- Add output primitive to netlist
addOutputPrim :: (Width, String, Unbit) -> Flatten ()
addOutputPrim (w, str, value) = do
  c <- flatten value
  id <- freshId
  let net = Net {
                netPrim = Output w str
              , netInstId = id
              , netInputs = [c]
              , netOutputWidths = []
            }
  addNet net

-- Convert RTL monad to a netlist
netlist :: RTL () -> IO [Net]
netlist rtl = do
  i <- newIORef (0 :: Int)
  (nl, _) <- runFlatten roots i
  return (JL.toList nl)
  where
    (_, actsJL, _) = runRTL rtl (1, assignMap) 0
    acts = JL.toList actsJL
    assignMap = fromListWith (++) [(v, [a]) | RTLAssign a@(_, v,_) <- acts]
    disps = [(go, items) | RTLDisplay (go, Format items) <- acts]
    fins  = [go | RTLFinish go <- acts]
    outs  = [out | RTLOutput out <- acts]
    roots = do mapM_ addDisplayPrim disps
               mapM_ addFinishPrim fins
               mapM_ addOutputPrim outs
