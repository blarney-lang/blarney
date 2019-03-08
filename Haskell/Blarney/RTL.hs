{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}

{-|
Module      : Blarney.RTL
Description : Register-transfer-level descriptions
Copyright   : (c) Matthew Naylor, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

The module defines the RTL monad, supporting:

1. Mutable wires, registers and register files.
2. Conditional statements.
3. Simulation-time I/O.
4. Module input and output declarations.
-}
module Blarney.RTL
  ( -- * RTL monad
    RTL             -- RTL monad (abstract)
    -- * Conditional statements
  , when            -- RTL conditional block
  , whenR           -- RTL conditional block (with return value)
  , ifThenElseRTL   -- RTL if-then-else statement
  , switch          -- RTL switch statement
  , (-->)           -- Operator for switch statement alternatives
    -- * Mutable variables: registers and wires
  , Reg(..)         -- Registers
  , writeReg        -- Write to register
  , Wire(..)        -- Wires
  , writeWire       -- Write to wire
  , makeReg         -- Create register
  , makeRegU        -- Create uninitialised regiseter
  , makeWire        -- Create wire
  , makeWireU       -- Create uninitialised wire
  , makeDReg        -- Like makeReg, but holds value one cycle only
    -- * Simulation-time statements
  , Displayable(..) -- To support N-ary display statement
  , display         -- Display statement
  , finish          -- Terminate simulator
    -- * External inputs and outputs
  , input           -- Declare module input
  , output          -- Declare module output
  , inputBV         -- Declare module input (untyped)
  , outputBV        -- Declare module output (untyped)
    -- * Register files
  , RegFileRTL(..)  -- Register file interface
  , makeRegFileInit -- Create initialised register file
  , makeRegFile     -- Create uninitialised register file
    -- * Convert RTL to a netlist
  , addRoots        -- Add netlist roots
  , netlist         -- Convert RTL monad to a netlist
  ) where

-- Blarney imports
import Blarney.BV
import Blarney.Bit
import Blarney.Bits
import Blarney.FShow
import Blarney.Prelude
import Blarney.IfThenElse
import qualified Blarney.JList as JL

-- Standard imports
import Prelude
import Data.IORef
import GHC.TypeLits
import Control.Monad.Fix
import Control.Monad hiding (when)
import Data.Map (Map, findWithDefault, fromListWith)

-- |The RTL monad, for register-transfer-level descriptions,
-- is a fairly standard reader-writer-state monad.
newtype RTL a =
  RTL { runRTL :: R -> S -> (S, W, a) }

-- |The writer component maintains a list of all RTL actions to be performed
type W = JL.JList RTLAction

-- |RTL actions
data RTLAction =
    RTLAssign Assign
  | RTLDisplay (Bit 1, Format)
  | RTLFinish (Bit 1)
  | RTLOutput (Width, String, BV)
  | RTLInput (Width, String)
  | RTLRegFileCreate (String, VarId, Width, Width)
  | RTLRegFileUpdate (VarId, Bit 1, Int, Int, BV, BV)
  | RTLRoots [BV]

-- |Variable identifiers
type VarId = Int

-- |Conditional variable assignment
data Assign = Assign { enable :: Bit 1, lhs :: VarId, rhs :: BV }

-- |The right-hand-side of an assignment is untyped,
-- but we can give it type with the following function
rhsTyped :: Bits a => Assign -> a
rhsTyped = unpack . FromBV . rhs

-- |The reader component contains the current condition on any
-- RTL actions, and a list of all assigments in the computation
-- (obtained circularly from the writer output of the monad).
data R = R { cond :: Bit 1, assigns :: Map VarId [Assign] }

-- |The state component contains the next free variable identifier
type S = VarId

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

instance MonadFix RTL where
  mfix f = RTL (\r s -> let (s', w, a) = runRTL (f a) r s in (s', w, a))

-- |Get state component
get :: RTL S
get = RTL (\r s -> (s, JL.Zero, s))

-- |Set state component
set :: S -> RTL ()
set s' = RTL (\r s -> (s', JL.Zero, ()))

-- |Get reader component
ask :: RTL R
ask = RTL (\r s -> (s, JL.Zero, r))

-- |Execute computation in given environment
local :: R -> RTL a -> RTL a
local r m = RTL (\_ s -> runRTL m r s)

-- |Add action to writer component
write :: RTLAction -> RTL ()
write rtl = RTL (\r s -> (s, JL.One rtl, ()))

-- |Get fresh variable id
fresh :: RTL VarId
fresh = do
  v <- get
  set (v+1)
  return v

-- |RTL conditional block
when :: Bit 1 -> RTL () -> RTL ()
when c a = do
  r <- ask
  local (r { cond = c .&. cond r }) a

-- |RTL conditional block with return value
whenR :: Bit 1 -> RTL a -> RTL a
whenR c a = do
  r <- ask
  local (r { cond = c .&. cond r }) a

-- |If-then-else statement for RTL
ifThenElseRTL :: Bit 1 -> RTL () -> RTL () -> RTL ()
ifThenElseRTL c a b =
  do r <- ask
     local (r { cond = cond r .&. c }) a
     local (r { cond = cond r .&. inv c }) b

-- |Overloaded if-then-else
instance IfThenElse (Bit 1) (RTL ()) where
  ifThenElse = ifThenElseRTL

-- |RTL switch statement
switch :: Bits a => a -> [(a, RTL ())] -> RTL ()
switch subject alts =
  forM_ alts $ \(lhs, rhs) ->
    when (pack subject .==. pack lhs) rhs

-- |Operator for switch statement alternatives
infixl 0 -->
(-->) :: a -> RTL () -> (a, RTL ())
lhs --> rhs = (lhs, rhs)

-- |Register variables
data Reg a =
  Reg {
    -- |Unique identifier
    regId  :: VarId
    -- |Current register value
  , regVal :: a
  }

-- |Register assignment
writeReg :: Bits a => Reg a -> a -> RTL ()
writeReg v x = do
  r <- ask
  write $ RTLAssign $
    Assign {
      enable = cond r
    , lhs = regId v
    , rhs = toBV (pack x)
    }

-- |Wire variables
data Wire a =
  Wire {
    -- |Unique identifier
    wireId  :: VarId
    -- |Current wire value
  , wireVal :: a
    -- |Is wire being assigned on this cycle?
  , active  :: Bit 1
  }

-- |Wire assignment
writeWire :: Bits a => Wire a -> a -> RTL ()
writeWire v x = do
  r <- ask
  write $ RTLAssign $
    Assign {
      enable = cond r
    , lhs = wireId v
    , rhs = toBV (pack x)
    }

-- |Create wire with don't care initial value
makeRegU :: Bits a => RTL (Reg a)
makeRegU = makeReg dontCare

-- |Create register with initial value
makeReg :: Bits a => a -> RTL (Reg a)
makeReg init =
  do v <- fresh
     r <- ask
     let as = findWithDefault [] v (assigns r)
     let en = orList (map enable as)
     let inp = case as of
                 [a] -> rhsTyped a
                 other -> select [(enable a, rhsTyped a) | a <- as]
     let out = delayEn init en inp
     return (Reg { regId = v, regVal = out })

-- |Create wire with default value
makeWire :: Bits a => a -> RTL (Wire a)
makeWire defaultVal =
  do v <- fresh
     r <- ask
     let as = findWithDefault [] v (assigns r)
     let any = orList (map enable as)
     let none = inv any
     let out = select ([(enable a, rhsTyped a) | a <- as]
                         ++ [(none, defaultVal)])
     return $
       Wire {
         wireId  = v
       , wireVal = out
       , active  = any
       }

-- |Create wire with don't care default value
makeWireU :: Bits a => RTL (Wire a)
makeWireU = makeWire dontCare

-- |A DReg holds the assigned value only for one cycle.
-- At all other times, it has the given default value.
makeDReg :: Bits a => a -> RTL (Reg a)
makeDReg defaultVal = do
  -- Create wire with default value
  w :: Wire a <- makeWire defaultVal

  -- Register the output of the wire
  r :: Reg a <- makeReg defaultVal

  -- Always assign to the register
  writeReg r (wireVal w)

  -- Write to wire and read from reg
  return (Reg { regId = wireId w, regVal = regVal r })

-- |Terminate simulator
finish :: RTL ()
finish = do
  r <- ask
  write (RTLFinish (cond r))

-- |To support a display statement with variable number of arguments
class Displayable a where
  disp :: Format -> a

-- |Base case
instance Displayable (RTL a) where
  disp x = do
     r <- ask
     write (RTLDisplay (cond r, x))
     return (error "Return value of 'display' should be ignored")

-- |Recursive case
instance (FShow b, Displayable a) => Displayable (b -> a) where
  disp x b = disp (x <> fshow b)

-- |Display statement
display :: Displayable a => a
display = disp (Format [])

-- |RTL external input declaration
input :: KnownNat n => String -> RTL (Bit n)
input str =
    do write (RTLInput (w, str))
       return inp
  where
    inp = FromBV (inputPinBV w str)
    w = widthOf inp

-- |RTL external input declaration (untyped)
inputBV :: String -> Width -> RTL BV
inputBV str w =
    do write (RTLInput (w, str))
       return (inputPinBV w str)

-- |RTL external output declaration
output :: String -> Bit n -> RTL ()
output str out = do
  let bv = toBV out
  write (RTLOutput (bvWidth bv, str, bv))

-- |RTL external output declaration (untyped)
outputBV :: String -> BV -> RTL ()
outputBV str bv = write (RTLOutput (bvWidth bv, str, bv))

-- Register files
-- ==============

-- |Register file interface
data RegFileRTL a d =
  RegFileRTL {
    lookupRTL :: a -> d
  , updateRTL :: a -> d -> RTL ()
  }

-- | Create register file with initial contents
makeRegFileInit :: forall a d. (Bits a, Bits d) =>
                     String -> RTL (RegFileRTL a d)
makeRegFileInit initFile = do
  -- Create regsiter file identifier
  id <- fresh

  -- Determine widths of address/data bus
  let aw = sizeOf (error "_|_" :: a)
  let dw = sizeOf (error "_|_" :: d)

  -- Record register file for netlist generation
  write $ RTLRegFileCreate (initFile, id, aw, dw)

  return $
    RegFileRTL {
      lookupRTL = \a ->
        unpack $ FromBV $ regFileReadBV id dw $ toBV (pack a)
    , updateRTL = \a d -> do
        r <- ask
        write $
          RTLRegFileUpdate (id, cond r, aw, dw, toBV (pack a), toBV (pack d))
    }

-- |Create uninitialised register file
makeRegFile :: forall a d. (Bits a, Bits d) => RTL (RegFileRTL a d)
makeRegFile = makeRegFileInit ""

-- Netlist generation
-- ==================

-- Add display primitive to netlist
addDisplayPrim :: (Bit 1, [FormatItem]) -> Flatten ()
addDisplayPrim (cond, items) = do
    c <- flatten (toBV cond)
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
  c <- flatten (toBV cond)
  id <- freshId
  let net = Net {
                netPrim = Finish
              , netInstId = id
              , netInputs = [c]
              , netOutputWidths = []
            }
  addNet net

-- Add output primitive to netlist
addOutputPrim :: (Width, String, BV) -> Flatten ()
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

-- Add input primitive to netlist
addInputPrim :: (Width, String) -> Flatten ()
addInputPrim (w, str) = do
  id <- freshId
  let net = Net {
                netPrim = Input w str
              , netInstId = id
              , netInputs = []
              , netOutputWidths = [w]
            }
  addNet net

-- Add RegFile primitives to netlist
addRegFilePrim :: (String, VarId, Width, Width) -> Flatten ()
addRegFilePrim (initFile, regFileId, aw, dw) = do
  id <- freshId
  let net = Net {
                netPrim = RegFileMake initFile aw dw regFileId
              , netInstId = id
              , netInputs = []
              , netOutputWidths = []
            }
  addNet net

-- Add RegFile primitives to netlist
addRegFileUpdatePrim :: (VarId, Bit 1, Int, Int, BV, BV) -> Flatten ()
addRegFileUpdatePrim (regFileId, c, aw, dw, a, d) = do
  cf <- flatten (toBV c)
  af <- flatten a
  df <- flatten d
  id <- freshId
  let net = Net {
                netPrim = RegFileWrite aw dw regFileId
              , netInstId = id
              , netInputs = [cf, af, df]
              , netOutputWidths = []
            }
  addNet net

-- |Add netlist roots
addRoots :: [BV] -> RTL ()
addRoots roots = write (RTLRoots roots)

-- |Convert RTL monad to a netlist
netlist :: RTL () -> IO [Net]
netlist rtl = do
  i <- newIORef (0 :: Int)
  (nl, _) <- runFlatten roots i
  return (JL.toList nl)
  where
    (_, actsJL, _) = runRTL rtl (R { cond = 1, assigns = assignMap }) 0
    acts = JL.toList actsJL
    assignMap = fromListWith (++) [(lhs a, [a]) | RTLAssign a <- acts]
    disps = reverse [(go, items) | RTLDisplay (go, Format items) <- acts]
    fins  = [go | RTLFinish go <- acts]
    outs  = [out | RTLOutput out <- acts]
    inps  = [out | RTLInput out <- acts]
    rfs   = [out | RTLRegFileCreate out <- acts]
    rfus  = [out | RTLRegFileUpdate out <- acts]
    rts   = concat [roots | RTLRoots roots <- acts]
    roots = do mapM_ addDisplayPrim disps
               mapM_ addFinishPrim fins
               mapM_ addOutputPrim outs
               mapM_ addInputPrim inps
               mapM_ addRegFilePrim rfs
               mapM_ addRegFileUpdatePrim rfus
               mapM_ flatten rts
