{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Blarney.Core.RTL
Description : Register-transfer-level descriptions
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019-2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

The module defines the RTL monad, supporting:

1. Mutable wires, registers and register files.
2. Conditional statements.
3. Simulation-time I/O.
4. Module input and output declarations.
-}
module Blarney.Core.RTL (
  -- * RTL monad
  RTL             -- RTL monad (abstract)
, evalPureRTL
, evalRTLRoots
  -- * Conditional statements
, whenRTL         -- RTL conditional block (with return value)
, ifThenElseRTL   -- RTL if-then-else statement
, switch          -- RTL switch statement
, (-->)           -- Operator for switch statement alternatives
  -- * Block naming statements
, withNameHint    -- Set a name hint for an RTL block
  -- * Mutable variables: registers and wires
, Reg(..)         -- Registers
, writeReg        -- Write to register
, Wire(..)        -- Wires
, writeWire       -- Write to wire
, makeReg         -- Create register
, makeRegU        -- Create uninitialised regiseter
, makeDReg        -- Like makeReg, but holds value one cycle only
, makeWire        -- Create wire
, makeWireU       -- Create uninitialised wire
  -- * Simulation-time statements
, Displayable(..) -- To support N-ary display statement
, display         -- Display statement
, display_        -- Display statement (without newline)
, finish          -- Terminate simulator
  -- * Assertions
, assert          -- assertion of a predicate
  -- * External inputs and outputs
, input           -- Declare module input
, output          -- Declare module output
, inputBV         -- Declare module input (untyped)
, outputBV        -- Declare module output (untyped)
  -- * Register files
, RegFileRTL(..)  -- Register file interface
, makeRegFileInit -- Create initialised register file
, makeRegFile     -- Create uninitialised register file
  -- * Add netlist roots
, addRoots        -- Add netlist roots
) where

-- Blarney imports
import Blarney.Core.BV
import Blarney.Core.Bit
import Blarney.Core.Bits
import Blarney.Core.Prim hiding (nameHints)
import Blarney.Core.FShow
import Blarney.Core.Common
import Blarney.Core.IfThenElse
import qualified Blarney.Core.JList as JL

-- Standard imports
import Prelude
import Data.Maybe
import Data.IORef
import GHC.TypeLits
--import Data.Array.IO
import Data.Set (Set, empty, insert, singleton, toList)
import Control.Monad.Fix
import Control.Monad.Trans
import Data.Functor.Identity
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Data.List (intercalate)
import Control.Monad
import Data.Map (Map, findWithDefault, fromListWith)

-- helper types and functions
--------------------------------------------------------------------------------
-- | Variable identifiers
type VarId = Int

-- | RTL actions
data RTLAction = RTLAssign Assign
               | RTLRoot BV

-- | Conditional variable assignment
data Assign = Assign { enable :: Bit 1, lhs :: VarId, rhs :: BV }

-- | The right-hand-side of an assignment is untyped, but we can give it type
--   with the following function
rhsTyped :: Bits a => Assign -> a
rhsTyped = unpack . FromBV . rhs

-- | Add name hints to a BV and turn it in Bits
bvHintsUpdt b hints = unpack . FromBV $ addBVNameHints b hints

-- The RTL Monad and primitive interaction functions
--------------------------------------------------------------------------------
-- | The RTL monad, for register-transfer-level descriptions
type RTL = ReaderT RTL_R (StateT RTL_S (WriterT RTL_W Identity))

-- | run the 'RTL' monad with the given environment and initial state, and
--   return a tuple of the final state, accumulated RTLActions and return value
execRTL :: RTL a -> RTL_R -> RTL_S -> (RTL_S, RTL_W, a)
execRTL rtlAct env s0 = (s, w, x)
  where f = runIdentity . runWriterT
                        . (flip runStateT) s0
                        . (flip runReaderT) env
        ((x, s), w) = f rtlAct

evalPureRTL :: RTL a -> String -> a
evalPureRTL rtlAct errStr
  | null (JL.toList w) && s == 0 = x
  | otherwise = error errStr
  where (s, w, x) = execRTL rtlAct dfltRTLEnv 0

evalRTLRoots :: RTL a -> [BV]
evalRTLRoots rtl = [root | RTLRoot root <- acts]
  where acts = JL.toList actsJL
        (_, actsJL, _) = execRTL rtl (RTLEnv { rtlEnvNameHints = mempty
                                             , rtlEnvCondition = 1
                                             , rtlEnvAssigns   = assignMap }) 0
        assignMap = fromListWith (++) [(lhs a, [a]) | RTLAssign a <- acts]

-- | The reader component contains name hints, the current condition on any
--   RTL actions, and a list of all assigments in the computation (obtained
--   circularly from the writer output of the monad).
data RTLEnv = RTLEnv { rtlEnvNameHints :: NameHints
                     , rtlEnvCondition :: Bit 1
                     , rtlEnvAssigns   :: Map VarId [Assign] }

-- | A default RTL environment
dfltRTLEnv :: RTLEnv
dfltRTLEnv = RTLEnv { rtlEnvNameHints = mempty
                    , rtlEnvCondition = 0
                    , rtlEnvAssigns   = mempty }

-- | The reader component's local type
type RTL_R = RTLEnv

-- | The state component contains the next free variable identifier
type RTL_S = VarId

-- | The writer component accumulater the 'RTLAction's
type RTL_W = JL.JList RTLAction

-- | Get reader name hints
askNameHints :: RTL NameHints
askNameHints = asks rtlEnvNameHints

-- | Get reader condition
askCondition :: RTL (Bit 1)
askCondition = asks rtlEnvCondition

-- | Get reader assignments
askAssigns :: RTL (Map VarId [Assign])
askAssigns = asks rtlEnvAssigns

-- | Get the entire environment
askEnv :: RTL RTLEnv
askEnv = ask

-- | Execute computation in given environment
localEnv :: RTLEnv -> RTL a -> RTL a
localEnv env act = local (const env) act

-- | Get fresh variable id
freshVarId :: RTL VarId
freshVarId = do v <- lift get
                lift $ put (v + 1)
                return v

-- | Add action to writer component
addRTLAction :: RTLAction -> RTL ()
addRTLAction rtlAct = lift . lift $ tell (JL.One rtlAct)

-- | Add netlist roots
addRoots :: [BV] -> RTL ()
addRoots roots = mapM_ addRTLAction (map RTLRoot roots)

--------------------------------------------------------------------------------

-- | Set a name hint for an RTL block
withNameHint :: NameHint -> RTL a -> RTL a
withNameHint hint m = do
  r@RTLEnv{..} <- askEnv
  localEnv (r { rtlEnvNameHints  = insert hint rtlEnvNameHints }) m

-- | RTL conditional block with return value
whenRTL :: Bit 1 -> RTL a -> RTL a
whenRTL c a = do
  r@RTLEnv{..} <- askEnv
  localEnv (r { rtlEnvCondition = c .&. rtlEnvCondition }) a

-- | If-then-else statement for RTL
ifThenElseRTL :: Bits a => Bit 1 -> RTL a -> RTL a -> RTL a
ifThenElseRTL c a b = do
  r@RTLEnv{..} <- askEnv
  ra <- localEnv (r { rtlEnvCondition = rtlEnvCondition .&. c }) a
  rb <- localEnv (r { rtlEnvCondition = rtlEnvCondition .&. inv c }) b
  return $ c ? (ra, rb)

-- | RTL switch statement
switch :: Bits a => a -> [(a, RTL ())] -> RTL ()
switch subject alts = forM_ alts \(lhs, rhs) ->
  whenRTL (pack subject .==. pack lhs) rhs

-- | Operator for switch statement alternatives
infixl 0 -->
(-->) :: a -> RTL () -> (a, RTL ())
lhs --> rhs = (lhs, rhs)

-- | Terminate simulator
finish :: RTL ()
finish = do
  cond <- askCondition
  addRTLAction . RTLRoot $ makePrim0 Finish [toBV cond]

-- | Assert that a predicate holds
assert :: Bit 1 -> String -> RTL ()
assert pred msg = do
  cond <- askCondition
  addRTLAction . RTLRoot $ makePrim0 (Assert msg)
                                     [toBV cond, toBV $ pack pred]

-- | To support a display statement with variable number of arguments
class Displayable a where
  disp :: Format -> Format -> a

-- | Base case
instance Displayable (RTL a) where
  disp x suffix = do
    cond <- askCondition
    let Format items = x <> suffix
    let prim = Display (map fst items)
    let inps = toBV cond : [bv | (_, Just bv) <- items]
    addRTLAction . RTLRoot $ makePrim0 prim inps
    return $ error "Return value of 'display' should be ignored"

-- | Recursive case
instance (FShow b, Displayable a) => Displayable (b -> a) where
  disp x suffix b = disp (x <> fshow b) suffix

-- | Display statement
display :: Displayable a => a
display = disp (Format []) (fshow "\n")

-- | Display statement (without new line)
display_ :: Displayable a => a
display_ = disp (Format []) (Format [])

-- | RTL external input declaration
input :: KnownNat n => String -> RTL (Bit n)
input str = mdo x <- FromBV <$> inputBV str (widthOf x)
                return x

-- | RTL external input declaration (untyped)
inputBV :: String -> Width -> RTL BV
inputBV str w = do let bv = inputPinBV w str
                   addRTLAction $ RTLRoot bv
                   return bv

-- | RTL external output declaration
output :: String -> Bit n -> RTL ()
output str out = outputBV str (toBV out)

-- | RTL external output declaration (untyped)
outputBV :: String -> BV -> RTL ()
outputBV str bv =
  addRTLAction . RTLRoot $ makePrim0 (Output (bvPrimOutWidth bv) str) [bv]

--------------------------------------------------------------------------------

-- | Register variables
data Reg a = Reg { regId  :: VarId -- ^ Unique identifier
                 , regVal :: a     -- ^ Current register value
                 }

-- | Register assignment
writeReg :: Bits a => Reg a -> a -> RTL ()
writeReg v x = do
  cond <- askCondition
  addRTLAction . RTLAssign $ Assign { enable = cond
                                    , lhs = regId v
                                    , rhs = toBV (pack x) }

-- | Create register with initial value
makeReg :: Bits a => a -> RTL (Reg a)
makeReg init = do
  v <- freshVarId
  assigns <- findWithDefault [] v <$> askAssigns
  nameHints <- askNameHints
  let w = sizeOf init
      initBV = toBV $ pack init
      en = toBV $ orList (map enable assigns) -- Note: en == 0 on empty list
      inp = mergeWritesBV MStratOr w [(toBV $ enable a, rhs a) | a <- assigns]
      out = regEnBV w initBV en inp
  return $ Reg { regId = v, regVal = bvHintsUpdt out nameHints }

-- | Wire variables
data Wire a = Wire { wireId  :: VarId -- ^ Unique identifier
                   , wireVal :: a     -- ^ Current wire value
                   , active  :: Bit 1 -- ^ Is wire being assigned on this cycle?
                   }

-- | Wire assignment
writeWire :: Bits a => Wire a -> a -> RTL ()
writeWire v x = do
  cond <- askCondition
  addRTLAction . RTLAssign $ Assign { enable = cond
                                    , lhs = wireId v
                                    , rhs = toBV (pack x) }

-- | Create wire with default value
makeWire :: Bits a => a -> RTL (Wire a)
makeWire defaultVal = do
  v <- freshVarId
  assigns <- findWithDefault [] v <$> askAssigns
  nameHints <- askNameHints
  let w = sizeOf defaultVal
      any = toBV $ orList (map enable assigns)
      none = invBV any
      inPairs =   (none, toBV $ pack defaultVal)
                : [(toBV $ enable a, rhs a) | a <- assigns]
      out = mergeWritesBV MStratOr w inPairs
  return $ Wire { wireId  = v
                , wireVal = bvHintsUpdt out nameHints
                , active  = bvHintsUpdt any $ insert (NmSuffix 0 "act")
                                                     nameHints }

--------------------------------------------------------------------------------

-- | Create wire with don't care initial value
makeRegU :: Bits a => RTL (Reg a)
makeRegU = makeReg dontCare

-- | A DReg holds the assigned value only for one cycle. At all other times, it
--   has the given default value.
makeDReg :: Bits a => a -> RTL (Reg a)
makeDReg defaultVal = do
  -- Create wire with default value
  w :: Wire a <- makeWire defaultVal
  -- Register the output of the wire
  r :: Reg a <- makeReg defaultVal
  -- Always assign to the register
  writeReg r (wireVal w)
  -- Write to wire and read from reg
  return $ Reg { regId = wireId w, regVal = regVal r }

-- |Create wire with don't care default value
makeWireU :: Bits a => RTL (Wire a)
makeWireU = makeWire dontCare

--------------------------------------------------------------------------------

-- | Register file interface
data RegFileRTL a d = RegFileRTL { lookupRTL :: a -> d
                                 , updateRTL :: a -> d -> RTL () }

-- | Create register file with initial contents
makeRegFileInit :: forall a d. (Bits a, Bits d) =>
                   String -> RTL (RegFileRTL a d)
makeRegFileInit initFile = do
  -- Create regsiter file identifier
  v <- freshVarId
  -- Determine widths of address/data bus
  let aw = sizeOf (error "_|_" :: a)
  let dw = sizeOf (error "_|_" :: d)
  -- Record register file for netlist generation
  let rfinfo = RegFileInfo{ regFileId = v
                          , regFileInitFile = initFile
                          , regFileAddrWidth = aw
                          , regFileDataWidth = dw }
  addRTLAction . RTLRoot $ makePrim0 (RegFileMake rfinfo) []
  return $
    RegFileRTL {
      lookupRTL = \a ->
        unpack $ FromBV $ regFileReadBV rfinfo $ toBV (pack a)
    , updateRTL = \a d -> do
        cond <- askCondition
        let rootInps = [toBV cond, toBV (pack a), toBV (pack d)]
        addRTLAction . RTLRoot $ makePrim0 (RegFileWrite rfinfo) rootInps
    }

-- | Create uninitialised register file
makeRegFile :: forall a d. (Bits a, Bits d) => RTL (RegFileRTL a d)
makeRegFile = makeRegFileInit ""
