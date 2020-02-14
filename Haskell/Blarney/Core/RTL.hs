{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Blarney.Core.RTL
Description : Register-transfer-level descriptions
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

The module defines the RTL monad, supporting:

1. Mutable wires, registers and register files.
2. Conditional statements.
3. Simulation-time I/O.
4. Module input and output declarations.
-}
module Blarney.Core.RTL
  ( -- * RTL monad
    RTL             -- RTL monad (abstract)
    -- * Conditional statements
  , when            -- RTL conditional block
  , whenR           -- RTL conditional block (with return value)
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
  , makeWire        -- Create wire
  , makeWireU       -- Create uninitialised wire
  , makeDReg        -- Like makeReg, but holds value one cycle only
    -- * Simulation-time statements
  , Displayable(..) -- To support N-ary display statement
  , display         -- Display statement
  , display_        -- Display statement (without newline)
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
import Blarney.Core.BV
import Blarney.Core.Net
import Blarney.Core.Bit
import Blarney.Core.Bits
import Blarney.Core.Prim hiding (nameHints)
import Blarney.Core.FShow
import Blarney.Core.Prelude
import Blarney.Core.Flatten
import Blarney.Core.IfThenElse
import qualified Blarney.Core.JList as JL

-- Standard imports
import Prelude
import Data.Array
import Data.Maybe
import Data.IORef
import GHC.TypeLits
import Data.Array.IO
import Data.Set (Set, empty, insert, singleton, toList)
import Control.Monad.Fix
import Data.List (intercalate)
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
  | RTLRoots [BV]

-- |Variable identifiers
type VarId = Int

-- |Conditional variable assignment
data Assign = Assign { enable :: Bit 1, lhs :: VarId, rhs :: BV }

-- |The right-hand-side of an assignment is untyped,
-- but we can give it type with the following function
rhsTyped :: Bits a => Assign -> a
rhsTyped = unpack . FromBV . rhs

-- |The reader component contains name hints, the current condition on any
-- RTL actions, and a list of all assigments in the computation
-- (obtained circularly from the writer output of the monad).
data R = R { nameHints :: NameHints
           , cond      :: Bit 1
           , assigns   :: Map VarId [Assign]
           }

-- |The state component contains the next free variable identifier
type S = VarId

instance Monad RTL where
  return a = RTL (\r s -> (s, mempty, a))
  m >>= f = RTL (\r s -> let (s0, w0, a) = runRTL m r s
                             (s1, w1, b) = runRTL (f a) r s0
                         in  (s1, w0 <> w1, b))

instance Applicative RTL where
  pure = return
  (<*>) = ap

instance Functor RTL where
  fmap = liftM

instance MonadFix RTL where
  mfix f = RTL (\r s -> let (s', w, a) = runRTL (f a) r s in (s', w, a))

-- |Get state component
get :: RTL S
get = RTL (\r s -> (s, mempty, s))

-- |Set state component
set :: S -> RTL ()
set s' = RTL (\r s -> (s', mempty, ()))

-- |Get reader component
ask :: RTL R
ask = RTL (\r s -> (s, mempty, r))

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

-- | Set a name hint for an RTL block
withNameHint :: NameHint -> RTL a -> RTL a
withNameHint hint m = do
  r@R{ nameHints = hints } <- ask
  local (r { nameHints = insert hint hints }) m

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

-- | local helper to add name hints to underlying BV for a value in Bits
bvHintsUpdt b hints = unpack (FromBV $ addBVNameHints (toBV $ pack b) hints)

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
     return (Reg { regId = v, regVal = bvHintsUpdt out (nameHints r) })

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
       , wireVal = bvHintsUpdt out (nameHints r)
       , active  = bvHintsUpdt any $ insert (NmSuffix 0 "act") (nameHints r)
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
  let root = makePrimRoot Finish [toBV (cond r)]
  write (RTLRoots [root])

-- |To support a display statement with variable number of arguments
class Displayable a where
  disp :: Format -> Format -> a

-- |Base case
instance Displayable (RTL a) where
  disp x suffix = do
      r <- ask
      let Format items = x <> suffix
      let prim = Display (map toDisplayArg items)
      let inps = toBV (cond r) : [b | FormatBit w b <- items]
      write (RTLRoots [makePrimRoot prim inps])
      return (error "Return value of 'display' should be ignored")
    where
      toDisplayArg (FormatString s) = DisplayArgString s
      toDisplayArg (FormatBit w b) = DisplayArgBit w

-- |Recursive case
instance (FShow b, Displayable a) => Displayable (b -> a) where
  disp x suffix b = disp (x <> fshow b) suffix

-- |Display statement
display :: Displayable a => a
display = disp (Format []) (fshow "\n")

-- |Display statement (without new line)
display_ :: Displayable a => a
display_ = disp (Format []) (Format [])

-- |RTL external input declaration
input :: KnownNat n => String -> RTL (Bit n)
input str = mdo
  x <- FromBV <$> inputBV str (widthOf x)
  return x

-- |RTL external input declaration (untyped)
inputBV :: String -> Width -> RTL BV
inputBV str w = do
  let bv = inputPinBV w str
  write (RTLRoots [bv])
  return bv

-- |RTL external output declaration
output :: String -> Bit n -> RTL ()
output str out = outputBV str (toBV out)

-- |RTL external output declaration (untyped)
outputBV :: String -> BV -> RTL ()
outputBV str bv = do
  let root = makePrimRoot (Output (bvWidth bv) str) [bv]
  write (RTLRoots [root])

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
  let root = makePrimRoot (RegFileMake initFile aw dw id) []
  write (RTLRoots [root])

  return $
    RegFileRTL {
      lookupRTL = \a ->
        unpack $ FromBV $ regFileReadBV id dw $ toBV (pack a)
    , updateRTL = \a d -> do
        r <- ask
        let rootInps = [toBV (cond r), toBV (pack a), toBV (pack d)]
        let root = makePrimRoot (RegFileWrite aw dw id) rootInps
        write (RTLRoots [root])
    }

-- |Create uninitialised register file
makeRegFile :: forall a d. (Bits a, Bits d) => RTL (RegFileRTL a d)
makeRegFile = makeRegFileInit ""

-- Netlist generation
-- ==================

-- |Add netlist roots
addRoots :: [BV] -> RTL ()
addRoots roots = write (RTLRoots roots)

-- | propagate names through the Netlist
propagateNames :: MNetlist -> [WireId] -> IO ()
propagateNames nl roots = do
  bounds <- getBounds nl
  visited :: IOUArray InstId Bool <- newArray bounds False
  -- Push destination name down through netlist
  let visit destName (instId, _) = do
      isVisited <- readArray visited instId
      if not isVisited then do
        net@Net{ netPrim = prim
               , netInputs = inpts
               , netNameHints = hints
               } <- readNet nl instId
        let inpts' = [w | x@(InputWire w) <- inpts]
        writeArray visited instId True
        -- Detect new destination and update destination name in recursive call
        if isDest prim then mapM_ (visit $ bestName net) inpts'
        else do
          let newHints = insert (NmSuffix 10 destName) hints
          writeArray nl instId $ Just net{netNameHints = newHints}
          mapM_ (visit destName) inpts'
      else return ()
  --
  forM_ roots $ \root@(instId, _) -> do
    net <- readNet nl instId
    visit (bestName net) root
  --
  where bestName Net{ netPrim = prim
                    , netNameHints = hints
                    , netInstId = instId
                    } = "DEST_" ++ nm
                        where nm = if null nms
                                   then primStr prim ++ "_id" ++ show instId
                                   else head nms
                              nms = [y | x@(NmRoot _ y) <- toList hints]
        --
        isDest Register{}     = True
        isDest RegisterEn{}   = True
        isDest BRAM{}         = True
        isDest TrueDualBRAM{} = True
        isDest Custom{}       = True
        isDest Input{}        = True
        isDest Output{}       = True
        isDest Display{}      = True
        isDest Finish         = True
        isDest TestPlusArgs{} = True
        isDest RegFileMake{}  = True
        isDest RegFileRead{}  = True
        isDest RegFileWrite{} = True
        isDest _ = False

-- |Convert RTL monad to a netlist
netlist :: RTL () -> IO Netlist
netlist rtl = do
  -- flatten BVs into a Netlist
  i <- newIORef (0 :: InstId)
  ((nl, nms, undo), roots) <- runFlatten flattenRoots i
  maxId <- readIORef i
  mnl :: MNetlist <-
    thaw $ listArray (0, maxId) (replicate (maxId+1) Nothing)
        // [(netInstId n, Just n) | n <- JL.toList nl]
  -- update netlist with gathered names
  forM_ (JL.toList nms) $ \(idx, hints) -> do
    mnet <- readArray mnl idx
    case mnet of
      Just net@Net{ netNameHints = oldHints } ->
        writeArray mnl idx (Just net { netNameHints = oldHints <> hints })
      _ -> return ()
  -- propagates existing names through the netlist
  --propagateNames mnl [x | InputWire x <- roots]
  -- run optimisation netlist passes
  nl' <- freeze mnl -- netlistPasses mnl
  -- run undo computations
  undo
  -- return final netlist
  return nl'
  ------------------------
  where
    (_, actsJL, _) = runRTL rtl (R { nameHints = empty
                                   , cond = 1
                                   , assigns = assignMap }) 0
    acts = JL.toList actsJL
    assignMap = fromListWith (++) [(lhs a, [a]) | RTLAssign a <- acts]
    flattenRoots = mapM flatten (concat [rts | RTLRoots rts <- acts])
