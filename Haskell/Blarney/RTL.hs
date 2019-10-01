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
    -- * Block naming statements
  , withNewName     -- Set name for RTL block
  , withExtendedName-- Extends current name for RTL block
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
import Data.Array
import Data.Array.IO
import Data.Maybe

-- For name hints
import Data.Set (Set, empty, singleton, insert, toList, union)
import Data.List (intercalate)

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
data R = R { nameHints :: Set String
           , cond      :: Bit 1
           , assigns   :: Map VarId [Assign]
           }

-- |The state component contains the next free variable identifier
type S = VarId

instance Monad RTL where
  return a = RTL (\r s -> (s, JL.Zero, a))
  m >>= f = RTL (\r s -> let (s0, w0, a) = runRTL m r s
                             (s1, w1, b) = runRTL (f a) r s0
                         in  (s1, w0 JL.++ w1, b))

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

-- | RTL named block
withNewName :: String -> RTL a -> RTL a
withNewName nm m = do
  r <- ask
  local (r { nameHints = singleton nm }) m

-- | RTL extended named block
withExtendedName :: String -> RTL a -> RTL a
withExtendedName nm m = do
  r <- ask
  local (r { nameHints = insert nm (nameHints r) }) m

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
     let name = intercalate "_" (toList (nameHints r))
     let newVal = if null (nameHints r) then out else nameBits name out
     return (Reg { regId = v, regVal = newVal })

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
     let name = intercalate "_" (toList (nameHints r))
     let newVal = if null (nameHints r) then out else nameBits name out
     return $
       Wire {
         wireId  = v
       , wireVal = newVal
       , active  = nameBits (name ++ "_act") any
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

-- | Finalise 'Name's in netlist to "v_"
finaliseNames :: IOArray InstId (Maybe Net) -> IO (IOArray InstId (Maybe Net))
finaliseNames = mapArray inner
  where inner (Just net) = Just net { netInputs = map f (netInputs net)
                                    , netName   = Final "v"
                                    }
        inner Nothing = Nothing
        f (InputWire (i, n, _)) = InputWire (i, n, Final "v")
        f i = i

-- | Finalise names in Netlist preserving name hints
--finaliseNames :: IOArray InstId (Maybe Net) -> IO (IOArray InstId (Maybe Net))
--finaliseNames arr = do
--  bounds <- getBounds arr
--  names :: IOArray InstId (Set String) <- newArray bounds empty
--  pairs <- getAssocs arr
--  -- accumulate names for each Net
--  forM_ [(a,b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
--    -- update names with current netName
--    updt names idx (netName net)
--    -- update names with current net's input wires
--    forM_ [wId | x@(InputWire wId) <- netInputs net] $ \(i, n, nm) -> do
--      updt names i nm
--  -- fold names back into Netlist
--  forM_ [(a,b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
--    -- prepare new netInputs
--    netInputs' <- forM (netInputs net) $ \inpt -> do
--      case inpt of
--        InputWire (i, n, _) -> do nm <- liftM genName (readArray names i)
--                                  return $ InputWire (i, n, Final nm)
--        x -> return x
--    -- prepare new netName
--    netName' <- liftM genName (readArray names idx)
--    -- update the current net
--    writeArray arr idx (Just net { netInputs = netInputs'
--                                 , netName   = Final netName'
--                                 })
--  -- return mutated netlist
--  return arr
--  -- inner helpers
--  where updt names i nm = do old <- readArray names i
--                             case nm of
--                               Hints nms -> writeArray names i (union nms old)
--                               Final nm  -> writeArray names i (insert nm old)
--        genName hints = if null hints then "v"
--                        else intercalate "_" (toList hints)

-- | Constant folding pass
foldConstants :: IOArray InstId (Maybe Net) -> IO (IOArray InstId (Maybe Net))
foldConstants = mapArray (fmap evalConstNet)

-- |Convert RTL monad to a netlist
netlist :: RTL () -> IO [Net]
netlist rtl = do
  i <- newIORef (0 :: InstId)
  ((nl, undo), _) <- runFlatten roots i
  maxId <- readIORef i
  let netlist = listArray (0, maxId) (replicate (maxId+1) Nothing)
                // [(netInstId n, Just n) | n <- JL.toList nl]
  netlist' <- thaw netlist
  netlist'' <- finaliseNames netlist'
  --netlist''' <- foldConstants netlist''
  undo
  nl' <- getElems netlist''
  --nl' <- getElems netlist'''
  return $ catMaybes nl'
  where
    (_, actsJL, _) = runRTL rtl (R { nameHints = empty
                                   , cond = 1
                                   , assigns = assignMap }) 0
    acts = JL.toList actsJL
    assignMap = fromListWith (++) [(lhs a, [a]) | RTLAssign a <- acts]
    roots = mapM_ flatten (concat [roots | RTLRoots roots <- acts])
