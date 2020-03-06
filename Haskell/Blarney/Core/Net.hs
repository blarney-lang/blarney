{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Blarney.Core.Net
Description : Net primitive for Netlist construction
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

This module provides types and functions to represent circuits as 'Netlist's
that can then be rendered as Verilog or in other formats...

-}

module Blarney.Core.Net (
  Net(..)        -- 'Net' type to represent 'Netlist' nodes
, Netlist        -- 'Netlist' type to represent a circuit
, NetInput(..)   -- 'NetInput' type to represent inputs to 'Net's
, MNetlist       -- 'MNetlist' type, mutable netlist
, readNet        -- Helper function to read a 'Net' out of an 'MNetlist'
, WireId         -- 'WireId' type to uniquely identify wires
, netlistPasses  -- Toplevel function for 'Netlist' transformation passes
) where

import Prelude
import Data.Maybe
import Data.IORef
import Data.Array
import Data.Array.IO
import Control.Monad
import Data.List (intercalate)
import qualified Data.Bits as B

import Blarney.Core.Prim
import Blarney.Core.IfThenElse

-- General type definitions and helpers
--------------------------------------------------------------------------------

-- | 'Net' type representing a 'Netlist' node
data Net = Net { -- | The 'Net' 's 'Prim'itive
                 netPrim         :: Prim
                 -- | The 'Net' 's 'InstId' identifier
               , netInstId       :: InstId
                 -- | Is the 'Net' a root of the Netlist
               , netIsRoot       :: Bool
                 -- | The 'Net' 's list of 'NetInput' inputs
               , netInputs       :: [NetInput]
                 -- | The 'Net' 's list of 'Width' output widths
               , netOutputWidths :: [Width]
                 -- | The 'Net' 's 'NameHints'
               , netNameHints    :: NameHints
               } deriving Show

-- | A 'WireId' uniquely identify a wire with a 'Net''s instance identifier
--   ('InstId') and an output number ('OutputNumber')
type WireId = (InstId, OutputNumber)

-- | A 'Net''s input ('NetInput') can be:
--   - a wire, using the 'InputWire' constructor
--   - a complex expression, using the 'InputTree' constructor
data NetInput = InputWire WireId
              | InputTree Prim [NetInput]
              deriving Show

-- | A 'Netlist', represented as an 'Array InstId (Maybe Net)'
type Netlist = Array InstId (Maybe Net)

-- | A helper type for mutable 'Netlist'
type MNetlist = IOArray InstId (Maybe Net)

-- | A helper function to read a 'Net' from a 'MNetlist'
readNet :: MNetlist -> InstId -> IO Net
readNet nl instId = fromMaybe (error "encountered InstId with no matching Net")
                              <$> (readArray nl instId)

-- | A helper type for 'Net' reference counting
type NetCounts = IOUArray InstId Int

-- | A 'Net' reference counting helper function
countNetRef :: MNetlist -> IO NetCounts
countNetRef arr = do
  bounds <- getBounds arr
  refCounts <- newArray bounds 0
  es <- getElems arr
  -- count references for each Net
  let innerCount (InputWire (instId, _)) = do
        cnt <- readArray refCounts instId
        writeArray refCounts instId (cnt + 1)
      innerCount (InputTree _ inpts) = mapM_ innerCount inpts
  forM_ [e | Just e <- es] $ \net -> mapM_ innerCount (netInputs net)
  -- return reference counts
  return refCounts

-- | Repeat computation until a predicate holds
untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM pred act =
  act >>= \x -> if pred x then return x else untilM pred act

-- Netlist transformation passes
--------------------------------------------------------------------------------

-- pattern helper to identify constant InputTree NetInputs
pattern Lit i <- InputTree (Const _ i) []
-- | Helper to evaluate constant Net
evalConstNet :: Net -> (Net, Bool)
evalConstNet n@Net{ netPrim = Add w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 + a1), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Sub w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 - a1), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Mul w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 * a1), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Div w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 `div` a1), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Mod w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 `mod` a1), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Not w, netInputs = [Lit a0] } =
  (n { netPrim = Const w ((2^w-1) `B.xor` a0), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = And w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 B..&. a1), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Or w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 B..|. a1), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Xor w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 `B.xor` a1), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = ShiftLeft w, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const w (a0 `B.shiftL` fromInteger (a1))
     , netInputs = [] }, True)
-- TODO XXX FORCE UNSIGNED FOR NON ARITHMETIC SHIFTS
--ev Net{ netPrim = ShiftRight w, netInputs = [a0, a1] } =
--  n { netPrim   = Const w (inV a0 `B.shiftR` fromInteger (inV a1))
--    , netInputs = [] }
--ev Net{ netPrim = ArithShiftRight w, netInputs = [a0, a1] } =
--  n { netPrim   = Const w (inV a0 `B.shiftR` fromInteger (inV a1))
--    , netInputs = [] }
-- TODO XXX FORCE UNSIGNED FOR NON ARITHMETIC SHIFTS
evalConstNet n@Net{ netPrim = Equal _, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const 1 (if a0 == a1 then 1 else 0), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = NotEqual _, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const 1 (if a0 /= a1 then 1 else 0), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = LessThan _, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const 1 (if a0 < a1 then 1 else 0), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = LessThanEq _, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const 1 (if a0 <= a1 then 1 else 0), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = ReplicateBit w, netInputs = [Lit a0] } =
  (n { netPrim = Const w (if a0 == 1 then 2^w-1 else 0), netInputs = [] }, True)
-- TODO XXX
--ev Net{ netPrim = ZeroExtend _ w1, netInputs = [a0] } =
--  n { netPrim   = Const w1 (if inV a0 > 0 then inV a0 else --TODO)
--    , netInputs = [] }
--ev Net{ netPrim = SignExtend _ w1, netInputs = [a0] } =
--  n { netPrim   = Const w1 (if inV a0 > 0 then inV a0 else --TODO)
--    , netInputs = [] }
-- TODO XXX
evalConstNet n@Net{ netPrim = SelectBits w hi lo, netInputs = [Lit a0] } =
  (n { netPrim   = Const (hi-lo+1) ((a0 `B.shiftR` lo) B..&. (2^(hi-lo+1)-1))
    , netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Concat w0 w1, netInputs = [Lit a0, Lit a1] } =
  (n { netPrim = Const (w0+w1) ((a0 `B.shiftL` w1) B..|. a1)
     , netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Mux w, netInputs = [Lit s, Lit a0, Lit a1] } =
  (n { netPrim = Const w (if s == 0 then a1 else a0), netInputs = [] }, True)
evalConstNet n@Net{ netPrim = Identity w, netInputs = [Lit a0] } =
  (n { netPrim   = Const w a0, netInputs = [] }, True)
evalConstNet n = (n, False)
-- | Constant folding pass
foldConstants :: MNetlist -> IO Bool
foldConstants nl = do
  pairs <- getAssocs nl -- list of nets with their index
  changed <- newIORef False -- keep track of modifications to the 'Netlist'
  -- Evaluate each constant 'Net' and update it in the 'Netlist'
  forM_ [(a, b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
    let (net', change) = evalConstNet net
    when change $ do writeIORef changed True -- keep track of changes
                     writeArray nl idx $ Just net' -- update 'Netlist'
  -- finish pass
  -- DEBUG HELP -- x <- readIORef changed
  -- DEBUG HELP -- putStrLn $ "foldConstant pass changed? " ++ show x
  readIORef changed -- return whether a change occured

-- | Constant propagation pass
propagateConstants :: MNetlist -> IO Bool
propagateConstants nl = do
  pairs <- getAssocs nl -- list of nets with their index
  changed <- newIORef False -- keep track of modifications to the 'Netlist'
  -- Turn constant 'InputWire' for each 'Net' into constant 'InputTree'
  forM_ [(a, b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
    -- process each 'NetInput' for the current 'Net'
    inputs' <- forM (netInputs net) $ \inpt -> do
      case inpt of
        InputWire (instId, _) -> do
          inptNet <- readNet nl instId
          -- keep track of change when transforming into an 'InputTree'
          case netPrim inptNet of
            p@(Const _ _)  -> writeIORef changed True >> return (InputTree p [])
            p@(DontCare _) -> writeIORef changed True >> return (InputTree p [])
            _              -> return inpt
        _ -> return inpt
    -- update the current 'Net' in the 'Netlist'
    writeArray nl idx (Just net { netInputs = inputs' })
  -- finish pass
  -- DEBUG HELP -- x <- readIORef changed
  -- DEBUG HELP -- putStrLn $ "propagateConstant pass changed? " ++ show x
  readIORef changed

-- | Helper to inline a 'Net''s inputs
inlineNetInput :: MNetlist -> NetCounts -> NetInput -> IO (NetInput, Bool)
inlineNetInput nl nc inpt@(InputWire (instId, _)) = do
  -- read ref count for our referenced 'Net'
  cnt <- readArray nc instId
  -- read netPrim and netInputs for our referenced 'Net'
  Net{ netPrim = prim, netInputs = inpts } <- readNet nl instId
  -- attempt inlining our referenced 'Net', and its inputs recursively, also
  -- returning if inlining could happen as a Bool
  if cnt == 1 && canInline prim then do
    (inpts', changes) <- unzip <$> mapM
      (\x -> if canInlineInput prim then do (x', _) <- inlineNetInput nl nc x
                                            return (x', True)
             else return (x, False)) inpts
    return (InputTree prim inpts', or changes)
  else return (inpt, False)
inlineNetInput nl nc (InputTree prim inpts) = do
  (inpts', changes) <- unzip <$> mapM (inlineNetInput nl nc) inpts
  return $ (InputTree prim inpts', or changes)
-- | Single reference 'Net' inlining pass
inlineSingleRefNet :: MNetlist -> IO Bool
inlineSingleRefNet nl = do
  refCounts <- countNetRef nl -- reference count for each 'Net'
  pairs <- getAssocs nl -- list of nets with their index
  changed <- newIORef False -- keep track of modifications to the 'Netlist'
  -- Inline each "inlinable" (that is with a supported combinational underlying
  -- primitive) 'Net'
  forM_ [(a,b) | x@(a, Just b) <- pairs, canInlineInput (netPrim b)] $
    \(idx, net) -> do
      (netInputs', changes) <- unzip <$> mapM (inlineNetInput nl refCounts)
                                              (netInputs net)
      when (or changes) $ do -- on change, update 'Netlist' and keep track
        writeArray nl idx (Just net { netInputs = netInputs' })
        writeIORef changed True
  -- finish pass
  -- DEBUG HELP -- x <- readIORef changed
  -- DEBUG HELP -- putStrLn $ "inlineNetInput pass changed? " ++ show x
  readIORef changed

-- | Transform a 'Net' with non-zero-width output and at least one zero-width
--   input into an equivalent 'Net' with no reference to any other zero-width
--   output 'Net' as its input
zeroWidthNetTransform :: Net -> (Net, Bool)
-- straight transformation cases
zeroWidthNetTransform net@Net{ netPrim = Equal 0 } =
  (net { netPrim = Const 1 1, netInputs = [] }, True)
zeroWidthNetTransform net@Net{ netPrim = NotEqual 0 } =
  (net { netPrim = Const 1 0, netInputs = [] }, True)
zeroWidthNetTransform net@Net{ netPrim = LessThan 0 } =
  (net { netPrim = Const 1 0, netInputs = [] }, True)
zeroWidthNetTransform net@Net{ netPrim = LessThanEq 0 } =
  (net { netPrim = Const 1 1, netInputs = [] }, True)
zeroWidthNetTransform net@Net{ netPrim = ZeroExtend 0 w } =
  (net { netPrim = Const w 0, netInputs = [] }, True)
zeroWidthNetTransform net@Net{ netPrim = SignExtend 0 w } =
  (net { netPrim = Const w 0, netInputs = [] }, True)
zeroWidthNetTransform net@Net{ netPrim = SelectBits 0 hi lo } =
  (net { netPrim = Const (hi-lo) 0, netInputs = [] }, True)
zeroWidthNetTransform net@Net{ netPrim = Concat w0 w1, netInputs = [i0, i1] }
  | w0 == 0 && w1 /= 0 = (net { netPrim = Identity w1, netInputs = [i1] }, True)
  | w0 /= 0 && w1 == 0 = (net { netPrim = Identity w0, netInputs = [i0] }, True)
  | otherwise = (net, False)
zeroWidthNetTransform net@Net{ netPrim = Display args, netInputs = inpts } =
  let bitArgs = [ ba | ba@(DisplayArgBit w) <- args ]
      f (DisplayArgBit 0) i = (InputTree (Const 0 0) [], True)
      f _ i = (i, False)
      (tmps, changes) = unzip $ zipWith f bitArgs (tail inpts)
      inpts' = head inpts : tmps
  in (net { netInputs = inpts' }, or changes)
zeroWidthNetTransform net@Net{ netPrim   = prim@Custom{ customInputs = primIns }
                             , netInputs = netIns }
  | any (\(_, x) -> x == 0) primIns =
    ( net { netPrim = prim { customInputs = primIns' }
          , netInputs = netIns' }
    , True )
  | otherwise = (net, False)
  where ins = zip netIns primIns
        ins' = [x | x@(_, (_, w)) <- ins, w /= 0]
        (netIns', primIns') = unzip ins'
-- TODO currently unsupported cases that could be transformed
zeroWidthNetTransform net@Net{ netPrim = BRAM { ramAddrWidth = 0 } } =
  error "zeroWidthNetTransform unsupported on BRAM Prim"
zeroWidthNetTransform net@Net{ netPrim = TrueDualBRAM { ramAddrWidth = 0 } } =
  error "zeroWidthNetTransform unsupported on TrueDualBRAM Prim"
zeroWidthNetTransform
  net@Net{ netPrim = RegFileRead RegFileInfo{ regFileAddrWidth = 0 } } =
    error "zeroWidthNetTransform unsupported on RegFileRead Prim"
-- do nothing cases for all others
zeroWidthNetTransform net = (net, False)

-- | Tell if a 'Net' is a zero-width root 'Net' (in practice, only the 'Input'
--   and 'Output' 'Prim's)
isZeroWidthRootNet :: Net -> Bool
isZeroWidthRootNet Net{ netIsRoot = True
                      , netPrim = Input 0 _ } = True
isZeroWidthRootNet Net{ netIsRoot = True
                      , netPrim = Output 0 _ } = True
isZeroWidthRootNet _ = False

-- | Ignore 0-width Nets pass
ignoreZeroWidthNet :: MNetlist -> IO Bool
ignoreZeroWidthNet nl = do
  pairs <- getAssocs nl -- list of nets with their index
  changed <- newIORef False -- keep track of modifications to the 'Netlist'
  -- For each 'Net' (in particular, those with a non-zero-width output) with at
  -- least one zero-width input, transform it into a 'Net' with no reference to
  -- any zero-width output 'Net'.
  forM_ [(a,b) | x@(a, Just b) <- pairs] $ \(idx, net@Net{netPrim = prim}) -> do
    let (net', netChanged) = zeroWidthNetTransform net
    when netChanged $ do writeArray nl idx (Just net')
                         writeIORef changed True
  -- Remove each zero-width root 'Net'
  forM_ [i | x@(i, Just n) <- pairs, isZeroWidthRootNet n] $ \idx -> do
    writeArray nl idx Nothing
    writeIORef changed True
  -- finish pass
  -- DEBUG HELP -- x <- readIORef changed
  -- DEBUG HELP -- putStrLn $ "ignoreZeroWidthNet pass changed? " ++ show x
  readIORef changed

-- | Dead Net elimination pass
eliminateDeadNet :: MNetlist -> IO Bool
eliminateDeadNet nl = do
  refCounts <- countNetRef nl -- reference count for each 'Net'
  pairs <- getAssocs nl -- list of nets with their index
  changed <- newIORef False -- keep track of modifications to the 'Netlist'
  -- kill Nets with a null reference count
  forM_ [(a,b) | x@(a, Just b) <- pairs] $ \(idx, net) -> do
    refCnt <- readArray refCounts idx
    when (refCnt == 0 && not (null $ netOutputWidths net)) $ do
      writeArray nl idx Nothing
      writeIORef changed True
  -- finish pass
  -- DEBUG HELP -- x <- readIORef changed
  -- DEBUG HELP -- putStrLn $ "eliminateDeadNet pass changed? " ++ show x
  readIORef changed

-- | Run netlist transformation passes
netlistPasses :: Bool -> MNetlist -> IO Netlist
netlistPasses optimise nl = do
  -- remove 'Bit 0' instances
  ignoreZeroWidthNet nl
  -- netlist optimisation passes
  when optimise $ do
    let constElim i = do a <- foldConstants nl
                         b <- propagateConstants nl
                         -- DEBUG HELP
                         -- putStrLn $ "constElim " ++ show (a || b)
                         return $ a || b
    -- DEBUG HELP -- putStrLn $ "about to untilM constElim"
    untilM not $ constElim nl
    -- DEBUG HELP -- putStrLn $ "about to inlineSingleRefNet"
    inlineSingleRefNet nl
    return ()

  -- eliminate 'Net' entries in the netlist for 'Net's that got removed
  -- DEBUG HELP -- putStrLn $ "about to eliminateDeadNet"
  untilM not $ eliminateDeadNet nl
  -- turn the final netlist immutable
  freeze nl
