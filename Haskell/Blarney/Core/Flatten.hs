{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Blarney.Flatten
Description : Flatten BV into Net
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019-2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}

module Blarney.Core.Flatten (
  ToNetlist(..) -- re-export the ToNetlist class
) where

import Prelude
import Data.IntSet
import Data.Array.ST
import Control.Monad
import Data.Array.Unboxed
import qualified Data.Set
import Data.Map (fromListWith)

import Blarney.Core.BV
import Blarney.Core.Prim
import Blarney.Core.NetHelpers
import Blarney.Core.Module (Module(..))
import qualified Blarney.Core.JList as JL
import Blarney.Core.RTL (RTL(..), RTLAction(..), R(..), Assign(..))

-- | A state/writer monad for accumulating the netlist
newtype Flatten a = Flatten { runFlatten :: S -> (S, W, a) }

-- | The state component contains the set of visited nodes
type S = IntSet

-- | The writer component contains the accumulated netlist and name hints
type W = (JL.JList Net, JL.JList (InstId, NameHints))

instance Monad Flatten where
  return a = Flatten $ \st -> (st, mempty, a)
  m >>= f  = Flatten $ \st -> let (s0, w0, a) = runFlatten m st
                                  (s1, w1, b) = runFlatten (f a) s0
                              in (s1, w0 <> w1, b)

instance Applicative Flatten where
  pure = return
  (<*>) = ap

instance Functor Flatten where
  fmap = liftM

-- | Retrieve the state component of the Flatten monad
getS :: Flatten S
getS = Flatten \st -> (st, (mempty, mempty), st)

-- | Set the state component of the Flatten monad
setS :: S -> Flatten ()
setS st = Flatten \_ -> (st, (mempty, mempty), ())

-- | Add a net to the netlist
addNet :: Net -> Flatten ()
addNet net = Flatten \st -> (st, (JL.One net, mempty), ())

-- | Add name hints to the list
addNameHints :: (InstId, NameHints) -> Flatten ()
addNameHints hints = Flatten \st -> (st, (mempty, JL.One hints), ())

-- | Flatten a root 'BV' to a netlist
flatten :: BV -> Flatten NetInput
flatten BV{bvPrim=p@(Const w v)} = return $ InputTree p []
flatten BV{bvPrim=p@(DontCare w)} = return $ InputTree p []
flatten BV{..} = do

  -- handle potential new name hints. TODO can we do this on first visit only?
  when (not $ Data.Set.null bvNameHints) $ addNameHints (bvInstId, bvNameHints)

  -- retrieve the set of visited nodes from the state
  visited <- getS

  -- Upon first visit of the current 'BV'
  when (not $ bvInstId `member` visited) do
    -- add current 'BV' to the set of visited nodes
    setS $ insert bvInstId visited
    -- recursively explore curent 'BV' 's inputs and generate and add a new
    -- 'Net' to the accumulation netlist
    ins <- mapM flatten bvInputs
    addNet Net { netPrim      = bvPrim
               , netInstId    = bvInstId
               , netInputs    = ins
               , netNameHints = mempty }

  -- return a handle to the visited 'Net'
  return $ InputWire (bvInstId, bvOutput)

-- | Convert RTL monad to a netlist
instance ToNetlist (RTL ()) where
  toNetlist rtl = runSTArray do
    mnl <- newListArray (0, length nl - 1)
                        [remapNetInstId (mapping !) n | n <- nl]
    -- update netlist with gathered names
    forM_ (JL.toList nms) $ \(idx, hints) -> do
      let idx' = mapping ! idx
      net@Net{ netNameHints = oldHints } <- readArray mnl (idx')
      writeArray mnl idx' net{ netNameHints = oldHints <> hints }
    -- return final netlist
    return mnl
    ------------------------
    where
      -- flatten BVs into a Netlist
      (visited, (jlnl, nms), _) = runFlatten flattenRoots empty
      nl = JL.toList jlnl
      -- guarantee consistent fresh mapping instead of relying on largest InstId
      -- being the size of the netlist... Likely unnecessary...
      maxInstId = findMax visited
      mapping :: UArray InstId InstId = array (0, maxInstId)
                                                (zip (fmap netInstId nl) [0..])
      -- get all actions from the RTL description
      (_, actsJL, _) = runRTL rtl (R { nameHints = mempty
                                     , cond = 1
                                     , assigns = assignMap }) 0
      acts = JL.toList actsJL
      assignMap = fromListWith (++) [(lhs a, [a]) | RTLAssign a <- acts]
      -- flatten the roots of the circuit
      flattenRoots = mapM flatten (concat [rts | RTLRoots rts <- acts])

-- | Convert Module monad to a netlist
instance ToNetlist (Module ()) where
  toNetlist = toNetlist . runModule
