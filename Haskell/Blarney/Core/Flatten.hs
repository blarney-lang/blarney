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
import qualified Data.Set
import Data.Array.Unboxed
import Control.Monad.Trans
import Data.Functor.Identity
import Data.Map (fromListWith)
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Blarney.Core.BV
import Blarney.Core.Prim
import Blarney.Core.NetHelpers
import Blarney.Core.Module (Module(..))
import qualified Blarney.Core.JList as JL
import Blarney.Core.RTL (RTL(..), RTLAction(..), R(..), Assign(..))

-- | A state/writer monad for accumulating the netlist
type Flatten = StateT S (WriterT W Identity)

-- | The state component contains the set of visited nodes
type S = IntSet

-- | The writer component contains the accumulated netlist and name hints
type W = (JL.JList Net, JL.JList (InstId, NameHints))

-- | run the 'Flatten' monad and return a tuple with the final state, the final
--   writer accumulator and a return value
execFlatten :: Flatten a -> S -> (S, W, a)
execFlatten m s0 = (s, w, x)
  where f = runIdentity . runWriterT . (flip runStateT) s0
        ((x, s), w) = f m

-- | Get the set of visited nodes
getVisited :: Flatten S
getVisited = get

-- | set the set of visited nodes
putVisited :: S -> Flatten ()
putVisited = put

-- | Add a net to the netlist
addNet :: Net -> Flatten ()
addNet net = lift $ tell (JL.One net, mempty)

-- | Add name hints to the list
addNameHints :: (InstId, NameHints) -> Flatten ()
addNameHints hints = lift $ tell (mempty, JL.One hints)

-- | Flatten a root 'BV' to a netlist
flatten :: BV -> Flatten NetInput
flatten BV{bvPrim=p@(Const w v)} = return $ InputTree p []
flatten BV{bvPrim=p@(DontCare w)} = return $ InputTree p []
flatten BV{..} = do

  -- handle potential new name hints. TODO can we do this on first visit only?
  when (not $ Data.Set.null bvNameHints) $ addNameHints (bvInstId, bvNameHints)

  -- retrieve the set of visited nodes from the state
  visited <- getVisited

  -- Upon first visit of the current 'BV'
  when (not $ bvInstId `member` visited) do
    -- add current 'BV' to the set of visited nodes
    putVisited $ insert bvInstId visited
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
      (visited, (jlnl, nms), _) = execFlatten flattenRoots empty
      nl = JL.toList jlnl
      -- for remapping instance ids to a compact range starting from 0
      minInstId = findMin visited
      maxInstId = findMax visited
      mapping :: UArray InstId InstId = array (minInstId, maxInstId)
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
