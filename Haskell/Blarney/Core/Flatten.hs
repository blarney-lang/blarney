{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Blarney.Flatten
Description : Flatten BV into Net
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019-2020
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}

module Blarney.Core.Flatten (
  Flatten(..)   -- Monad for flattening a circuit (BV) to a netlist
, doIO          -- Lift an IO computation to a Flatten computation
, freshInstId   -- Obtain a fresh instance id
, addNet        -- Add a net to the netlist
, flatten       -- Flatten a bit vector to a netlist
, ToNetlist(..) -- Class of types that can be turned into 'Netlist's
) where

import Prelude
import Data.IORef
import Data.Array
import Data.Array.IO
import Data.Map (fromListWith)
import Control.Monad

import Blarney.Core.BV
import Blarney.Core.Net
import Blarney.Core.Prim
import Blarney.Core.RTL (RTL(..), RTLAction(..), R(..), Assign(..))
import Blarney.Core.Module (Module(..))
import qualified Blarney.Core.JList as JL

-- |A reader/writer monad for accumulating the netlist
newtype Flatten a = Flatten { runFlatten :: FlattenR -> IO (FlattenW, a) }

-- |The reader component contains an IORef containing the next unique net id
type FlattenR = IORef Int

-- |The writer component contains the netlist and
-- an "undo" computation, which unperforms all IORef assignments.
type FlattenW = (JL.JList Net, JL.JList (InstId, NameHints), IO ())

instance Monad Flatten where
  return a = Flatten $ \r -> return (mempty, a)
  m >>= f  = Flatten $ \r ->
    do (w0, a) <- runFlatten m r
       (w1, b) <- runFlatten (f a) r
       return (w0 <> w1, b)

instance Applicative Flatten where
  pure = return
  (<*>) = ap

instance Functor Flatten where
  fmap = liftM

-- |Obtain a fresh 'InstId' with the next available id
freshInstId :: Flatten InstId
freshInstId = Flatten $ \r -> do
  id <- readIORef r
  writeIORef r (id+1)
  return (mempty, id)

-- |Add a net to the netlist
addNet :: Net -> Flatten ()
addNet net = Flatten $ \r -> return ((JL.One net, mempty, mempty), ())

-- |Add name hints to the list
addNameHints :: (InstId, NameHints) -> Flatten ()
addNameHints hints = Flatten $ \r -> return ((mempty, JL.One hints, mempty), ())

-- |Add an "undo" computation
addUndo :: IO () -> Flatten ()
addUndo undo = Flatten $ \r -> return ((mempty, mempty, undo), ())

-- |Lift an IO computation to a Flatten computation
doIO :: IO a -> Flatten a
doIO m = Flatten $ \r -> do
  a <- m
  return (mempty, a)

-- | Flatten a root 'BV' to a netlist
flatten :: BV -> Flatten NetInput
flatten BV{bvPrim=p@(Const w v)} = return $ InputTree p []
flatten BV{bvPrim=p@(DontCare w)} = return $ InputTree p []
flatten bv@BV{bvNameHints=hints,bvInstRef=instRef} = do
  -- handle instId traversal
  instIdVal <- doIO (readIORef instRef)
  let hasNameHints = not $ null hints
  case instIdVal of
    Nothing -> do
      instId <- freshInstId
      when hasNameHints $ addNameHints (instId, hints)
      doIO (writeIORef instRef (Just instId))
      addUndo (writeIORef instRef Nothing)
      ins <- mapM flatten (bvInputs bv)
      let net = Net { netPrim         = bvPrim bv
                    , netInstId       = instId
                    , netInputs       = ins
                    , netNameHints    = mempty
                    }
      addNet net
      return $ InputWire (instId, bvOutput bv)
    Just instId -> do when hasNameHints $ addNameHints (instId, hints)
                      return $ InputWire (instId, bvOutput bv)

class ToNetlist a where
  toNetlist :: a -> IO Netlist

-- | Convert RTL monad to a netlist
instance ToNetlist (RTL ()) where
  toNetlist rtl = do
    -- flatten BVs into a Netlist
    i <- newIORef (0 :: InstId)
    ((nl, nms, undo), _) <- runFlatten flattenRoots i
    maxId <- readIORef i
    mnl :: IOArray InstId (Maybe Net) <-
      thaw $ listArray (0, maxId) (replicate (maxId+1) Nothing)
          // [(netInstId n, Just n) | n <- JL.toList nl]
    -- update netlist with gathered names
    forM_ (JL.toList nms) $ \(idx, hints) -> do
      mnet <- readArray mnl idx
      case mnet of
        Just net@Net{ netNameHints = oldHints } ->
          writeArray mnl idx (Just net { netNameHints = oldHints <> hints })
        _ -> return ()
    -- run undo computations
    undo
    -- return final netlist
    freeze mnl
    ------------------------
    where
      (_, actsJL, _) = runRTL rtl (R { nameHints = mempty
                                     , cond = 1
                                     , assigns = assignMap }) 0
      acts = JL.toList actsJL
      assignMap = fromListWith (++) [(lhs a, [a]) | RTLAssign a <- acts]
      flattenRoots = mapM flatten (concat [rts | RTLRoots rts <- acts])

-- | Convert Module monad to a netlist
instance ToNetlist (Module ()) where
  toNetlist = toNetlist . runModule
