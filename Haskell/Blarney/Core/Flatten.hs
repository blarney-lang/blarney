{-# LANGUAGE PatternSynonyms #-}

{-|
Module      : Blarney.Flatten
Description : Flatten BV into Net
Copyright   : (c) Matthew Naylor, 2019
              (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}

module Blarney.Core.Flatten (
  Flatten(..) -- Monad for flattening a circuit (BV) to a netlist
, doIO        -- Lift an IO computation to a Flatten computation
, freshInstId -- Obtain a fresh instance id
, addNet      -- Add a net to the netlist
, flatten     -- Flatten a bit vector to a netlist
) where

import Prelude
import Data.IORef
import Control.Monad

import Blarney.Core.BV
import Blarney.Core.Net
import Blarney.Core.Prim
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

-- |Flatten bit vector to netlist
flatten :: BV -> Flatten NetInput
flatten BV{bvPrim=p@(Const w v)} = return $ InputTree p []
flatten BV{bvPrim=p@(DontCare w)} = return $ InputTree p []
flatten b@BV{bvNameHints=hints,bvInstRef=instRef} = do
  -- handle instId traversal
  instIdVal <- doIO (readIORef instRef)
  let hasNameHints = not $ null hints
  case instIdVal of
    Nothing -> do
      instId <- freshInstId
      when hasNameHints $ addNameHints (instId, hints)
      doIO (writeIORef instRef (Just instId))
      addUndo (writeIORef instRef Nothing)
      ins <- mapM flatten (bvInputs b)
      let net = Net { netPrim         = bvPrim b
                    , netInstId       = instId
                    , netInputs       = ins
                    , netOutputWidths = (bvWidths b)
                    , netNameHints    = mempty
                    }
      addNet net
      return $ InputWire (instId, bvOutNum b)
    Just instId -> do when hasNameHints $ addNameHints (instId, hints)
                      return $ InputWire (instId, bvOutNum b)
