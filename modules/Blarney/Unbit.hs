-- Untyped bit vector representation

-- For Observable Sharing
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module Blarney.Unbit where

-- For join lists
import qualified Blarney.JList as JL

-- For Observable Sharing
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

import Control.Monad

-- Every instance of a component in the circuit has a unique id
type InstId = Int

-- Each output from a primitive component is numbered.
type OutputNumber = Int

-- Primitive component name
type PrimName = String

-- Primitive components may have compile-time parameters
-- A parameter has a name and a value, both represented as strings
data Param = String :-> String deriving Show

lookupParam :: [Param] -> String -> String
lookupParam ps p = case [v | (k :-> v) <- ps, p == k] of
                     [] -> error ("Unrecognised parameter '" ++ p ++ "'")
                     v:vs -> v

-- A untyped bit vector output from a primitive component instance
data Unbit = 
  Unbit {
    -- What kind of primitive produced this bit vector?
    unbitPrim :: PrimName
    -- Compile-time parameters of primitive
  , unbitParams :: [Param]
    -- Unique id of primitive instance
  , unbitInstRef :: IORef (Maybe InstId)
    -- Inputs to the primitive instance
  , unbitInputs :: [Unbit]
    -- Output pin number
  , unbitOutNum :: OutputNumber
    -- Bit width of vector
  , unbitWidth :: Int
  }

-- Helper function for creating instance of a primitive component
{-# NOINLINE primInst #-}
primInst :: PrimName -> [Param] -> [Unbit] -> [Int] -> [Unbit]
primInst prim params ins outWidths = map outUnbit (zip [0..] outWidths)
  where
    outUnbit (i, w) = Unbit {
                        unbitPrim    = prim
                      , unbitParams  = params
                      , unbitInstRef = ref
                      , unbitInputs  = ins
                      , unbitOutNum  = i
                      , unbitWidth   = w
                    }

    {-# NOINLINE ref #-}
    ref = newRef Nothing

-- Use of unsafePerformIO to implement Observable Sharing
{-# NOINLINE newRef #-}
newRef :: Maybe InstId -> IORef (Maybe InstId)
newRef x = unsafePerformIO (newIORef x)

-- Create instance of primitive component which has one output
primInst1 :: PrimName -> [Param] -> [Unbit] -> Int -> Unbit
primInst1 prim params ins outWidth =
  head (primInst prim params ins [outWidth])

-- A reader/writer monad for accumulating the netlist
newtype Netlist a = Netlist { runNetlist :: NetlistR -> IO (NetlistW, a) }

-- The reader component contains a IORef containing the next unique net id
type NetlistR = IORef Int

-- The writer component is the netlist
type NetlistW = JL.JList Net

instance Monad Netlist where
  return a = Netlist (\r -> return (JL.Zero, a))
  m >>= f  = Netlist (\r -> do (w0, a) <- runNetlist m r
                               (w1, b) <- runNetlist (f a) r
                               return (w0 JL.:+: w1, b))

instance Applicative Netlist where
  pure = return
  (<*>) = ap

instance Functor Netlist where
  fmap = liftM

netlistFreshId :: Netlist Int
netlistFreshId = Netlist $ \r -> do
  id <- readIORef r
  writeIORef r (id+1)
  return (JL.Zero, id)

netlistAdd :: Net -> Netlist ()
netlistAdd net = Netlist $ \r -> return (JL.One net, ())

netlistLift :: IO a -> Netlist a
netlistLift m = Netlist $ \r -> do
  a <- m
  return (JL.Zero, a)

-- Creating netlists
data Net =
  Net {
      netName    :: String
    , netParams  :: [Param]
    , netInstId  :: InstId
    , netInputs  :: [NetId]
    , netWidth   :: Int
  } deriving Show

-- A net is uniquely identified by an instance id and an output
type NetId = (InstId, OutputNumber)

-- Flatten bit vector to netlist
flatten :: Unbit -> Netlist NetId
flatten b =
  do val <- netlistLift (readIORef (unbitInstRef b))
     case val of
       Nothing -> do
         id <- netlistFreshId
         netlistLift (writeIORef (unbitInstRef b) (Just id))
         ins <- mapM flatten (unbitInputs b)
         let net = Net { netName    = unbitPrim b
                       , netParams  = unbitParams b
                       , netInstId  = id
                       , netInputs  = ins
                       , netWidth   = unbitWidth b
                       }
         netlistAdd net
         return (id, unbitOutNum b)
       Just id -> return (id, unbitOutNum b)
