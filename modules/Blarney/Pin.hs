-- For Observable Sharing
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module Blarney.Pin where

-- For join lists
import qualified Blarney.JList as JL

-- For Observable Sharing
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

-- Every instance of a component in the circuit has a unique id
type InstId = Int

-- Each output from a primitive component is numbered.
type OutputNumber = Int

-- Primitive component name
type PrimName = String

-- Primitive components may have compile-time parameters
-- A parameter has a name and a value, both represented as strings
data Param = String :-> String deriving Show

-- An output pin from a primitive component instance
data Pin = 
  Pin {
    -- What kind of primitive produced this pin?
    pinPrim :: PrimName
    -- Compile-time parameters
  , pinParams :: [Param]
    -- Unique id of primitive instance that produced it
  , pinInstRef :: IORef (Maybe InstId)
    -- Inputs to the primitive instance
  , pinInputs :: [Pin]
    -- Output pin number
  , pinOutNum :: OutputNumber
    -- Bit width of pin
  , pinWidth :: Int
  }

-- Helper function for creating instance of a primitive component
{-# NOINLINE primInst #-}
primInst :: PrimName -> [Param] -> [Pin] -> [Int] -> [Pin]
primInst prim params ins outWidths = map outPin (zip [0..] outWidths)
  where
    outPin (i, w) = Pin {
                        pinPrim    = prim
                      , pinParams  = params
                      , pinInstRef = ref
                      , pinInputs  = ins
                      , pinOutNum  = i
                      , pinWidth   = w
                    }

    {-# NOINLINE ref #-}
    ref = newRef Nothing

-- Use of unsafePerformIO to implement Observable Sharing
{-# NOINLINE newRef #-}
newRef :: Maybe InstId -> IORef (Maybe InstId)
newRef x = unsafePerformIO (newIORef x)

-- Create instance of primitive component which has one output
primInst1 :: PrimName -> [Param] -> [Pin] -> Int -> Pin
primInst1 prim params ins outWidth =
  head (primInst prim params ins [outWidth])

-- A reader/writer monad for accumulating the netlist
data Netlist a = Netlist { runNetlist :: NetlistR -> IO (NetlistW, a)) }

-- The reader component contains a IORef containing the next unique net id
type NetlistR = IORef Int

-- The writer component is the netlist
type NetlistW = JL.JList Net

instance Monad Netlist where
  return a = Netlist (\r -> return ([], a))
  m >>= f  = Netlist (\r -> do (w0, a) <- runRTL m r
                               (w1, b) <- runRTL (f a) r
                               return (w0 ++ w1, b))

instance Applicative Netlist where
  pure = return
  (<*>) = ap

instance Functor Netlist where
  fmap = liftM

netlistFreshId :: Netlist Int
netlistFreshId = Netlist $ \r -> do
  id <- readIORef r
  writeIOReg (id+1)
  return ([], id)

netlistAdd :: Net -> NetList ()
netlistAdd net = NetList $ \r -> do
  return (JL.One net, ())

-- Creating netlists
data Net =
  Net {
      netName    :: String
    , netParams  :: [Param]
    , netId      :: InstId
    , netInputs  :: [NetId]
    , netWidth   :: Int
  } deriving Show

-- A net is uniquely identified by a instance id and an output
type NetId = (InstId, OutputNumber)

-- Convert pin to netlist
pinToNetlist :: Pin -> IO NetId
pinToNetList pin =
  do val <- readIORef (pinInstRef pin)
     case val of
       Nothing -> do
         id <- netlistFreshId
         writeIORef (pingInstRef pin) (Just id)
         inIds <- mapM pinToNetlist (pinInputs pin)
         let net = Net { netName    = pinPrim pin
                       , netParams  = pinParams pin
                       , netId      = num
                       , netInputs  = inIds
                       , netWidth   = pinWidth pin
                       }
         return (id, pinOutNum pin)
       Just id -> return (id, pinOutNum pin)

--  XXX: in progress

pinToNetlist :: IORef Int -> Bit -> IO (JL.JList Net, NetId)
pinToNetlist i pin =
  do val <- readIORef (pinInstRef pin)
     num <- readIORef i
     case val of
       Nothing ->
         do writeIORef (pingInstRef pin) (Just num)
            writeIORef i (num+1)
            rest <- Prelude.mapM (pinToNetlist i) (pinInputs pin)
            let (nls, wires) = unzip rest
            let net = Net { netName    = pinPrim pin
                          , netParams  = pinParams pin
                          , netId      = num
                          , netInputs  = wires
                          , netWidth   = pinWidth pin
                          }
            return (foldr (JL.:+:) (JL.One net) nls, (num, pinOutNum pin))
       Just j -> return (JL.Zero, (j, pinOutNum pin))
