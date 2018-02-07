-- Untyped bit vector representation

-- For Observable Sharing
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module Blarney.Unbit where

-- For join lists
import qualified Blarney.JList as JL

-- For Observable Sharing
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

-- Standard imports
import Control.Monad

-- Every instance of a component in the circuit has a unique id
type InstId = Int

-- Each output from a primitive component is numbered
type OutputNumber = Int

-- Bit vector widths and indices
type InputWidth = Int
type OutputWidth = Int
type BitIndex = Int

-- Primitive components
data Prim =
    -- Constants
    Const OutputWidth Integer
    -- Arithmetic
  | Add OutputWidth
  | Sub OutputWidth
  | Mul OutputWidth
  | Div OutputWidth
  | Mod OutputWidth
    -- Bitwise
  | Not OutputWidth
  | Inv OutputWidth
  | And OutputWidth
  | Or OutputWidth
  | Xor OutputWidth
    -- Shift
  | ShiftLeft OutputWidth
  | ShiftWidth OutputWidth
    -- Comparison
  | Equal InputWidth
  | NotEqual InputWidth
  | LessThan InputWidth
  | LessThanEq InputWidth
  | GreaterThan InputWidth
  | GreaterThanEq InputWidth
    -- Stateful
  | Reg InputWidth
  | RegEn InputWidth
    -- Width adjustment
  | ReplicateBit OutputWidth
  | ZeroExtend InputWidth OutputWidth
  | SignExtend InputWidth OutputWidth
    -- Bit selection and concatenation
  | SelectBits BitIndex BitIndex
  | Concat OutputWidth
    -- Misc
  | Mux OutputWidth
  | CountOnes OutputWidth
    -- Custom
    -- (component name, input names, output names, parameters)
  | Custom String [String] [String] [Param]

-- Custom components may have compile-time parameters
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
    unbitPrim :: Prim
    -- Unique id of primitive instance
  , unbitInstRef :: IORef (Maybe InstId)
    -- Inputs to the primitive instance
  , unbitInputs :: [Unbit]
    -- Output pin number
  , unbitOutNum :: OutputNumber
  }

-- Helper function for creating instance of a primitive component
{-# NOINLINE makePrim #-}
makePrim :: Prim -> [Unbit] -> Int -> [Unbit]
makePrim prim ins numOuts =
  [ Unbit {
        unbitPrim    = prim
      , unbitInstRef = ref
      , unbitInputs  = ins
      , unbitOutNum  = i
    }
  | i <- 0 .. (numOuts-1) ]
  where
    {-# NOINLINE ref #-}
    ref = newRef Nothing

-- Use of unsafePerformIO to implement Observable Sharing
{-# NOINLINE newRef #-}
newRef :: Maybe InstId -> IORef (Maybe InstId)
newRef x = unsafePerformIO (newIORef x)

-- Create instance of primitive component which has one output
makePrim1 :: Prim -> [Unbit] -> Unbit
makePrim1 prim ins = head (makePrim prim ins 1)

-- Netlists are lists of nets
data Net =
  Net {
      netPrim    :: Prim
    , netInstId  :: InstId
    , netInputs  :: [WireId]
  } deriving Show

-- An wire is uniquely identified by an instance id and an output number
type WireId = (InstId, OutputNumber)

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

-- Flatten bit vector to netlist
flatten :: Unbit -> Netlist WireId
flatten b =
  do val <- netlistLift (readIORef (unbitInstRef b))
     case val of
       Nothing -> do
         id <- netlistFreshId
         netlistLift (writeIORef (unbitInstRef b) (Just id))
         ins <- mapM flatten (unbitInputs b)
         let net = Net { netPrim    = unbitPrim b
                       , netInstId  = id
                       , netInputs  = ins
                       }
         netlistAdd net
         return (id, unbitOutNum b)
       Just id -> return (id, unbitOutNum b)
