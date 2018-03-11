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
type Width = Int
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
  | And OutputWidth
  | Or OutputWidth
  | Xor OutputWidth
    -- Shift
  | ShiftLeft OutputWidth
  | ShiftRight OutputWidth
    -- Comparison
  | Equal InputWidth
  | NotEqual InputWidth
  | LessThan InputWidth
  | LessThanEq InputWidth
    -- Stateful
  | Register Integer InputWidth
  | RegisterEn Integer InputWidth
  | RAM { ramInitFile  :: Maybe String
        , ramAddrWidth :: Width
        , ramDataWidth :: Width }
  | TrueDualRAM { ramInitFile  :: Maybe String
                , ramAddrWidth :: Width
                , ramDataWidth :: Width }
    -- Width adjustment
  | ReplicateBit OutputWidth
  | ZeroExtend InputWidth OutputWidth
  | SignExtend InputWidth OutputWidth
    -- Bit selection and concatenation
  | SelectBits InputWidth BitIndex BitIndex
  | Concat InputWidth InputWidth
    -- External I/O
  | Input OutputWidth String
  | Output InputWidth String
    -- Misc
  | Mux OutputWidth
  | CountOnes OutputWidth
  | Identity OutputWidth
    -- Simulation-time I/O
  | Display [DisplayArg]
  | Finish
    -- Custom combinatorial component
    -- (component name, input names, output names/widths, parameters)
  | Custom String [String] [(String, Int)] [Param]
  deriving Show

-- Display a string literal or a wire of a given width
data DisplayArg = DisplayArgString String | DisplayArgBit InputWidth

instance Show DisplayArg where
  show (DisplayArgString s) = show s
  show (DisplayArgBit w) = show w

-- Custom components may have compile-time parameters
-- A parameter has a name and a value, both represented as strings
data Param = String :-> String deriving Show

lookupParam :: [Param] -> String -> String
lookupParam ps p = case [v | (k :-> v) <- ps, p == k] of
                     [] -> error ("Unrecognised parameter '" ++ p ++ "'")
                     v:vs -> v

-- A untyped bit vector output wire from a primitive component instance
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
    -- Width of this output pin
  , unbitWidth :: Width
    -- Width of each output pin
  , unbitWidths :: [Width]
  }

-- Helper function for creating instance of a primitive component
{-# NOINLINE makePrim #-}
makePrim :: Prim -> [Unbit] -> [Int] -> [Unbit]
makePrim prim ins outWidths =
  [ Unbit {
        unbitPrim    = prim
      , unbitInstRef = ref
      , unbitInputs  = ins
      , unbitOutNum  = i
      , unbitWidth   = w
      , unbitWidths  = outWidths
    }
  | (i, w) <- zip [0..] outWidths ]
  where
    {-# NOINLINE ref #-}
    ref = newRef Nothing

-- Use of unsafePerformIO to implement Observable Sharing
{-# NOINLINE newRef #-}
newRef :: Maybe InstId -> IORef (Maybe InstId)
newRef x = unsafePerformIO (newIORef x)

-- Create instance of primitive component which has one output
makePrim1 :: Prim -> [Unbit] -> Int -> Unbit
makePrim1 prim ins width = head (makePrim prim ins [width])

-- Netlists are lists of nets
data Net =
  Net {
      netPrim         :: Prim
    , netInstId       :: InstId
    , netInputs       :: [WireId]
    , netOutputWidths :: [Width]
  } deriving Show

-- An wire is uniquely identified by an instance id and an output number
type WireId = (InstId, OutputNumber)

-- A reader/writer monad for accumulating the netlist
newtype Flatten a = Flatten { runFlatten :: FlattenR -> IO (FlattenW, a) }

-- The reader component contains a IORef containing the next unique net id
type FlattenR = IORef Int

-- The writer component is the netlist
type FlattenW = JL.JList Net

instance Monad Flatten where
  return a = Flatten (\r -> return (JL.Zero, a))
  m >>= f  = Flatten (\r -> do (w0, a) <- runFlatten m r
                               (w1, b) <- runFlatten (f a) r
                               return (w0 JL.:+: w1, b))

instance Applicative Flatten where
  pure = return
  (<*>) = ap

instance Functor Flatten where
  fmap = liftM

freshId :: Flatten Int
freshId = Flatten $ \r -> do
  id <- readIORef r
  writeIORef r (id+1)
  return (JL.Zero, id)

addNet :: Net -> Flatten ()
addNet net = Flatten $ \r -> return (JL.One net, ())

doIO :: IO a -> Flatten a
doIO m = Flatten $ \r -> do
  a <- m
  return (JL.Zero, a)

-- Flatten bit vector to netlist
flatten :: Unbit -> Flatten WireId
flatten b =
  do val <- doIO (readIORef (unbitInstRef b))
     case val of
       Nothing -> do
         id <- freshId
         doIO (writeIORef (unbitInstRef b) (Just id))
         ins <- mapM flatten (unbitInputs b)
         let net = Net { netPrim         = unbitPrim b
                       , netInstId       = id
                       , netInputs       = ins
                       , netOutputWidths = (unbitWidths b)
                       }
         addNet net
         return ((id, unbitOutNum b))
       Just id -> return ((id, unbitOutNum b))
