-- For Observable Sharing
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

module Blarney.Pin where

-- For Observable Sharing
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

-- Every instance of a component in the circuit has a unique id
type InstId = Int

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
  , pinOutNum :: Int
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
