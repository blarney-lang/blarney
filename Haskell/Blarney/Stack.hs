{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Blarney.Stack
Description : Library of various stack implementations
Copyright   : (c) Matthew Naylor, 2021
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}
module Blarney.Stack where

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.Option
import Blarney.PulseWire
import Blarney.SourceSink

-- Standard imports
import Data.Proxy

-- | Stack interface
data Stack a =
  Stack {
    notEmpty :: Bit 1
  , notFull  :: Bit 1
  , push     :: a -> Action ()
  , pop      :: Action ()
  , top      :: a
  , clear    :: Action ()
  } deriving (Generic, Interface)

instance ToSource (Stack t) t where
  toSource s = Source { canPeek = s.notEmpty
                      , peek    = s.top
                      , consume = s.pop
                      }

instance ToSink (Stack t) t where
  toSink s = Sink { canPut = s.notFull
                  , put    = s.push
                  }

-- | Sized queue of given size, backed by RAM, with top element cached
-- in a register
makeSizedStack :: Bits a =>
     Int
     -- ^ Log of the capacity of the stack
  -> Module (Stack a)
makeSizedStack logSize = do
  -- Lift size to type-level address-width
  liftNat logSize $ \(_ :: Proxy aw) -> do

    -- A dual-port RAM, big enough to hold entire stack
    ram :: RAM (Bit aw) a <- makeDualRAMForward

    -- Stack pointer
    sp :: Reg (Bit aw) <- makeReg 0

    -- Top of stack cached in register
    topReg :: Reg a <- makeReg dontCare

    -- Stack full/empty?
    empty <- makeReg true
    full <- makeReg false

    -- Interface wires
    popWire <- makePulseWire
    pushWire <- makeWire dontCare
    clearWire <- makePulseWire

    always do
      if clearWire.val
        then do
          empty <== true
          full <== false
          sp <== 0
        else do
          let spMinus1 = sp.val - 1
          -- On push, always update topReg
          when pushWire.active do
            topReg <== pushWire.val
          -- When pushing but not popping
          when (pushWire.active .&&. inv popWire.val) do
            ram.store sp.val topReg.val
            ram.load sp.val
            sp <== sp.val + 1
            empty <== false
            full <== sp.val .==. ones
          -- When popping but not pushing
          when (inv pushWire.active .&&. popWire.val) do
            topReg <== ram.out
            ram.load (sp.val - 2)
            sp <== spMinus1
            empty <== sp.val .==. 1
            full <== false

    return
      Stack {
        notEmpty = inv empty.val
      , notFull = inv full.val
      , push = \x -> do pushWire <== x
      , pop = popWire.pulse
      , top = topReg.val
      , clear = clearWire.pulse
      }
