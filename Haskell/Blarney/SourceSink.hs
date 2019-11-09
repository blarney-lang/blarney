{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}

{-|
Module      : Blarney.SourceSink
Description : Source and Sink interfaces for control flow
Copyright   : (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : alexandre.joannou@gmail.com
Stability   : experimental

This module defines the 'Source' and 'Sink' types. These types can be used as
'Module' 'Interfaces' do provide a standardized way of handling flow control.

-}
module Blarney.SourceSink where

-- Standard imports
import Control.Monad hiding (when)
import GHC.Generics

-- Blarney imports
import Blarney
import Blarney.Connectable

------------------------------
-- Source / Sink interfaces --
--------------------------------------------------------------------------------

-- | 'Source' type. A 'Source' 't' 'Interface' returns values of type 't' via
--   its 'peek' method and flow control through its 'canPeek' and 'consume'
--   methods.
data Source t = Source { canPeek :: Bit 1
                         -- ^ Checks whether a value can be 'peek'ed or
                         --   'consume'd on this cycle.
                       , peek    :: t
                         -- ^ Returns the value that the 'Source' is producing
                         --   this cycle. The value is only relevant on a cycle
                         --   where 'canPeek' is 'true'.
                       , consume :: Action ()
                         -- ^ Consumes a value from the 'Source'. 'consume'
                         --   should only be called on a cycle where 'canPeek'
                         --   is 'true'. When implementing a 'Module' returning
                         --   a 'Source' 'Interface', one might consider using
                         --   the signal returned by 'canPeek' to protect the
                         --   'Action' with a 'when'.
                       } deriving (Generic, Interface)

-- | 'Sink' type. A 'Sink' 't' 'Interface' can be fed values of type 't' via its
--   'put' method and can exercise back pressure with the returned 'Bit' 1.
data Sink t = Sink { put :: t -> Action (Bit 1)
                     -- ^ Attempts to put a value of type 't' into the 'Sink',
                     --   and returns 'true' on success and 'false' on failure.
                   } deriving (Generic, Interface)

-------------------------------
-- ToSource / ToSink classes --
--------------------------------------------------------------------------------

-- | 'ToSource' class to convert to a 'Source'
class ToSource a b | a -> b where
  -- | Converts its argument of type 'a' to a 'Source' 'b'
  toSource :: a -> Source b

-- | Identity conversion from 'Source' to 'Source'
instance ToSource (Source t) t where
  toSource = id

-- | 'ToSink' class to convert to a 'Sink'
class ToSink a b | a -> b where
  -- | Converts its argument of type 'a' to a 'Sink' 'b'
  toSink :: a -> Sink b

-- | Identify conversion from 'Sink' to 'Sink'
instance ToSink (Sink t) t where
  toSink = id

---------------------------
-- Other class instances --
--------------------------------------------------------------------------------

-- | 'Connectable' instance for 'Source' 't' and 'Sink' 't'
instance Connectable (Source t) (Sink t) where
  makeConnection src snk = always do
    when (src.canPeek) do done <- (snk.put) (src.peek)
                          when done do src.consume

-----------------------------
-- helpers and other utils --
--------------------------------------------------------------------------------

-- | \"Null\" 'Source' that never produces any valid output.
nullSource :: (Bits t) => Source t
nullSource = Source { canPeek = false
                    , peek    = dontCare
                    , consume = noAction
                    }
-- | \"Null\" 'Source' 'Module' that never produces any valid output.
makeNullSource :: (Bits t) => Module (Source t)
makeNullSource = return nullSource

-- | \"Null\" 'Sink' that always consumes its input.
nullSink :: Sink t
nullSink = Sink { put = \x -> do return true }

-- | \"Null\" 'Sink' 'Module' that always consumes its input.
makeNullSink :: Module (Sink t)
makeNullSink = return nullSink

-- | Wraps a 'Source' 't' with some debug info.
debugSource :: (FShow t) => Source t -- ^ The 'Source' to debug
                         -> Format   -- ^ A 'Format' prefix
                         -> Source t -- ^ The wrapped 'Source'
debugSource src msg = Source {
  canPeek = src.canPeek
, peek    = src.peek
, consume = do display msg
                       " - Source consume - canPeek: " (fshow (src.canPeek))
                       " - " (fshow (src.peek))
               src.consume
}

-- | Wraps a 'Sink' 't' with some debug info.
debugSink :: (FShow t) => Sink t -- ^ The 'Sink' to debug
                       -> Format -- ^ A 'Format' prefix
                       -> Sink t -- ^ The wrapped 'Sink'
debugSink snk msg = Sink {
  put = \x -> do didPut <- (snk.put) x
                 when didPut do display msg " - Sink put - " (fshow x)
                 return didPut
}
