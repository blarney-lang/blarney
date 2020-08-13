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

instance Functor Source where
  fmap = mapSource

-- | 'Sink' type. A 'Sink' 't' 'Interface' can be fed values of type 't' via its
--   'put' method, expected to be called when its 'canPut' method returns
--   'true'.
data Sink t = Sink { canPut :: Bit 1
                     -- ^ Checks whether a value can be 'put' this cycle
                   , put :: t -> Action ()
                     -- ^ Puts a value of type 't' into the 'Sink'
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
    when ((src.canPeek) .&. (snk.canPut)) do (snk.put) (src.peek)
                                             src.consume

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
nullSink = Sink { canPut = true
                , put    = \x -> noAction }

-- | \"Null\" 'Sink' 'Module' that always consumes its input.
makeNullSink :: Module (Sink t)
makeNullSink = return nullSink

-- | \"map\" a function over a 'Source' and returns the new 'Source'. Note: this
--   function is suitable for use as 'fmap' in a 'Functor' instance of 'Source'.
mapSource :: (a -> b) -- ^ The function to map over the initial 'Source'
          -> Source a -- ^ The initial 'Source'
          -> Source b -- ^ The new 'Source'
mapSource f src = src { peek = f (src.peek) }

-- | \"map\" a function over a 'Sink' and returns the new 'Sink'
mapSink :: (b -> a) -- ^ The function to map over the initial 'Sink'. Note:
                    --   The type 'b -> a' is making this function not suitable
                    --   for a potential 'Functor' instance of 'Sink'
        -> Sink a   -- ^ The initial 'Source'
        -> Sink b   -- ^ The new 'Source'
mapSink f snk = snk { put = \x -> (snk.put) (f x) }

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
  canPut = snk.canPut
, put = \x -> do (snk.put) x
                 display msg " - Sink put - " (fshow x)
}

-- | Left-biased merge of two sources
mergeTwoSources :: Bits a => Source a -> Source a -> Source a
mergeTwoSources s0 s1 =
  Source {
    canPeek = s0.canPeek .|. s1.canPeek
  , peek    = s0.canPeek ? (s0.peek, s1.peek)
  , consume = if s0.canPeek then s0.consume else s1.consume
  }

-- |Sequential left-biased merge of multiple sources
mergeSourcesSeq :: Bits a => [Source a] -> Source a
mergeSourcesSeq = foldr mergeTwoSources nullSource

-- |Tree merge of multiple sources, where each merge is left biased
mergeSourcesTree :: Bits a => [Source a] -> Source a
mergeSourcesTree = tree mergeTwoSources nullSource
