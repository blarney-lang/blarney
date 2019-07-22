{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Module      : Blarney.Stream
Description : Stream library
Copyright   : (c) Matthew Naylor, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}
module Blarney.Stream
  ( Stream(..), Source(..)
  , toStream
  , nullStream
  , SP(..)
  , ToSP(..)
  ) where

import Blarney
import Blarney.SourceSink
import Blarney.Connectable

-- | Stream interface
type Stream a = Source a

-- | Convert to a Stream
toStream :: (ToSource a b) => a -> Stream b
toStream = toSource

-- | Null stream
nullStream :: Bits a => Stream a
nullStream = nullSource

-- | StreamProcessor type
type SP t0 t1 = Stream t0 -> Module (Stream t1)

-- | Convert to a StreamProcessor
class ToSP a t0 t1 | a -> t0 t1 where
  toSP :: a -> SP t0 t1

-- | ToSP instance for StreamProcessor itself
instance ToSP (SP t0 t1) t0 t1 where
  toSP = id

-- | ToSP instance for (Sink, Source) pairs
instance ToSP (Sink t0, Source t1) t0 t1 where
  toSP (inSnk, outSrc) = \inStream -> do makeConnection inStream inSnk
                                         return outSrc
