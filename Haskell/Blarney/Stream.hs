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
  ( Stream(..)
  , ToStream(..)
  , nullStream
  , StreamProcessor(..)
  , ToStreamProcessor(..)
  ) where

import Blarney
import Blarney.RAM

-- | Stream interface
data Stream a = Stream { canGet :: Bit 1
                       , get    :: Action ()
                       , value  :: a
                       } deriving (Generic, Interface)

-- | Convert to a Stream
class ToStream a b | a -> b where
  toStream :: a -> Stream b

-- | ToStream instance for Stream itself
instance ToStream (Stream t) t where
  toStream = id

-- | Null stream
nullStream :: Bits a => Stream a
nullStream = Stream { get    = noAction
                    , canGet = false
                    , value  = dontCare
                    }

-- | StreamProcessor type
type StreamProcessor t0 t1 = Stream t0 -> Module (Stream t1)

-- | Convert to a StreamProcessor
class ToStreamProcessor a t0 t1 | a -> t0 t1 where
  toStreamProcessor :: a -> StreamProcessor t0 t1

-- | ToStreamProcessor instance for StreamProcessor itself
instance ToStreamProcessor (StreamProcessor t0 t1) t0 t1 where
  toStreamProcessor = id
