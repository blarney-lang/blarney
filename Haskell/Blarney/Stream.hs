{-|
Module      : Blarney.Stream
Description : Stream library
Copyright   : (c) Matthew Naylor, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental
-}
module Blarney.Stream 
  ( Get(..)
  , Stream
  , toStream
  ) where

import Blarney
import Blarney.Queue
import Blarney.RAM

-- |Get interface
data Get a =
  Get {
    canGet :: Bit 1
  , get    :: RTL ()
  , value  :: a
  }
  deriving (Generic, Interface)

-- |Streams
type Stream a = Get a

-- |Convert a queue to a stream
toStream :: Queue a -> Stream a
toStream q =
  Get {
    get = q.deq
  , canGet = q.canDeq
  , value = q.first
  }
