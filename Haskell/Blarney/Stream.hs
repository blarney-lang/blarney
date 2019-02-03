{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

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
  , toStream
  ) where

import Blarney
import Blarney.Queue
import Blarney.RAM
import Prelude hiding ((.))

-- |Stream interface
data Stream a =
  Stream {
    canGet :: Bit 1
  , get    :: Action ()
  , value  :: a
  }
  deriving (Generic, Interface)

-- |Convert a queue to a stream
toStream :: Queue a -> Stream a
toStream q =
  Stream {
    get = q.deq
  , canGet = q.canDeq
  , value = q.first
  }
