{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Blarney.SourceSink where

-- Standard imports
--import GHC.TypeLits
--import Control.Monad.Fix
import Control.Monad hiding (when)
import GHC.Generics

-- Blarney imports
import Blarney
import Blarney.Stream
import Blarney.Connectable

------------------------------
-- Source / Sink interfaces --
--------------------------------------------------------------------------------

-- | Source interface
data Source t = Source { canPeek :: Bit 1
                       , peek    :: t
                       , consume :: Action ()
                       } deriving (Generic, Interface)

-- | Sink interface
data Sink t = Sink { put :: t -> Action (Bit 1)
                   } deriving (Generic, Interface)

-------------------------------
-- ToSource / ToSink classes --
--------------------------------------------------------------------------------

-- | ToSource class to convert to a Source
class ToSource a b | a -> b where
  toSource :: a -> Source b

-- | Identity convertion from Source to Source
instance ToSource (Source t) t where
  toSource = id

-- | ToSink class to convert to a Sink
class ToSink a b | a -> b where
  toSink :: a -> Sink b

-- | Identity convertion from Sink to Sink
instance ToSink (Sink t) t where
  toSink = id

-- | ToSource instance for members of ToStream
--instance {-# OVERLAPPABLE #-} (ToStream stream_t t) => ToSource stream_t t where
instance {-# OVERLAPPABLE #-} ToSource (Stream t) t where
  toSource stream = Source { canPeek = stream.canGet
                           , peek    = stream.value
                           , consume = stream.get
                           }

---------------------------
-- Other class instances --
--------------------------------------------------------------------------------

-- | Connectable instance for Source and Sink
instance Connectable (Source t) (Sink t) where
  makeConnection src snk = always do
    when (src.canPeek) do done <- (snk.put) (src.peek)
                          when done do src.consume

-- | ToStream instance for members of ToSource
--instance {-# OVERLAPPABLE #-} (ToSource src_t t) => ToStream src_t t where
instance {-# OVERLAPPABLE #-} ToStream (Source t) t where
  toStream src = Stream { get    = src.consume
                        , canGet = src.canPeek
                        , value  = src.peek
                        }

-- | ToStreamProcessor instance for (Sink, Source) pairs
instance ToStreamProcessor (Sink t0, Source t1) t0 t1 where
  toStreamProcessor (inSnk, outSrc) = \s -> do makeConnection (toSource s) inSnk
                                               return $ toStream outSrc

-----------------------------
-- helpers and other utils --
--------------------------------------------------------------------------------

-- | Null Source
nullSource :: (Bits t) => Source t
nullSource = Source { canPeek = false
                    , peek    = dontCare
                    , consume = noAction
                    }

-- | Null Sink
nullSink :: Sink t
nullSink = Sink { put = \x -> do return true }

-- | Wrap Source with debug info
debugSource :: (FShow t) => Source t -> Format -> Source t
debugSource src msg = Source {
  canPeek = src.canPeek
, peek    = src.peek
, consume = do display msg
                       " - Source consume - canPeek: " (fshow (src.canPeek))
                       " - " (fshow (src.peek))
               src.consume
}

-- | Wrap Sink with debug info
debugSink :: (FShow t) => Sink t -> Format -> Sink t
debugSink snk msg = Sink {
  put = \x -> do didPut <- (snk.put) x
                 when didPut do display msg " - Sink put - " (fshow x)
                 return didPut
}
