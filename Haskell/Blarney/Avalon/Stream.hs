module Blarney.Avalon.Stream where

import Blarney
import Blarney.Stream

-- | Avalon stream interface
data AvlStream a =
  AvlStream {
    avlStreamReady :: Bit 1 -> Action ()
  , avlStreamValid :: Bit 1
  , avlStreamData :: a
  }
  deriving (Generic, Interface)

-- | Convert Blarney stream to Avalon stream
toAvlStream :: Stream a -> AvlStream a
toAvlStream s =
  AvlStream {
    avlStreamReady = \ready ->
      when (ready .&&. s.canPeek) do s.consume
  , avlStreamValid = s.canPeek
  , avlStreamData = s.peek
  }

-- | Convert Avalon stream to Blarney stream
fromAvlStream :: AvlStream a -> Stream a
fromAvlStream s =
  Source {
    peek = s.avlStreamData
  , canPeek = s.avlStreamValid
  , consume = avlStreamReady s true
  }
