module Blarney.Avalon.Stream where

import Blarney
import Blarney.Stream

type AvlStream a = AvlStreamIfc (SizeOf a)

-- | Avalon stream interface
data AvlStreamIfc w =
  AvlStreamIfc {
    avlStreamReady :: Action ()
  , avlStreamValid :: Bit 1
  , avlStreamData :: Bit w
  }
  deriving (Generic, Interface)

-- | Convert Blarney stream to Avalon stream
toAvlStream :: Bits a => Stream a -> AvlStream a
toAvlStream s =
  AvlStreamIfc {
    avlStreamReady = when (s.canPeek) do s.consume
  , avlStreamValid = s.canPeek
  , avlStreamData = s.peek.pack
  }

-- | Convert Avalon stream to Blarney stream
fromAvlStream :: Bits a => AvlStream a -> Stream a
fromAvlStream s =
  Source {
    peek = s.avlStreamData.unpack
  , canPeek = s.avlStreamValid
  , consume = s.avlStreamReady
  }
