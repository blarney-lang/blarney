module Blarney.RegFile where

import GHC.TypeLits
import Blarney.Bit
import Blarney.Bits
import Blarney.Prelude
import Blarney.RTL
import Control.Monad hiding (when)
import Prelude

data RegFile a d =
  RegFile {
    (!)    :: a -> d
  , update :: a -> d -> RTL ()
  }

makeRegFile :: forall a d. (Bits a, Bits d, _) => RTL (RegFile a d)
makeRegFile = do
  -- Create list of registers
  regs :: [Reg d] <- replicateM (2 ^ sizeOf (__ :: a)) makeReg

  -- Methods
  let load a = index (pack a) (map val regs)

  let store a d =
        sequence_
          [ when (pack a .==. fromInteger i) (r <== d)
          | (i, r) <- zip [0..] regs ]

  return (RegFile load store)
