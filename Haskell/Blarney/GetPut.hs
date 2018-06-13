module Blarney.GetPut where

import Blarney
import Blarney.Interface
import GHC.Generics

data Get a =
  Get {
    get    :: RTL ()
  , canGet :: Bit 1
  , value  :: a
  }
  deriving (Generic, Interface)

data Put a =
  Put {
    canPut :: Bit 1
  , put    :: a -> RTL ()
  }
  deriving (Generic, Interface)
