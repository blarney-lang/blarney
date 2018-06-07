module Blarney.GetPut where

import Blarney

data Get a =
  Get {
    get    :: RTL ()
  , canGet :: Bit 1
  , value  :: a
  }

data Put a =
  Put {
    canPut :: Bit 1
  , put    :: a -> RTL ()
  }
