{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module Blarney.Connectable
  ( -- * Connectable class
    Connectable(..)
  ) where

-- Blarney import
import Blarney

-- | Connectable class
class Connectable a b where
  makeConnection :: a -> b -> Module ()
