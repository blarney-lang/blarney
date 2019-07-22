{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

{-|
Module      : Blarney.Connectable
Description : Module to connect interfaces
Copyright   : (c) Alexandre Joannou, 2019
License     : MIT
Maintainer  : alexandre.joannou@gmail.com
Stability   : experimental

The 'Connectable' class provides a standard way to connect hardware modules
-}
module Blarney.Connectable
  ( -- * Connectable class
    Connectable(..)
  ) where

-- Blarney import
import Blarney

-- | 'Connectable' class
class Connectable a b where
  -- | Connects two interfaces that can be connected
  makeConnection :: a -- ^ First interface to connect
                 -> b -- ^ Second interface to connect
                 -> Module () -- ^ 'Module' with no interface, implementing the
                              --   connection
