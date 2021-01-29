{-# LANGUAGE NoRebindableSyntax #-}

{- |
Module      : Blarney.Netlist.Misc.MonadLoops
Description : Monadic loops non provided in base
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

This module provides monadic loop helpers non present in base.
Inspired by Control.Monad.Loops

-}

module Blarney.Misc.MonadLoops (
  untilPredM
, untilPredM_
, untilM
, untilM_
) where

import Prelude

infixr 0 `untilPredM`
-- | Repeat computation until a predicate holds
untilPredM :: Monad m => m a -> (a -> Bool) -> m [a]
untilPredM act pred = do
  x <- act
  if pred x then return [x] else do xs <- untilPredM act pred
                                    return $ x:xs

infixr 0 `untilPredM_`
-- | Same as 'untilPredM' but discard the final result
untilPredM_ :: Monad m => m a -> (a -> Bool) -> m ()
untilPredM_ act pred = untilPredM act pred >> return ()

infixr 0 `untilM`
-- | Repeat computation until the second monadic computation returns 'True'
untilM :: Monad m => m a -> m Bool -> m [a]
untilM act predM = do
  x <- act
  cond <- predM
  if cond then return [x] else do xs <- untilM act predM
                                  return $ x:xs

infixr 0 `untilM_`
-- | Same as 'untilM' but discard the final result
untilM_ :: Monad m => m a -> m Bool -> m ()
untilM_ act predM = untilM act predM >> return ()
