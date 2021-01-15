{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE BlockArguments #-}

{-|
Module      : Blarney.Recipe
Description : Basic imperative programming on top of RTL
Copyright   : (c) 2019 Matthew Naylor, Dillon Huff
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

Recipe is a lightweight imperative language, aiding concise
definitions of complex state machines.
-}
module Blarney.Recipe
  ( Recipe(..)
  , runOnce
  , runRecipe
  , runRecipeOn
  ) where

-- Standard imports
import Prelude
import Data.Maybe

-- Blarney imports
import Blarney.Core

-- |Abstract syntax of Recipe
data Recipe =
    Skip
  | Tick
  | Action (Action ())
  | Seq [Recipe]
  | Par [Recipe]
  | Wait (Bit 1)
  | When (Bit 1) Recipe
  | If (Bit 1) Recipe Recipe
  | While (Bit 1) Recipe
  | Background Recipe

-- Is time taken by given statement known?
known :: Recipe -> Bool
known r = isJust (time r)

-- Static timing analysis
time :: Recipe -> Maybe Int
time Skip = Just 0
time Tick = Just 1
time (Action act) = Just 1
time (Seq rs) = sum `fmap` mapM time rs
time (Par rs) = foldr max 0 `fmap` mapM time rs
time other = Nothing

-- Return index of the slowest recipe
slowest :: [Recipe] -> Int
slowest = snd . maximum . flip zip [0..] . map time

-- |Run a recipe.  Take a go pulse and return a finish pulse.
run :: Bit 1 -> Recipe -> Module (Bit 1)
run go Skip         = return go
run go Tick         = return (reg 0 go)
run go (Action act) = always (when go act) >> return (reg 0 go)
run go (Seq [])     = return go
run go (Seq (r:rs)) = do { done <- run go r; run done (Seq rs) }
run go (Par rs)     = do
  dones <- mapM (run go) rs
  return (if all known rs then dones !! slowest rs else sync dones)
run go (Wait c)     = run go (While (inv c) Tick)
run go (When c r)   = run (go .&. c) r
run go (If c r1 r2) = do
  done1 <- run (go .&. c) r1
  done2 <- run (go .&. inv c) r2
  return (done1 .|. done2)
run go (While c r)  = do
  ready <- makeWire dontCare
  done <- run (val ready .&. c) r
  always do
    ready <== go .|. done
  return (val ready .&. inv c)
run go (Background r) = do
  done <- run go r
  return go

sync :: [Bit 1] -> Bit 1
sync [x] = x
sync xs = let done = andList [setReset x done | x <- xs] in done

setReset :: Bit 1 -> Bit 1 -> Bit 1
setReset s r = let out = s .|. reg 0 (out .&. inv r) in out

-- |Run a recipe with a start pulse that is high only on the first
-- cycle of execution, and ignore the finish pulse.
runRecipe :: Recipe -> Module ()
runRecipe r = run (reg 1 0) r >> return ()

-- |Same as 'runRecipe'.  For backwards compatibility.
runOnce :: Recipe -> Module ()
runOnce = runRecipe

-- |Run a recipe, triggered by the given start pulse.
-- Returns the finish pulse.
runRecipeOn :: Bit 1 -> Recipe -> Module (Bit 1)
runRecipeOn = run
