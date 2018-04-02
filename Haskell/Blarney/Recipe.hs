{-# LANGUAGE GADTs, DataKinds #-}

module Blarney.Recipe 
  ( Recipe(..)
  , run
  ) where

import Prelude
import Blarney.Bit
import Blarney.Prelude
import Blarney.RTL
import Blarney.Bits
import Blarney.IfThenElse
import Data.Maybe

infixl 1 :=

-- Abstract syntax
data Recipe where
  Tick  :: Recipe
  (:=)  :: (Var v, Bits a) => v a -> a -> Recipe
  Wait  :: Bit 1 -> Recipe
  When  :: Bit 1 -> RTL () -> Recipe
  Do    :: [RTL ()] -> Recipe
  Seq   :: [Recipe] -> Recipe
  Par   :: [Recipe] -> Recipe
  If    :: Bit 1 -> Recipe -> Recipe
  While :: Bit 1 -> Recipe -> Recipe

-- Is time taken by given statement known?
known :: Recipe -> Bool
known r = isJust (time r)

-- Static timing analysis
time :: Recipe -> Maybe Int
time Tick = Just 1
time (v := e) = Just 1
time (Seq rs) = sum `fmap` mapM time rs
time (Par rs) = foldr max 0 `fmap` mapM time rs
time other = Nothing

-- Return index of the slowest recipe
slowest :: [Recipe] -> Int 
slowest = snd . maximum . flip zip [0..] . map time

-- Semantics
run :: Bit 1 -> Recipe -> RTL (Bit 1)
run go Tick         = return (reg 0 go)
run go (v := x)     = run go (Do [v <== x])
run go (Wait c)     = run go (While (inv c) Tick)
run go (When c a)   = run go (Seq [Wait c, Do [a]])
run go (Do [])      = return go
run go (Do (a:as))  = when go a >> run (reg 0 go) (Do as)
run go (Seq [])     = return go
run go (Seq (r:rs)) = do { done <- run go r; run done (Seq rs) }
run go (Par rs)     = do
  dones <- mapM (run go) rs
  return (if all known rs then dones !! slowest rs else sync dones)
run go (If c r)     = do
  done <- run (go .&. c) r
  return (done .|. (go .&. inv c))
run go (While c r)  = do
  ready <- makeWire
  done <- run (val ready .&. c) r
  ready <== go .|. done
  return (val ready .&. inv c)

sync :: [Bit 1] -> Bit 1
sync [x] = x
sync xs = let done = andList [setReset x done | x <- xs] in done

setReset :: Bit 1 -> Bit 1 -> Bit 1
setReset s r = let out = s .|. reg 0 (out .&. inv r) in out
