module Blarney.Recipe 
  ( Recipe(..)
  , run
  ) where

import Blarney.Bit
import Blarney.Prelude
import Blarney.RTL

data Recipe =
    Tick
  | Wait (Bit 1)
  | When (Bit 1) (RTL ())
  | Do [RTL ()]
  | Seq [Recipe]
  | Par [Recipe]
  | If (Bit 1) Recipe
  | While (Bit 1) Recipe

run :: Bit 1 -> Recipe -> RTL (Bit 1)
run go Tick         = return (reg 0 go)
run go (Wait c)     = run go (While (inv c) Tick)
run go (When c a)   = run go (Seq [Wait c, Do [a]])
run go (Do [])      = return go
run go (Do (a:as))  = when go a >> run (reg 0 go) (Do as)
run go (Seq [])     = return go
run go (Seq (r:rs)) = run go r >> run (reg 0 go) (Seq rs)
run go (Par rs)     = mapM (run go) rs >>= return . sync
run go (If c r)     = do
  done <- run (go .&. c) r
  return (done .|. (go .&. inv c))
run go (While c r)  = do
  ready <- makeWire 0
  done <- run (val ready .&. c) r
  ready <== go .|. done
  return (val ready .&. inv c)

sync :: [Bit 1] -> Bit 1
sync [x] = x
sync xs = let done = andList [setReset x done | x <- xs] in done

setReset :: Bit 1 -> Bit 1 -> Bit 1
setReset s r = let out = s .|. reg 0 (out .&. inv r) in out
