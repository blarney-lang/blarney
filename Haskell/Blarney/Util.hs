{-|
Module      : Blarney.Util
Description : Utility functions
Copyright   : (c) Matthew Naylor, 2019
License     : GPL-3 
Maintainer  : mattfn@gmail.com

A group of functions that I can't think of a better name for right now.
-}
module Blarney.Util where

import Prelude

log2 :: Int -> Int
log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

log2ceil :: Int -> Int
log2ceil n
  | n <= 0 = error "log2ceil: argument <= 0"
  | n == 1 = 1
  | otherwise = 1 + log2 (n-1)
