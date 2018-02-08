module Blarney.Util where

log2 :: Int -> Int
log2 1 = 0
log2 n = 1 + log2 (n `div` 2)
