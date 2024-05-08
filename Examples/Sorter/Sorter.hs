import Blarney
import Blarney.Vector (Vec, fromList, toList, vectorise)
import System.Environment

twoSort :: (Bit 8, Bit 8) -> (Bit 8, Bit 8)
twoSort (a, b) = if a .<. b then (a, b) else (b, a)

{-
top :: Module ()
top = do
  display "twoSort (1,2) = " (twoSort (1,2))
  display "twoSort (2,1) = " (twoSort (2,1))
  finish
-}

insert :: Bit 8 -> [Bit 8] -> [Bit 8]
insert x [] = [x]
insert x (y:ys) = small : insert big ys
  where (small, big) = twoSort (x, y)

sort :: [Bit 8] -> [Bit 8]
sort [] = []
sort (x:xs) = insert x (sort xs)

top :: Module ()
top = always do
  let inputs = [3, 4, 1, 0, 2]
  display "sort " inputs " = " (sort inputs)
  finish

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "Sorter" "Sorter-Verilog/"

vecSort :: Vec n (Bit 8) -> Vec n (Bit 8)
vecSort = vectorise sort

gen :: IO ()
gen = writeVerilogModule (vecSort @8) "Sorter8" "Sorter8-Verilog/"
