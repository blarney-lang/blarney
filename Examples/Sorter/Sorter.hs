import Blarney

twoSort :: (Bit 8, Bit 8) -> (Bit 8, Bit 8)
twoSort (a, b) = a .<. b ? ((a, b), (b, a))

{-
top :: Module ()
top = do
  display "twoSort (1,2) = " (twoSort (1,2))
  display "twoSort (2,1) = " (twoSort (2,1))
  finish
-}

bubble :: [Bit 8] -> [Bit 8]
bubble [] = []
bubble [x] = [x]
bubble (x:y:rest) = bubble (small:rest) ++ [big]
  where (small, big) = twoSort (x, y)

sort :: [Bit 8] -> [Bit 8]
sort [] = []
sort (x:xs) = smallest : sort rest
  where (smallest:rest) = bubble (x:xs)

top :: Module ()
top = always do
  let inputs = [3, 4, 1, 0, 2]
  display "sort " inputs " = " (sort inputs)
  finish

main :: IO ()
main = writeVerilogTop top "top" "Sorter-Verilog/"
