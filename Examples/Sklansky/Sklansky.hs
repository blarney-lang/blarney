import Blarney
import qualified Blarney.Vector as V

-- | Sklanksy parallel prefix network (assumes power-of-2 list size)
sklansky :: (a -> a -> a) -> [a] -> [a]
sklansky op [x] = [x]
sklansky op xs = ys' ++ map (last ys' `op`) zs'
  where
    (ys, zs) = halve xs
    ys' = sklansky op ys
    zs' = sklansky op zs

-- | Split a list into two halves
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

-- | Does parallel scan equal sequential scan for given operator?
prop_scan :: forall n m.
     (Bit n -> Bit n -> Bit n)
  -> V.Vec m (Bit n) -> Module (Bit 1)
prop_scan op ins = return equal
  where
    res0 = sklansky op (V.toList ins)
    res1 = scanl1 op (V.toList ins)
    equal = andList (zipWith (.==.) res0 res1)

main :: IO ()
main = do
  print (scanl1 (+) [1..16])
  print (sklansky (+) [1..16])
  writeSMT2Script (prop_scan @4 @4 (+)) "goodSklansky4_4" "Sklansky-SMT2/"
