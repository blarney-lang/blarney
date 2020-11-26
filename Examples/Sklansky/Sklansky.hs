import Blarney
import qualified Blarney.Vector as V

sklansky :: (a -> a -> a) -> [a] -> [a]
sklansky op [x] = [x]
sklansky op xs = ys' ++ map (last ys' `op`) zs'
  where
    (ys, zs) = halve xs
    ys' = sklansky op ys
    zs' = sklansky op zs
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

--sklanskyM :: KnownNat m => (Bit n -> Bit n -> Bit n) -> V.Vec m (Bit n) -> Module (V.Vec m (Bit n))
--sklanskyM op ins = return $ V.fromList $ sklansky op (V.toList ins)

isSklanskyGood :: forall n m. (Bit n -> Bit n -> Bit n) -> V.Vec m (Bit n) -> Module (Bit 1)
isSklanskyGood op ins = return $ andList $ zipWith (.==.) res0 res1
  where res0 = sklansky op (V.toList ins)
        res1 = scanl1 op (V.toList ins)

main :: IO ()
main = do
  print (scanl1 (+) [1..16])
  print (sklansky (+) [1..16])
  --writeVerilogModule (sklanskyM @4 @3) "sklansky4_3" "Sklansky-Verilog/"
  writeSMT2Script (isSklanskyGood @4 @4 (+)) "goodSklansky4_4" "Sklansky-SMT2/"
