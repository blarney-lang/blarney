import Blarney

-- | Isolate first hot bit in a bit vector
firstHot :: KnownNat n => Bit n -> Bit n
firstHot x = x .&. (inv x + 1)

-- | Check that 'firstHot' isolates a bit that is indeed hot
prop_hotCommon :: KnownNat n => Bit n -> Bit 1
prop_hotCommon x = (x .&. firstHot x) .==. firstHot x

-- | Count the number of ones in a bit vector
countOnes :: (KnownNat n, 1 <= n) => Bit n -> Bit n
countOnes x = tree (+) 0 $ map zeroExtend $ toBitList x

-- | Check that 'firstHot' returns a one-hot vector
prop_oneHot :: (KnownNat n, 1 <= n) => Bit n -> Bit 1
prop_oneHot x = countOnes (firstHot x) .==. (if x .==. 0 then 0 else 1)

-- | Assert properties for 8-bit vectors
prop_firstHot :: Bit 8 -> Module ()
prop_firstHot x = always do
  assert (prop_oneHot x) "prop_oneHot"
  assert (prop_hotCommon x) "prop_hotCommon"

main :: IO ()
main = do
  writeSMTScript dfltVerifyConf prop_firstHot "prop_firstHot" "./"
