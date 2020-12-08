import Blarney

-- | Half adder taking two bits and returning a tuple with the carry bit first
--   and the sum second
halfAdder :: Bit 1 -> Bit 1 -> (Bit 1, Bit 1)
halfAdder x y = (x .&. y, x .^. y)

-- | Full adder taking two bits and an input carry and returning a tuple with
--   the output carry bit first and the sum second
fullAdder :: Bit 1 -> Bit 1 -> Bit 1 -> (Bit 1, Bit 1)
fullAdder x y c_in = (c_out, s1)
  where
    (c0, s0) = halfAdder x y
    (c1, s1) = halfAdder s0 c_in
    c_out = c0 .|. c1

-- | Bit-list adder
listAdder :: Bit 1 -> [Bit 1] -> [Bit 1] -> [Bit 1]
listAdder c_in _ [] = []
listAdder c_in [] _ = []
listAdder c_in (a:as) (b:bs) =
    sum : listAdder c_out as bs
  where
    (c_out, sum) = fullAdder a b c_in

-- | Bit-vector adder
adder :: KnownNat n => Bit n -> Bit n -> Bit n
adder x y = fromBitList $ listAdder 0 (toBitList x) (toBitList y)

-- | N-bit bugged adder
doom_adder :: KnownNat n => Bit n -> Bit n -> Bit n
doom_adder x y = fromBitList $ listAdder 1 (toBitList x) (toBitList y)

-- | Adder property
prop_add :: KnownNat n => (Bit n -> Bit n -> Bit n)
                       -> Bit n -> Bit n -> Module (Bit 1)
prop_add adder x y = return $ adder x y .==. x + y

main :: IO ()
main = do
  writeSMT2Script (prop_add @16 adder) "goodAdder16" "Adder-SMT2/"
  writeSMT2Script (prop_add @1 doom_adder) "brokenAdder1" "Adder-SMT2/"
  writeSMT2Script (prop_add @2 doom_adder) "brokenAdder2" "Adder-SMT2/"
  writeSMT2Script (prop_add @16 doom_adder) "brokenAdder16" "Adder-SMT2/"
