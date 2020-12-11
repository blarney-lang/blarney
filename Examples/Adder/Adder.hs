import System.Directory

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
prop_add :: KnownNat n => (Bit n -> Bit n -> Bit n) -> Bit n -> Bit n -> Bit 1
prop_add adder_imp x y = adder_imp x y .==. x + y

--------------------------------------------------------------------------------

-- | Sequential full adder
adderSeq x y = res
  where c_in = delay 0 c_out
        res@(c_out, _) = fullAdder x y c_in
-- | prop
prop_addSeq x y = adderSeq x y === adderSeq x y

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- path to script output directory
  cwd <- getCurrentDirectory
  let smtDir = cwd ++ "/Adder-SMT2/"
  -- generate smt2 scripts
  --writeSMT2Script (prop_add @2  adder)      "goodAdder2"    smtDir
  --writeSMT2Script (prop_add @16 adder)      "goodAdder16"   smtDir
  --writeSMT2Script (prop_add @2  doom_adder) "brokenAdder2"  smtDir
  --writeSMT2Script (prop_add @16 doom_adder) "brokenAdder16" smtDir
  writeSMT2Script (prop_addSeq) "addSeq" smtDir
  -- helper usage message
  putStrLn $ "SMT2 scripts generated under " ++ smtDir
  putStrLn $ "Run an SMT solver such as z3 with an SMT2 script as input:"
  putStrLn $ "    $ z3 " ++ smtDir ++ "goodAdder2.smt2"
  putStrLn $ "This should return \"unsat\". Running:"
  putStrLn $ "    $ z3 " ++ smtDir ++ "brokenAdder2.smt2"
  putStrLn $ "should return \"sat\". Run:"
  putStrLn $ "    $ echo \"(get-model)\" >> "
             ++ smtDir ++ "brokenAdder2.smt2"
  putStrLn $ "    $ z3 " ++ smtDir ++ "brokenAdder2.smt2"
  putStrLn $ "to get a set of input assignments for which the tested property"
             ++ " does not hold."
