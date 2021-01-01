import System.Directory

import Blarney

-- | Half adder taking two bits and returning a tuple with the carry bit first
--   and the sum second
halfAdder :: Bit 1 -> Bit 1 -> (Bit 1, Bit 1)
halfAdder a b = (a .&. b, a .^. b)

-- | Full adder taking two bits and an input carry and returning a tuple with
--   the output carry bit first and the sum second
fullAdder :: Bit 1 -> Bit 1 -> Bit 1 -> (Bit 1, Bit 1)
fullAdder a b cIn = (cOut, s1)
  where (c0, s0) = halfAdder a b
        (c1, s1) = halfAdder s0 cIn
        cOut = c0 .|. c1

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

testAssert :: KnownNat n => Bit n -> Bit n -> Module ()
testAssert x y = always do assert (prop_add adder x y) "prop_add"
                                  "the prop_add property"
                           assert (prop_add doom_adder x y) "doom_prop_add"
                                  "the doom_prop_add property"

--------------------------------------------------------------------------------

-- | Sequential full adder
adderSeq x y = res
  where c_in = delay 0 c_out
        res@(c_out, _) = fullAdder x y c_in
doom_adderSeq x y = res
  where c_in = delay 0 s
        res@(c_out, s) = fullAdder x y c_in
-- | prop
prop_addSeq x y = adderSeq x y === adderSeq x y
prop_brokenAddSeq x y = adderSeq x y === doom_adderSeq x y

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- path to script output directory
  cwd <- getCurrentDirectory
  let smtDir = cwd ++ "/Adder-SMT2/"
  let verilogDir = cwd ++ "/Adder-Verilog/"
  -- verilog
  writeVerilogModule (testAssert @4) "testAssert" verilogDir
  -- generate smt2 scripts
  writeSMT2Script verifConf (prop_add @2  adder)      "goodAdder2"    smtDir
  writeSMT2Script verifConf (prop_add @16 adder)      "goodAdder16"   smtDir
  writeSMT2Script verifConf (prop_add @2  doom_adder) "brokenAdder2"  smtDir
  writeSMT2Script verifConf (prop_add @16 doom_adder) "brokenAdder16" smtDir
  writeSMT2Script verifConf (prop_addSeq)       "goodAddSeq"   smtDir
  writeSMT2Script verifConf (prop_brokenAddSeq) "brokenAddSeq" smtDir
  writeSMT2Script verifConf (testAssert @4) "testAssert" smtDir
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
  where verifConf = dfltVerifyConf
