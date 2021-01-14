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
prop_add :: KnownNat n => (Bit n -> Bit n -> Bit n) -> Bit n -> Bit n
                       -> Action ()
prop_add adder_imp x y = assert (adder_imp x y .==. x + y)
                                "Adder equivalent to blarney '+' operator"

--------------------------------------------------------------------------------

-- | Sequential full adder
adderSeq x y = res
  where c_in = delay 0 c_out
        res@(c_out, _) = fullAdder x y c_in
doom_adderSeq x y = res
  where c_in = delay 0 s
        res@(c_out, s) = fullAdder x y c_in
-- | prop
prop_addSeq x y = assert (adderSeq x y === adderSeq x y)
                         "Sequential adder equivalent to itself"
prop_brokenAddSeq x y = assert (adderSeq x y === doom_adderSeq x y)
                               "Broken seq adder is equivalent to working one"

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- path to script output directory
  cwd <- getCurrentDirectory
  let smtDir = cwd ++ "/CheckAdder-SMT/"
  -- generate smt2 scripts
  writeSMTScript verifConf (prop_add @2  adder)      "goodAdder2"    smtDir
  writeSMTScript verifConf (prop_add @16 adder)      "goodAdder16"   smtDir
  writeSMTScript verifConf (prop_add @2  doom_adder) "brokenAdder2"  smtDir
  writeSMTScript verifConf (prop_add @16 doom_adder) "brokenAdder16" smtDir
  writeSMTScript verifConf (prop_addSeq)             "goodAddSeq"    smtDir
  writeSMTScript verifConf (prop_brokenAddSeq)       "brokenAddSeq"  smtDir
  where verifConf = dfltVerifyConf
