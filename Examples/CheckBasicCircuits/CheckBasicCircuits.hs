import System.Directory

import Blarney

-- This is an example derived from the sections 5.4 and 5.5 of Koen Claessen 's thesis
-- http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=4958B8E53D3385B0F641ACF6683B7929?doi=10.1.1.110.5587&rep=rep1&type=pdf

-- Here is a set of util components / connection patterns
---------------------------------------------------------

edge :: Bit 1 -> Bit 1
edge inpt = inpt .^. inpt'
  where inpt' = delay 0 inpt

toggle :: Bit 1 -> Bit 1
toggle inpt = outpt
  where outpt = inpt ? (inv prev, prev)
        prev = delay 0 outpt

delayN :: Bits a => Integer -> a -> a -> a
delayN 0 _ inpt = inpt
delayN n initSt inpt = outpt
  where outpt = delay initSt prev
        prev = delayN (n - 1) initSt inpt

puls :: Integer -> Bit 1
puls n = outpt
  where outpt = delayN (n - 1) 0 prev
        prev = delay 1 outpt

-- | Half adder taking two bits and returning a tuple with the carry bit first
--   and the sum second
halfAdder :: (Bit 1, Bit 1) -> (Bit 1, Bit 1)
halfAdder (a, b) = (a .&. b, a .^. b)

-- | Full adder taking two bits and an input carry and returning a tuple with
--   the output carry bit first and the sum second
fullAdder :: (Bit 1, Bit 1) -> Bit 1 -> (Bit 1, Bit 1)
fullAdder (a, b) cIn = (cOut, s1)
  where (c0, s0) = halfAdder (a, b)
        (c1, s1) = halfAdder (s0, cIn)
        cOut = c0 .|. c1

-- | Sequential adder taking two bits as input and returning their sum, latching
--   the carry for the next addition
seqAdder :: (Bit 1, Bit 1) -> Bit 1
seqAdder (a, b) = s
  where cIn = delay 0 cOut
        (cOut, s) = fullAdder (a, b) cIn

-- | Connection pattern for making a serial "row" from a sequential circuit
seqRow :: (Num b, Bits b) => (a -> b -> (b, c)) -> a -> c
seqRow circ inpt = outpt
  where cIn = delay 0 cOut
        (cOut, outpt) = circ inpt cIn

seqAdder' = seqRow fullAdder

-- | connection pattern for making a serial "row" from a sequential circuit and
--   a reset state
seqResetRow :: (Num b, Bits b) => (a -> b -> (b, c)) -> Bit 1 -> a -> c
seqResetRow circ rst inpt = outpt
  where cIn = delay 0 carry
        carry = rst ? (cOut, 0)
        (cOut, outpt) = circ inpt cIn

seqResetAdder = seqResetRow fullAdder

seqPeriodRow :: (Num b, Bits b) => Integer -> (a -> b -> (b, c)) -> a -> c
seqPeriodRow n circ inpt = outpt
  where rst = puls n
        outpt = seqResetRow circ rst inpt

seqPeriodAdder n = seqPeriodRow n fullAdder

-- Here are some properties we want to verify
---------------------------------------------

-- provable with induction of depth 1
prop_ToggleEdgeIdentity inpt = assert ok "ToggleEdgeIdentity property"
  where mid = toggle inpt
        outpt = edge mid
        ok = outpt .<==>. inpt

prop_ToggleTogglesWhenHigh inpt = assert ok "ToggleTogglesWhenHigh property"
  where outpt = toggle inpt
        outpt' = delay 0 outpt
        differ = outpt .!=. outpt'
        ok = inpt .==>. differ

-- NOT provable with induction of depth 1
-- need the notion of induction depth
prop_Toggle_vs_Puls = assert ok "Toggle Vs Puls property"
  where outpt1 = toggle 1
        outpt2 = puls 2
        ok = inv (outpt1 .<==>. outpt2)

-- NOT provable with simple induction of depth k
-- need the notion of restricted states
prop_AdderPeriod2 inpts = assert ok "Adder period 2 property"
  where sum1 = seqPeriodAdder 2 inpts
        two = delay 0 (inv two)
        sum2 = seqResetAdder two inpts
        ok = sum1 .<==>. sum2

--prop_SameSeqAdder inpts = assert ok "Seq addres equivalent"
--  where outpt1 = seqAdder inpts
--        outpt2 = seqAdder' inpts
--        ok = outpt1 .<==>. outpt2

prop_all i0 i1 i2 = do
  prop_ToggleEdgeIdentity i0
  prop_ToggleTogglesWhenHigh i1
  prop_Toggle_vs_Puls
  prop_AdderPeriod2 i2

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- path to script output directory
  cwd <- getCurrentDirectory
  let oDir = cwd ++ "/CheckBasicCircuits-SMT/"
  -- generate smt2 scripts
  writeSMTScript cnfA (prop_ToggleEdgeIdentity)
                 "prop_ToggleEdgeIdentity" oDir
  writeSMTScript cnfA (prop_ToggleTogglesWhenHigh)
                 "prop_ToggleTogglesWhenHigh" oDir
  writeSMTScript cnfA (prop_Toggle_vs_Puls)
                 "prop_Toggle_vs_Puls" oDir
  writeSMTScript cnfA (prop_AdderPeriod2)
                 "prop_AdderPeriod2" oDir
  --writeSMTScript cnfA (prop_all)
  --               "prop_all" oDir
  -- verify
  --verifyWith cnfB prop_ToggleEdgeIdentity
  --verifyWith cnfB prop_ToggleTogglesWhenHigh
  --verifyWith cnfB prop_Toggle_vs_Puls
  --verifyWith cnfB prop_AdderPeriod2
  verifyWith cnfB prop_all
  where cnfA = dfltVerifyConf { verifyConfMode = Induction (fixedDepth 5) True }
        cnfB = dfltVerifyConf { verifyConfMode = Induction (IncreaseFrom 1) True
                              , verifyConfUser =
                                  dfltUserConf { userConfInteractive = True } }
