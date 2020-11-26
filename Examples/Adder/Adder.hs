import Blarney

-- | Half adder taking two bits and returning a tuple with the carry bit first
--   and the sum second
halfAdder :: Bit 1 -> Bit 1 -> (Bit 1, Bit 1)
halfAdder x y = (x .&. y, x .^. y)

-- | Full adder taking two bits and an input carry and returning a tuple with
--   the output carry bit first and the sum second
fullAdder :: Bit 1 -> Bit 1 -> Bit 1 -> (Bit 1, Bit 1)
fullAdder x y cin = (cout, s1)
                    where (c0, s0) = halfAdder x y
                          (c1, s1) = halfAdder s0 cin
                          cout = c0 .|. c1

-- | N-bit adder
adder :: Bit n -> Bit n -> Bit n
adder x y = let xs = unsafeToBitList x
                ys = unsafeToBitList y
                ins = zip xs ys
                step (c_prev, s_prev) (a, b) = fullAdder a b c_prev
                outs = tail $ scanl step (0, undefined) ins
            in unsafeFromBitList (map snd outs)
adderM :: Bit n -> Bit n -> Module (Bit n)
adderM x y = return $ adder x y

-- | N-bit bugged adder
doom_adder :: Bit n -> Bit n -> Bit n
doom_adder x y = let xs = unsafeToBitList x
                     ys = unsafeToBitList y
                     ins = zip xs ys
                     step (c_prev, arf_bug_dammit) (a, b) =
                       fullAdder a b arf_bug_dammit
                     outs = tail $ scanl step (0, 0) ins
            in unsafeFromBitList (map snd outs)

isAdderGood :: KnownNat n => (Bit n -> Bit n -> Bit n)
                          -> Bit n -> Bit n -> Module (Bit 1)
isAdderGood test_add x y = return $ test_add x y .==. x + y

top :: Module ()
top = do
  -- Create a register
  cycleCount :: Reg (Bit 4) <- makeReg 0
  separateCount :: Reg (Bit 8) <- makeReg 0
  always do
    -- Increment on every cycle
    cycleCount <== cycleCount.val + 1
    separateCount <== separateCount.val `adder` 4

    -- Display value an every cycle
    display "cycleCount = %0d" (cycleCount.val)
    display "separateCount = %0d" (separateCount.val)

    -- Terminate simulation when count reaches 10
    when (cycleCount.val .==. 10) do
      display "Finished"
      finish
  return ()

main :: IO ()
main = do
  writeVerilogTop top "top" "Adder-Verilog/"
  writeVerilogModule (adderM @2) "adder2" "Adder-Verilog/"
  writeSMT2Script (isAdderGood @2 adder) "goodAdder2" "Adder-SMT2/"
  writeSMT2Script (isAdderGood @16 adder) "goodAdder16" "Adder-SMT2/"
  writeSMT2Script (isAdderGood @1 doom_adder) "brokenAdder1" "Adder-SMT2/"
  writeSMT2Script (isAdderGood @2 doom_adder) "brokenAdder2" "Adder-SMT2/"
  writeSMT2Script (isAdderGood @16 doom_adder) "brokenAdder16" "Adder-SMT2/"
