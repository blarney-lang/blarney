import Blarney
import Blarney.FloatingPoint

top :: RTL ()
top = do
  -- Single-cycle "go" pulse
  let go = reg 1 0

  -- Value 2.0 as a single-precision float
  let two = 0x40000000

  -- Floating-point add
  (done, result) <- addFP32 (go, two, two)
  when done (display "add = " result)

  -- Floating-point subtract
  (done, result) <- subFP32 (go, two, two)
  when done (display "sub = " result)

  -- Floating-point multiply
  (done, result) <- mulFP32 (go, two, two)
  when done (display "mul = " result)

  -- Floating-point divide
  (done, result) <- divFP32 (go, two, two)
  when done (display "div = " result)

  -- Floating-point compare
  (done, result) <- cmpFP32 (go, two, two)
  when done (display "cmp = " result)

  -- Floating-point convert from int
  (done, result) <- toFP32 (go, two)
  when done (display "fromInt = " result)

  -- Floating-point convert to int
  (done, result) <- fromFP32 (go, two)
  when done (display "toInt = " result)

  -- Floating-point square root
  (done, result) <- sqrtFP32 (go, two)
  when done (display "sqrt = " result)

  count :: Reg (Bit 8) <- makeRegInit 0
  count <== count.val + 1
  when (count.val .==. 255) finish

main :: IO ()
main = emitVerilogTop top "top" "FP-Verilog/"
