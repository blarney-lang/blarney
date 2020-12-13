import Blarney

data MyIfc n = MyIfc { meh0 :: Bit n
                     , meh8 :: Bit 8 } deriving (Generic, Interface)

testMyIfc :: Bit 8 -> Module (MyIfc 0)
testMyIfc val = return $ MyIfc { meh0 = 0, meh8 = val }

makeTestMyIfc :: Bit 8 -> Module (MyIfc 0)
makeTestMyIfc = makeInstance "testMyIfc"

top :: Module ()
top = do
  let a :: Bit 0 = dontCare
  let b :: Bit 0 = dontCare
  someInst <- makeTestMyIfc 10

  always do
    if a .==. b then display "Bit 0 .==. true"
                else display "Bit 0 .==. false"
    if a .!=. b then display "Bit 0 .!=. true"
                else display "Bit 0 .!=. false"
    if  a .<. b then display "Bit 0 .<. true"
                else display "Bit 0 .<. false"
    if a .<=. b then display "Bit 0 .<=. true"
                else display "Bit 0 .<=. false"
    display "concat (10 :: Bit 8) with a Bit 0: " ((someInst.meh8) # a)
    display "zeroExtend a Bit 0 to a Bit 4: " (zeroExtend a :: Bit 4)
    display "signExtend a Bit 0 to a Bit 4: " (signExtend a :: Bit 4)
    display "displaying a Bit 0: " a
    finish

main :: IO ()
main = do
  writeVerilogTop top "top" "Bit0-Verilog/"
  writeVerilogModule testMyIfc "testMyIfc" "Bit0-Verilog/"
