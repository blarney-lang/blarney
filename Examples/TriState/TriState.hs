import Blarney
import System.Environment

--deviceA :: forall a. (Num a, Cmp a, Bits a) => TriStateWire a -> Module ()
deviceA :: TriStateWire (Bit 8) -> Module ()
deviceA tsWire = do
  -- count cycles to drive a state machine
  cycleCount :: Reg (Bit 4) <- makeReg 0
  always do cycleCount <== cycleCount.val + 1
  -- state machine
  always do
    when (cycleCount.val .==. 1) do
      display "device A sends 10"
      tsWire <== 10
    when (cycleCount.val .==. 4) do
      display "device A sends 20"
      tsWire <== 20
    when (tsWire.val .==. 30) do
      display "device A received 30"
      finish

--deviceB :: (Num a, Cmp a, Bits a) => TriStateWire a -> Module ()
deviceB :: TriStateWire (Bit 8) -> Module ()
deviceB tsWire = do
  -- end condition register
  send30 <- makeReg false
  always do
    when (tsWire.val .==. 10) do
      display "device B received 10"
    when (tsWire.val .==. 20) do
      display "device B received 20"
      send30 <== true
    when (send30.val) do
      display "device B sends 30"
      tsWire <== 30

--makeDeviceA :: (Num a, Cmp a, Bits a) => TriStateWire a -> Module ()
makeDeviceA :: TriStateWire (Bit 8) -> Module ()
makeDeviceA = makeBoundary "deviceA" deviceA

--makeDeviceB :: (Num a, Cmp a, Bits a) => TriStateWire a -> Module ()
makeDeviceB :: TriStateWire (Bit 8) -> Module ()
makeDeviceB = makeBoundary "deviceB" deviceB

top :: Module ()
top = do
  tsWire <- makeTriStateWire @(Bit 8)
  --deviceA tsWire
  --deviceB tsWire
  makeDeviceA tsWire
  makeDeviceB tsWire
  return ()

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "TriState" "TriState-Verilog/"
