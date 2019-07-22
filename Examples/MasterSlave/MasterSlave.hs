import Blarney
import Blarney.Stream
import Blarney.Queue

type MulReq  = (Bit 32, Bit 32)
type MulResp = Bit 32

slave :: Stream MulReq -> Module (Stream MulResp)
slave reqs = do
  buffer <- makeQueue

  always do
    when (reqs.canPeek .&. buffer.notFull) do
      consume reqs
      enq buffer (reqs.peek.fst * reqs.peek.snd)

  return (buffer.toStream)

master :: Stream MulResp -> Module (Stream MulReq)
master resps = do
  buffer <- makeQueue

  always do
    when (buffer.notFull) do
      enq buffer (16, 4)

    when (resps.canPeek) do
      consume resps
      display "Response: 0x%08x" (resps.peek)
      finish

  return (buffer.toStream)

makeSlave :: Stream MulReq -> Module (Stream MulResp)
makeSlave = makeInstance "slave"

makeMaster :: Stream MulResp -> Module (Stream MulReq)
makeMaster = makeInstance "master"

top :: Module ()
top = mdo
  resps <- makeMaster reqs
  reqs <- makeSlave resps
  return ()

main :: IO ()
main = do
  writeVerilogTop top "top" "MasterSlave-Verilog/"
  writeVerilogModule slave "slave" "MasterSlave-Verilog/"
  writeVerilogModule master "master" "MasterSlave-Verilog/"
