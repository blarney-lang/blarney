import Blarney
import Blarney.Stream
import Blarney.Queue
import System.Environment

type MulReq  = (Bit 32, Bit 32)
type MulResp = Bit 32

slave :: Stream MulReq -> Module (Stream MulResp)
slave reqs = do
  buffer <- makeQueue

  always do
    when (reqs.canPeek .&&. buffer.notFull) do
      reqs.consume
      buffer.enq (fst reqs.peek * snd reqs.peek)

  return (toStream buffer)

master :: Stream MulResp -> Module (Stream MulReq)
master resps = do
  buffer <- makeQueue

  always do
    when buffer.notFull do
      buffer.enq (16, 4)

    when resps.canPeek do
      resps.consume
      display "Response: 0x" (formatHex 8 resps.peek)
      finish

  return (toStream buffer)

makeSlave :: Stream MulReq -> Module (Stream MulResp)
makeSlave = makeBoundary "slave" slave

makeMaster :: Stream MulResp -> Module (Stream MulReq)
makeMaster = makeBoundary "master" master

top :: Module ()
top = mdo
  resps <- makeMaster reqs
  reqs <- makeSlave resps
  return ()

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> do writeVerilogModule slave "slave" "MasterSlave-Verilog/"
                       writeVerilogModule master "master" "MasterSlave-Verilog/"
                       writeVerilogTop top "MasterSlave" "MasterSlave-Verilog/"
