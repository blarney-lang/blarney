import Blarney
import Blarney.Stmt
import Blarney.SourceSink
import Blarney.ClientServer
import System.Environment

makeIterator :: Bits a =>
  (a -> a) -> (a -> Bit 1) -> Module (Server a a)
makeIterator step done = do
  -- State
  busy  <- makeReg false
  state <- makeReg dontCare

  -- Result ready?
  let ready = done state.val

  -- Update state
  always do
    when (busy.val .&&. inv ready) do
      state <== step state.val

  return
    Server {
      reqs = Sink {
               canPut = inv busy.val
             , put = \req -> do busy <== true
                                state <== req
             }
    , resps = Source {
                canPeek = busy.val .&&. ready
              , peek    = state.val
              , consume = busy <== false
              }
    }

makeRemServer :: (Bits a, Num a, Cmp a) => Module (Server (a, a) (a, a))
makeRemServer = makeIterator step done
  where
    step (x, y) = (x-y, y)
    done (x, y) = x .<. y .||. y .==. 0

makeGCDServer :: (Bits a, Num a, Cmp a) => Module (Server (a, a) (a, a))
makeGCDServer = makeIterator step done
  where
    step (x, y) = if x .>. y then (x - y, y) else (x, y - x)
    done (x, y) = x .==. y


top :: Module ()
top = do
  gcd <- makeGCDServer @(Bit 8)

  runStmt do
    wait gcd.reqs.canPut
    action do
      gcd.reqs.put (110, 66)
    wait gcd.resps.canPeek
    action do
      display "gcd=" (fst gcd.resps.peek)
      gcd.resps.consume
      finish

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "Iterator" "Iterator-Verilog/"
