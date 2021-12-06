import Blarney
import Blarney.Union
import Blarney.Option
import System.Environment

data MemLoadReq a =
  MemLoadReq {
    addr :: a
  }
  deriving (Generic, Bits)

data MemStoreReq a d =
  MemStoreReq {
    addr :: a
  , storeData :: d
  }
  deriving (Generic, Bits)

type MemReq a d = Union [MemLoadReq a, MemStoreReq a d]

getAddr :: forall a d. (Bits a, Bits d) => MemReq a d -> a
getAddr req = load.valid ? (load.val.addr, store.val.addr)
  where
    load = fromUnion @(MemLoadReq a) req
    store = fromUnion @(MemStoreReq a d) req

top :: Module ()
top = do
  let req :: MemReq (Bit 8) (Bit 32) =
        toUnion (MemLoadReq { addr = 100 :: Bit 8 })

  always do
    display (getAddr req)
    finish

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "Union" "Union-Verilog/"
