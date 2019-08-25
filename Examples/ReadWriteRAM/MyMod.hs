import Blarney

data MyMod = MyMod {
  toggle :: Action (),
  out :: Bit 1
} deriving (Generic, Interface)

top :: Module (MyMod)
top = do
  value :: Reg (Bit 1) <- makeReg 0

  doToggle :: Wire (Bit 1) <- makeWire 0

  always (when (val doToggle) (value <== value.val.inv))

  return MyMod { toggle = doToggle <== 1, out = value.val }

main :: IO ()
main = writeVerilogModule top "toggle" "toggle-verilog"
