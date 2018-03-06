import Blarney

-- RAM interface
data RAM a d =
  RAM {
    load     :: a -> RTL ()
  , store    :: a -> d -> RTL ()
  , out      :: d
  }

-- RAM module
makeRAM :: (Bits a, Bits d) => RTL (RAM a d)
makeRAM = do
  -- Address bus and data bus and write-enable
  addrBus :: Wire a <- makeWire (unpack 0)
  dataBus :: Wire d <- makeWire (unpack 0)
  writeEn :: Wire (Bit 1) <- makeWire 0

  -- RAM primitive
  let output = unpack (ram (pack (val addrBus),
                         pack (val dataBus),
                           val writeEn))

  -- Methods
  let load a = do
        addrBus <== a

  let store a d = do
        addrBus <== a
        dataBus <== d
        writeEn <== 1

  -- Return interface
  return (RAM load store output)

-- Top-level module
top :: RTL ()
top = do
  -- RAM
  ram :: RAM (Bit 8) (Bit 4) <- makeRAM

  -- Counter
  i :: Reg (Bit 8) <- makeReg 0

  -- Simple test sequence
  let testSeq =
        Seq [
          While (val i .<. 100) $
            Do [
              store ram (val i) (lower (val i)),
              i <== val i + 1
            ],
          Do [ i <== 0 ],
          While (val i .<. 100) $
            Do [
              load ram (val i),
              display "ram[" (val i) "] = " (out ram),
              i <== val i + 1
            ]
        ]

  done <- run (reg 1 0) testSeq

  when done finish

  return ()

-- Main function
main :: IO ()
main = netlist top >>= writeCXX "/tmp/ram"
