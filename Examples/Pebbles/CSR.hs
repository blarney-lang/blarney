module CSR where

-- Control/status registers

import Blarney
import Blarney.Queue
import Blarney.Stream

-- +-------------+---------+--------+-------------------------------------+
-- | CSR         | Address | Access | Description                         |
-- +-------------+---------+--------+-------------------------------------+
-- | mstatus     | 0x300   | MRW    | Machine status register             |
-- | mepc        | 0x341   | MRW    | Machine exception program counter   |
-- | mcause      | 0x342   | MRW    | Machine trap cause                  |
-- | SimEmit     | 0x800   | W      | Emit word in simulation             |
-- | SimFinish   | 0x801   | W      | Terminate simulator                 |
-- | UARTCanPut  | 0x802   | R      | Can write to UART?                  |
-- | UARTPut     | 0x803   | W      | Write byte to UART                  |
-- | UARTCanGet  | 0x804   | R      | Can read from UART?                 |
-- | UARTGet     | 0x805   | R      | Read byte from UART                 |
-- +-------------+---------+--------+-------------------------------------+

-- CSR address
type CSRIdx = Bit 12

-- CSR unit, providing ability to read and write CSRs
data CSRUnit =
  CSRUnit { mstatus  :: Reg (Bit 32)
          , mepc     :: Reg (Bit 32)
          , mcause   :: Reg (Bit 32)
          , writeCSR :: CSRIdx -> Bit 32 -> Action ()
          , readCSR  :: CSRIdx -> WriteOnly (Bit 32) -> Action ()
          }

makeCSRUnit :: Stream (Bit 8) -> Module (Stream (Bit 8), CSRUnit)
makeCSRUnit uartIn = do
  -- UART output buffer
  uartOut :: Queue (Bit 8) <- makeShiftQueue 1

  -- Cycle counter
  cycleCount :: Reg (Bit 32) <- makeReg 0
  always do cycleCount <== cycleCount.val + 1

  -- Standard RISCV CSRs
  -- TODO: deal with proper types and legal values
  mstatus :: Reg (Bit 32) <- makeReg 0
  mepc    :: Reg (Bit 32) <- makeReg 0
  mcause  :: Reg (Bit 32) <- makeReg 0

  -- Handle CSR writes
  let writeCSR csridx x =
        switch csridx [
          0x300 --> mstatus <== x
        , 0x341 --> mepc    <== x
        , 0x342 --> mcause  <== x
        , 0x800 --> display (cycleCount.val) ": 0x%08x" x
        , 0x801 --> finish
        , 0x803 --> enq uartOut (truncate x)
        ]

  -- Handle CSR writes
  let readCSR csridx result =
        switch csridx [
          0x300 --> result <== mstatus.val
        , 0x341 --> result <== mepc.val
        , 0x342 --> result <== mcause.val
        , 0x802 --> result <== zeroExtend (uartOut.notFull)
        , 0x804 --> result <== zeroExtend (uartIn.canPeek)
        , 0x805 --> do result <== zeroExtend (uartIn.peek)
                       uartIn.consume
        ]

  return (uartOut.toStream, CSRUnit { mstatus  = mstatus
                                    , mepc     = mepc
                                    , mcause   = mcause
                                    , writeCSR = writeCSR
                                    , readCSR  = readCSR
                                    })
