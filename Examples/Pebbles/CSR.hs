module CSR where

-- Control/status registers

import Blarney
import Blarney.Queue
import Blarney.Stream

-- +-------------+---------+--------+-------------------------------------+
-- | CSR         | Address | Access | Description                         |
-- +-------------+---------+--------+-------------------------------------+
-- | SimEmit     | 0x800   | W      | Emit word in simulation             |
-- | SimFinish   | 0x801   | W      | Terminate simulator                 |
-- | UARTCanPut  | 0x802   | R      | Can write to UART?                  |
-- | UARTPut     | 0x803   | W      | Write byte to UART                  |
-- | UARTCanGet  | 0x804   | R      | Can read from UART?                 |
-- | UARTGet     | 0x805   | R      | Read byte from UART                 |
-- +-------------+---------+--------+-------------------------------------+

-- CSR address
type CSR = Bit 12

-- CSR unit, providing ability to read and write CSRs
data CSRUnit =
  CSRUnit {
    writeCSR :: CSR -> Bit 32 -> Action ()
  , readCSR :: CSR -> WriteOnly (Bit 32) -> Action ()
  }

makeCSRUnit :: Stream (Bit 8) -> Module (Stream (Bit 8), CSRUnit)
makeCSRUnit uartIn = do
  -- UART output buffer
  uartOut :: Queue (Bit 8) <- makeShiftQueue 1

  -- Handle CSR writes
  let writeCSR csr x =
        switch csr [
          0x800 --> display "0x%08x" x
        , 0x801 --> finish
        , 0x803 --> enq uartOut (truncate x)
        ]

  -- Handle CSR writes
  let readCSR csr result =
        switch csr [
          0x802 --> result <== zeroExtend (uartOut.notFull)
        , 0x804 --> result <== zeroExtend (uartIn.canGet)
        , 0x805 --> do
            result <== zeroExtend (uartIn.value)
            uartIn.get
        ]

  return (uartOut.toStream, CSRUnit writeCSR readCSR)
