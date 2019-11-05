module Trap where

-- Blarney imports
import Blarney

-- Pebbles imports
import CSR
import Pipeline

type ExceptionCode = Bit 31

exc_instrAddrMissaligned    :: ExceptionCode = 0
exc_instrAccessFault        :: ExceptionCode = 1
exc_illegalInstr            :: ExceptionCode = 2
exc_breakpoint              :: ExceptionCode = 3
exc_loadAddrMissaligned     :: ExceptionCode = 4
exc_loadAccessFault         :: ExceptionCode = 5
exc_storeAMOAddrMissaligned :: ExceptionCode = 6
exc_storeAMOAccessFault     :: ExceptionCode = 7
exc_eCallFromU              :: ExceptionCode = 8
exc_eCallFromS              :: ExceptionCode = 9
exc_res10                   :: ExceptionCode = 10
exc_eCallFromM              :: ExceptionCode = 11
exc_instrPageFault          :: ExceptionCode = 12
exc_loadPageFault           :: ExceptionCode = 13
exc_res14                   :: ExceptionCode = 14
exc_storeAMOPageFault       :: ExceptionCode = 15

type InterruptCode = Bit 31

int_res0         :: InterruptCode = 0
int_softIrqS     :: InterruptCode = 1
int_res2         :: InterruptCode = 2
int_softIrqM     :: InterruptCode = 3
int_res4         :: InterruptCode = 4
int_timerIrqS    :: InterruptCode = 5
int_res6         :: InterruptCode = 6
int_timerIrqM    :: InterruptCode = 7
int_res8         :: InterruptCode = 8
int_externalIrqS :: InterruptCode = 9
int_res10        :: InterruptCode = 10
int_externalIrqM :: InterruptCode = 11

data TrapCode = Exception ExceptionCode | Interrupt InterruptCode

toCause :: TrapCode -> Bit 32
toCause (Exception e) = 0b0 # e
toCause (Interrupt i) = 0b1 # i

-- |Simple trap function that installs a trap cause and sets the new PC.
-- |Does not handle pc writing from other pipeline stages.
-- |Does not update mstatus.
trap :: State -> CSRUnit -> TrapCode -> Action ()
trap s csr c = do
  csr.mcause <== toCause c
  s.pc <== csr.mepc.val
