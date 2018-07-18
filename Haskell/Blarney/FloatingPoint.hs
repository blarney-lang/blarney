module Blarney.FloatingPoint where

import Blarney

-- Interface to Verilog floating-point modules
-- ===========================================

-- To account for latency, each module takes a "go" trigger and
-- returns a "done" trigger.

addFP32  :: (Bit 1, Bit 32, Bit 32) -> RTL (Bit 1, Bit 32)
addFP32  =  makeInstance "FPAdd32"

subFP32  :: (Bit 1, Bit 32, Bit 32) -> RTL (Bit 1, Bit 32)
subFP32  =  makeInstance "FPSub32"

mulFP32  :: (Bit 1, Bit 32, Bit 32) -> RTL (Bit 1, Bit 32)
mulFP32  =  makeInstance "FPMul32"

divFP32  :: (Bit 1, Bit 32, Bit 32) -> RTL (Bit 1, Bit 32)
divFP32  =  makeInstance "FPDiv32"

sqrtFP32 :: (Bit 1, Bit 32) -> RTL (Bit 1, Bit 32)
sqrtFP32 =  makeInstance "FPSqrt32"

cmpFP32  :: (Bit 1, Bit 32, Bit 32) -> RTL (Bit 1, Bit 3)
cmpFP32  =  makeInstance "FPCmp32"

toFP32   :: (Bit 1, Bit 32) -> RTL (Bit 1, Bit 32)
toFP32   =  makeInstance "FPFromInt32"

fromFP32 :: (Bit 1, Bit 32) -> RTL (Bit 1, Bit 32)
fromFP32 =  makeInstance "FPToInt32"
