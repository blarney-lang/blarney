module Blarney.FloatingPoint (
  addFP32, subFP32, mulFP32, divFP32,
  sqrtFP32, cmpFP32, toFP32, fromFP32
) where

import Blarney
import Blarney.Unbit

-- Interface to Verilog floating-point modules
-- ===========================================

-- To account for latency, each module takes a token passed it all the
-- way through to the result.  For example, if the token contains a
-- "go" pulse, they you'll get a "done" pulse on the output.

addFP32 :: KnownNat n => (Bit n, Bit 32, Bit 32) -> RTL (Bit n, Bit 32)
addFP32 (t, a, b) = makeInstanceWithParams "FPAdd32" params (t, a, b)
  where params = ["tokenWidth" :-> show (widthOf t)]

subFP32 :: KnownNat n => (Bit n, Bit 32, Bit 32) -> RTL (Bit n, Bit 32)
subFP32 (t, a, b) = makeInstanceWithParams "FPSub32" params (t, a, b)
  where params = ["tokenWidth" :-> show (widthOf t)]

mulFP32 :: KnownNat n => (Bit n, Bit 32, Bit 32) -> RTL (Bit n, Bit 32)
mulFP32 (t, a, b) = makeInstanceWithParams "FPMul32" params (t, a, b)
  where params = ["tokenWidth" :-> show (widthOf t)]

divFP32 :: KnownNat n => (Bit n, Bit 32, Bit 32) -> RTL (Bit n, Bit 32)
divFP32 (t, a, b) =  makeInstanceWithParams "FPDiv32" params (t, a, b)
  where params = ["tokenWidth" :-> show (widthOf t)]

sqrtFP32 :: KnownNat n => (Bit n, Bit 32) -> RTL (Bit n, Bit 32)
sqrtFP32 (t, a) = makeInstanceWithParams "FPSqrt32" params (t, a)
  where params = ["tokenWidth" :-> show (widthOf t)]

cmpFP32 :: KnownNat n => (Bit n, Bit 32, Bit 32) -> RTL (Bit n, Bit 3)
cmpFP32 (t, a, b) = makeInstanceWithParams "FPCmp32" params (t, a, b)
  where params = ["tokenWidth" :-> show (widthOf t)]

toFP32 :: KnownNat n => (Bit n, Bit 32) -> RTL (Bit n, Bit 32)
toFP32 (t, a) = makeInstanceWithParams "FPFromInt32" params (t, a)
  where params = ["tokenWidth" :-> show (widthOf t)]

fromFP32 :: KnownNat n => (Bit n, Bit 32) -> RTL (Bit n, Bit 32)
fromFP32 (t, a) = makeInstanceWithParams "FPToInt32" params (t, a)
  where params = ["tokenWidth" :-> show (widthOf t)]
