-- Emit netlist in C format

module Blarney.EmitC
  ( printC
  , writeC
  , sequentialise
  ) where

import Blarney.Unbit
import System.IO
import Data.Bits
import qualified Data.Set as S
import qualified Data.Map as M

-- Emit C code to standard out
printC :: [Net] -> IO ()
printC = hWriteC stdout

-- Emit C code to file
writeC :: String -> [Net] -> IO ()
writeC filename netlist = do
  h <- openFile filename WriteMode
  hWriteC h netlist
  hClose h

-- Extract state variables (that are updated on each cycle) from net
getStateVars :: Net -> [(WireId, Width)]
getStateVars net =
  case netPrim net of
    Register i w   -> [((netInstId net, 0), w)]
    RegisterEn i w -> [((netInstId net, 0), w)]
    other          -> []

-- This pass introduces temporary register variables, where necessary,
-- so that parallel register updates can be performed sequentially
sequentialise :: [Net] -> [Net]
sequentialise nets = intro (length nets) M.empty nets
  where
    intro id mod [] = []
    intro id mod (net:nets)
      | null stateVars = net : intro id mod nets
      | otherwise      = new ++ [net {netInputs = ins}] ++ intro id' mod' nets
      where
        stateVars       = getStateVars net
        mod'            = M.union (M.fromList stateVars) mod
        (id', new, ins) = replace id (netInputs net)

        replace id [] = (id, [], [])
        replace id (i:is) =
          let (id0, new0, wire)  = rep id i
              (id1, new1, wires) = replace id0 is
          in  (id1, new0 ++ new1, wire:wires)

        rep id i =
          case M.lookup i mod of
            Nothing -> (id, [], i)
            Just w  -> let net = Net { netPrim = Identity w
                                     , netInstId = id
                                     , netInputs = [i]
                                     , netOutputWidths = [w] }
                       in  (id+1, [net], (id, 0))

hWriteC :: Handle -> [Net] -> IO ()
hWriteC h netlistOrig = do
    let netlist = sequentialise netlistOrig
    emit "#include <stdio.h>\n"
    emit "#include <stdlib.h>\n"
    emit "#include <stdint.h>\n"
    emit "#include <BitVec.h>\n\n"
    emit "struct State {\n"
    mapM_ emitDecls netlist
    emit "};\n\n"
    emit "State* createState() {\n"
    emit "State* s = calloc(1, sizeof(State));\n"
    mapM_ emitInits netlist
    emit "return s;\n}\n\n"
    emit "int main() {\n"
    emit "State* s = createState();\n"
    emit "while (1) {\n"
    mapM_ emitInst netlist
    emit "}\n"
    emit "return 0;\n}\n"
    --emit "always @(posedge clock) begin\n"
    --mapM_ emitAlways netlist
    --emit "end\n"
    --emit "endmodule\n"

  where
    emit = hPutStr h

    emitWirePlain (instId, outNum) = do
      emit "v"
      emit (show instId)
      emit "_"
      emit (show outNum)

    emitWire wire = do
      emit "s->"
      emitWirePlain wire

    emitInput = emitWire

    isStdWidth w = w `elem` [8, 16, 32, 64]

    typeOf w
      | w <= 8    = "uint8_t"
      | w <= 16   = "uint16_t"
      | w <= 32   = "uint32_t"
      | w <= 64   = "uint64_t"
      | otherwise = "uint32_t*"

    numChunks w = (w+31) `div` 32

    mask w = let i = (1 :: Integer) `shiftL` w in
               show (i - 1) ++ (if w > 32 then "ull" else "")

    emitDecl wire w
      | w > 64 = do
          emit "uint32_t "
          emitWirePlain wire
          emit ("[" ++ show (numChunks w) ++ "];\n")
      | otherwise = do
          emit (typeOf w)
          emit " "
          emitWirePlain wire
          emit ";\n"

    emitDecls net =
      sequence_ [ emitDecl (netInstId net, n) w
                | (n, w) <- zip [0..] (netOutputWidths net) ]

    emitInit :: WireId -> Integer -> Width -> IO ()
    emitInit wire i w
      | i == 0 = return ()
      | w <= 64 = do
          emitWire wire
          emit " = "
          emit (show i)
          emit (if w > 32 then "ull;\n" else ";\n")
      | otherwise =
          emitInitBU wire i 0 (numChunks w)

    emitInitBU wire i c 0 = return ()
    emitInitBU wire i c n
      | lower == 0 = emitInitBU wire (i `shiftR` 32) (c+1) (n-1)
      | otherwise = do
          emitWire wire
          emit ("[" ++ show c ++ "] = " ++ show lower ++ ";\n")
          emitInitBU wire (i `shiftR` 32) (c+1) (n-1)
      where lower = i .&. 0xffffffff

    emitInits net =
      case netPrim net of
        Const w i      -> emitInit (netInstId net, 0) i w
        Register i w   -> emitInit (netInstId net, 0) i w
        RegisterEn i w -> emitInit (netInstId net, 0) i w
        other          -> return ()

    emitInfixOpInst op bigOp net w applyMask
      | w <= 64 = do
          emitWire (netInstId net, 0)
          emit " = ("
          emitInput (netInputs net !! 0)
          emit " " >> emit op >> emit " "
          emitInput (netInputs net !! 1)
          emit ")"
          if not (isStdWidth w) && applyMask
            then emit (" & " ++ mask w)
            else return ()
          emit ";\n"
      | otherwise = do
          emit bigOp
          emit "("
          emitInput (netInputs net !! 0)
          emit ","
          emitInput (netInputs net !! 1)
          emit ","
          emitWire (netInstId net, 0)
          emit ","
          emit (show w ++ ");\n")

    emitPrefixOpInst op bigOp net w applyMask
      | w <= 64 = do
          emitWire (netInstId net, 0)
          emit " = "
          emit op
          emit "("
          emitInput (netInputs net !! 0)
          emit ")"
          if not (isStdWidth w) && applyMask
            then emit (" & " ++ mask w)
            else return ()
          emit ";\n"
      | otherwise = do
          emit bigOp
          emit "("
          emitInput (netInputs net !! 0)
          emit ","
          emitWire (netInstId net, 0)
          emit ","
          emit (show w ++ ");\n")

    emitCmpInst op bigOp net w
      | w <= 64 = do
          emitWire (netInstId net, 0)
          emit " = "
          emitInput (netInputs net !! 0)
          emit op
          emitInput (netInputs net !! 1)
          emit ";\n"
      | otherwise = do
          emitWire (netInstId net, 0)
          emit " = "
          emit bigOp
          emit "("
          emitInput (netInputs net !! 0)
          emit ","
          emitInput (netInputs net !! 1)
          emit ","
          emit (show w ++ ");\n")

    emitReplicateInst w net
      | w <= 64 = do
          emitWire (netInstId net, 0)
          emit " = "
          emitInput (netInputs net !! 0)
          emit " ? "
          emit (mask w)
          emit " : "
          emit (if w <= 32 then "0" else "0ull")
          emit ";\n"
      | otherwise = do
          emit "replicateBU("
          emitInput (netInputs net !! 0)
          emit ","
          emitWire (netInstId net, 0)
          emit ","
          emit (show w ++ ");\n")

{-

    emitMuxInst net = do
      emit "assign "
      emitWire (netInstId net, 0)
      emit " = "
      emitInput (netInputs net !! 0)
      emit " ? "
      emitInput (netInputs net !! 1)
      emit " : "
      emitInput (netInputs net !! 2)
      emit ";\n"

    emitConcatInst net = do
      emit "assign "
      emitWire (netInstId net, 0)
      emit " = {"
      emitInput (netInputs net !! 0)
      emit ","
      emitInput (netInputs net !! 1)
      emit "};\n"

    emitSelectBitsInst net hi lo = do
      emit "assign "
      emitWire (netInstId net, 0)
      emit " = "
      emitInput (netInputs net !! 0)
      emit "["
      emit (show hi)
      emit ":"
      emit (show lo)
      emit "];\n"

    emitZeroExtendInst net wi wo = do
      emit "assign "
      emitWire (netInstId net, 0)
      emit " = "
      emit "{{"
      emit (show (wo-wi))
      emit "{1'b0}},"
      emitInput (netInputs net !! 0)
      emit "};\n"

    emitSignExtendInst net wi wo = do
      emit "assign "
      emitWire (netInstId net, 0)
      emit " = "
      emit "{"
      emit (show (wo-wi))
      emit "{"
      emitInput (netInputs net !! 0)
      emit "["
      emit (show (wi-1))
      emit "],"
      emitInput (netInputs net !! 0)
      emit "};\n"
-}

    emitInst net =
      case netPrim net of
        Const w i         -> return ()
        Add w             -> emitInfixOpInst "+" "addBU" net w True
        Sub w             -> emitInfixOpInst "-" "subBU" net w True
        Mul w             -> emitInfixOpInst "*" "mulBU" net w True
        Div w             -> emitInfixOpInst "/" "divBU" net w False
        Mod w             -> emitInfixOpInst "%" "modBU" net w False
        Not w             -> emitPrefixOpInst "~" "notBU" net w True
        And w             -> emitInfixOpInst "&" "andBU" net w False
        Or  w             -> emitInfixOpInst "|" "orBU" net w False
        Xor w             -> emitInfixOpInst "^" "xorBU" net w False
        ShiftLeft w       -> emitInfixOpInst "<<" "leftBU" net w True
        ShiftRight w      -> emitInfixOpInst ">>" "rightBU" net w False
        Equal w           -> emitCmpInst "==" "eqBU" net w
        NotEqual w        -> emitCmpInst "!=" "neqBU" net w
        LessThan w        -> emitCmpInst "<" "ltBU" net w
        LessThanEq w      -> emitCmpInst "<=" "leBU" net w
        Register i w      -> return ()
        RegisterEn i w    -> return ()
        ReplicateBit w    -> emitReplicateInst w net
        other             -> return ()
{-
        ZeroExtend wi wo  -> emitZeroExtendInst net wi wo
        SignExtend wi wo  -> emitSignExtendInst net wi wo
        SelectBits hi lo  -> emitSelectBitsInst net hi lo
        Concat aw bw      -> emitConcatInst net
        Mux w             -> emitMuxInst net
        CountOnes w       -> emitPrefixOpInst "countOnes" net
        Identity w        -> emitPrefixOpInst "" net
        Display args      -> return ()
        Finish            -> return ()
        Custom p is os ps ->
          error "Custom operators not yet supported in C backend"
-}
