-- Emit netlist in Verilog format

module Blarney.Verilog
  ( emitVerilog
  ) where

import Blarney.Unbit
import System.IO

emitVerilog :: [Net] -> IO ()
emitVerilog = hWriteVerilog stdout

hWriteVerilog :: Handle -> [Net] -> IO ()
hWriteVerilog h netlist = do
    emit "module top (input wire clock);\n"
    mapM_ emitDecl netlist
    mapM_ emitInst netlist
    emit "always @(posedge clock) begin\n"
    mapM_ emitAlways netlist
    emit "end\n"
    emit "endmodule\n"
  where
    emit = hPutStr h

    emitWire (instId, outNum) = do
      emit "v"
      emit (show instId)
      emit "_"
      emit (show outNum)

    emitDecl net
      | netName net == "display" = return ()
      | netName net `elem` ["reg", "regEn"] = do
          emit "reg ["
          emit (show (netWidth net-1))
          emit ":0] "
          emitWire (netInstId net, 0)
          emit " = "
          emit (lookupParam (netParams net) "init")
          emit ";\n"
      | otherwise = do
          emit "wire ["
          emit (show (netWidth net-1))
          emit ":0] "
          emitWire (netInstId net, 0)
          emit ";\n"

    emitInst net
      | netName net `elem` ["reg", "regEn"] = return ()
      | netName net == "const" = do
          emitWire (netInstId net, 0)
          emit " <= "
          emit (lookupParam (netParams net) "val")
          emit ";\n"
      | netName net == "~" = do
          emitWire (netInstId net, 0)
          emit " <= ~"
          emitWire (netInputs net !! 0)
          emit ";\n"
      | netName net == "negate" = do
          emitWire (netInstId net, 0)
          emit " <= -"
          emitWire (netInputs net !! 0)
          emit ";\n"
      | netName net == "replicate" = do
          emitWire (netInstId net, 0)
          emit " <= {"
          emit (show (netWidth net))
          emit "{"
          emitWire (netInputs net !! 0)
          emit "}};\n"
      | netName net == "?" = do
          emitWire (netInstId net, 0)
          emit " <= "
          emitWire (netInputs net !! 0)
          emit " ? "
          emitWire (netInputs net !! 1)
          emit " : "
          emitWire (netInputs net !! 2)
          emit ";\n"
      | netName net == "#" = do
          emitWire (netInstId net, 0)
          emit " <= {"
          emitWire (netInputs net !! 0)
          emit ","
          emitWire (netInputs net !! 1)
          emit "};\n"
      | netName net == "bit" = do
          emitWire (netInstId net, 0)
          emit " <= "
          emitWire (netInputs net !! 0)
          emit "["
          emit (lookupParam (netParams net) "index")
          emit "];\n"
      | netName net == "range" = do
          emitWire (netInstId net, 0)
          emit " <= "
          emitWire (netInputs net !! 0)
          emit "["
          emit (lookupParam (netParams net) "high")
          emit ":"
          emit (lookupParam (netParams net) "low")
          emit "];\n"
      | netName net == "zeroExtend" = do
          emitWire (netInstId net, 0)
          emit " <= "
          emit "{"
          emit (lookupParam (netParams net) "ext")
          emit "{1`b0},"
          emitWire (netInputs net !! 0)
          emit "};\n"
      | netName net == "signExtend" = do
          emitWire (netInstId net, 0)
          emit " <= "
          emit "{"
          emit (lookupParam (netParams net) "ext")
          emit "{"
          emitWire (netInputs net !! 0)
          emit "["
          emit (lookupParam (netParams net) "msb")
          emit "],"
          emitWire (netInputs net !! 0)
          emit "};\n"
      | otherwise = do
          -- Default case: binary operator
          emitWire (netInstId net, 0)
          emit " <= "
          emitWire (netInputs net !! 0)
          emit (netName net)
          emitWire (netInputs net !! 1)
          emit ";\n"

    emitAlways net
      | netName net == "reg" = do
          emitWire (netInstId net, 0)
          emit " <= "
          emitWire (netInputs net !! 0)
          emit ";\n"
      | netName net == "regEn" = do
          emit "if ("
          emitWire (netInputs net !! 0)
          emit " == 1) "
          emitWire (netInstId net, 0)
          emit " <= "
          emitWire (netInputs net !! 1)
          emit ";\n"
      | netName net == "display" = do
          emit "if ("
          emitWire (netInputs net !! 0)
          emit " == 1) $display("
          emitDisplay 0 (netParams net) (tail (netInputs net))
          emit ");\n"
      | otherwise = return ()

    emitDisplay n [] [] = return ()
    emitDisplay n ((k :-> v) : params) xs
      | show n == k = do
          emit ("\"" ++ v ++ "\"")
          if null params && null xs then return () else emit ","
          emitDisplay (n+1) params xs
    emitDisplay n params (x:xs) = do
      emitWire x
      if null params && null xs then return () else emit ","
      emitDisplay (n+1) params xs
