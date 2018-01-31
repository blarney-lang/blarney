-- Emit netlist in Verilog format

module Blarney.Verilog
  ( printVerilog
  , writeVerilog
  ) where

import Blarney.Unbit
import System.IO

printVerilog :: [Net] -> IO ()
printVerilog = hWriteVerilog stdout

writeVerilog :: String -> [Net] -> IO ()
writeVerilog filename netlist = do
  h <- openFile filename WriteMode
  hWriteVerilog h netlist
  hClose h

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
      | netName net == "finish" = return ()
      | netName net `elem` ["<", "<=", ">", ">=", "==", "!="] = do
          emit "wire [0:0] "
          emitWire (netInstId net, 0)
          emit ";\n"
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
      | netName net == "display" = return ()
      | netName net == "finish" = return ()
      | netName net `elem` ["reg", "regEn"] = return ()
      | netName net == "const" = do
          emit "assign "
          emitWire (netInstId net, 0)
          emit " = "
          emit (lookupParam (netParams net) "val")
          emit ";\n"
      | netName net == "~" = do
          emit "assign "
          emitWire (netInstId net, 0)
          emit " = ~"
          emitWire (netInputs net !! 0)
          emit ";\n"
      | netName net == "countOnes" = do
          emit "assign "
          emitWire (netInstId net, 0)
          emit " = $countones("
          emitWire (netInputs net !! 0)
          emit ");\n"
      | netName net == "negate" = do
          emit "assign "
          emitWire (netInstId net, 0)
          emit " = -"
          emitWire (netInputs net !! 0)
          emit ";\n"
      | netName net == "replicate" = do
          emit "assign "
          emitWire (netInstId net, 0)
          emit " = {"
          emit (show (netWidth net))
          emit "{"
          emitWire (netInputs net !! 0)
          emit "}};\n"
      | netName net == "?" = do
          emit "assign "
          emitWire (netInstId net, 0)
          emit " = "
          emitWire (netInputs net !! 0)
          emit " ? "
          emitWire (netInputs net !! 1)
          emit " : "
          emitWire (netInputs net !! 2)
          emit ";\n"
      | netName net == "#" = do
          emit "assign "
          emitWire (netInstId net, 0)
          emit " = {"
          emitWire (netInputs net !! 0)
          emit ","
          emitWire (netInputs net !! 1)
          emit "};\n"
      | netName net == "bit" = do
          emit "assign "
          emitWire (netInstId net, 0)
          emit " = "
          emitWire (netInputs net !! 0)
          emit "["
          emit (lookupParam (netParams net) "index")
          emit "];\n"
      | netName net == "range" = do
          emit "assign "
          emitWire (netInstId net, 0)
          emit " = "
          emitWire (netInputs net !! 0)
          emit "["
          emit (lookupParam (netParams net) "hi")
          emit ":"
          emit (lookupParam (netParams net) "lo")
          emit "];\n"
      | netName net == "zeroExtend" = do
          emit "assign "
          emitWire (netInstId net, 0)
          emit " = "
          emit "{"
          emit (lookupParam (netParams net) "ext")
          emit "{1`b0},"
          emitWire (netInputs net !! 0)
          emit "};\n"
      | netName net == "signExtend" = do
          emit "assign "
          emitWire (netInstId net, 0)
          emit " = "
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
          emit "assign "
          emitWire (netInstId net, 0)
          emit " = "
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
          emit " == 1) $display(\""
          emitDisplayFormat 0 (netParams net) (tail (netInputs net))
          emit ","
          emitDisplayArgs 0 (netParams net) (tail (netInputs net))
          emit ");\n"
      | netName net == "finish" = do
          emit "if ("
          emitWire (netInputs net !! 0)
          emit " == 1) $finish;\n"
      | otherwise = return ()

    emitDisplayFormat n [] [] = emit "\\n\""
    emitDisplayFormat n ((k :-> v) : params) xs
      | show n == k = do
          emit "%s"
          emitDisplayFormat (n+1) params xs
    emitDisplayFormat n params (x:xs) = do
      emit "%d"
      emitDisplayFormat (n+1) params xs

    emitDisplayArgs n [] [] = return ()
    emitDisplayArgs n ((k :-> v) : params) xs
      | show n == k = do
          emit ("\"" ++ v ++ "\"")
          if null params && null xs then return () else emit ","
          emitDisplayArgs (n+1) params xs
    emitDisplayArgs n params (x:xs) = do
      emitWire x
      if null params && null xs then return () else emit ","
      emitDisplayArgs (n+1) params xs
