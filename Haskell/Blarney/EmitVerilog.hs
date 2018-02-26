-- Emit netlist in Verilog format

module Blarney.EmitVerilog
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
    emit "module top (\n"
    emit "  input wire clock"
    mapM_ emitInputOutput netlist
    emit "\n);\n"
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

    emitInput = emitWire

    emitDeclHelper width wire = do
      emit "["
      emit (show (width-1))
      emit ":0] "
      emitWire wire
      emit ";\n"

    emitDeclInitHelper width wire init = do
      emit "["
      emit (show (width-1))
      emit ":0] "
      emitWire wire
      emit " = "
      emit (show width)
      emit "'d"
      emit (show init)
      emit ";\n"

    emitWireDecl width wire = do
      emit "wire "
      emitDeclHelper width wire

    emitWireInitDecl width wire init = do
      emit "wire "
      emitDeclInitHelper width wire init

    emitRegInitDecl width wire init = do
      emit "reg "
      emitDeclInitHelper width wire init

    emitDecl net =
      let wire = (netInstId net, 0) in
        case netPrim net of
          Const w i          -> emitWireInitDecl w wire i
          Add w              -> emitWireDecl w wire
          Sub w              -> emitWireDecl w wire
          Mul w              -> emitWireDecl w wire
          Div w              -> emitWireDecl w wire
          Mod w              -> emitWireDecl w wire
          Not w              -> emitWireDecl w wire
          And w              -> emitWireDecl w wire
          Or  w              -> emitWireDecl w wire
          Xor w              -> emitWireDecl w wire
          ShiftLeft w        -> emitWireDecl w wire
          ShiftRight w       -> emitWireDecl w wire
          Equal w            -> emitWireDecl 1 wire
          NotEqual w         -> emitWireDecl 1 wire
          LessThan w         -> emitWireDecl 1 wire
          LessThanEq w       -> emitWireDecl 1 wire
          Register i w       -> emitRegInitDecl w wire i
          RegisterEn i w     -> emitRegInitDecl w wire i
          ReplicateBit w     -> emitWireDecl w wire
          ZeroExtend wi wo   -> emitWireDecl wo wire
          SignExtend wi wo   -> emitWireDecl wo wire
          SelectBits w hi lo -> emitWireDecl (hi-lo) wire
          Concat aw bw       -> emitWireDecl (aw+bw) wire
          Mux w              -> emitWireDecl w wire
          CountOnes w        -> emitWireDecl w wire
          Identity w         -> emitWireDecl w wire
          Display args       -> return ()
          Finish             -> return ()
          Input w s          -> return ()
          Output w s         -> return ()
          Custom p is os ps  -> 
            sequence_ [ emitWireDecl w (netInstId net, n)
                      | ((o, w), n) <- zip os [0..] ]

    emitInputOutput net =
      case netPrim net of
        Input w s ->
          emit (", input wire [" ++ show (w-1) ++ ":0] " ++ s ++ "\n")
        Output w s -> 
          emit (", output wire [" ++ show (w-1) ++ ":0] " ++ s ++ "\n")
        other -> return ()

    emitAssignConst w i net = do
      emit "assign "
      emitWire (netInstId net, 0)
      emit " = "
      emit (show w)
      emit "'d" >>  emit (show i) >> emit ";\n"

    emitPrefixOpInst op net = do
      emit "assign "
      emitWire (netInstId net, 0)
      emit " = " >> emit op >> emit "("
      emitInput (netInputs net !! 0)
      emit ");\n"

    emitInfixOpInst op net = do
      emit "assign "
      emitWire (netInstId net, 0)
      emit " = "
      emitInput (netInputs net !! 0)
      emit " " >> emit op >> emit " "
      emitInput (netInputs net !! 1)
      emit ";\n"

    emitReplicateInst w net = do
      emit "assign "
      emitWire (netInstId net, 0)
      emit " = {"
      emit (show w)
      emit "{"
      emitInput (netInputs net !! 0)
      emit "}};\n"

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

    emitCustomInst net name ins outs params = do
      emit name >> emit " "
      let numParams = length params
      if numParams == 0
        then return ()
        else do
          emit "#("
          sequence_
               [ do emit "." >> emit key
                    emit "(" >> emit val >> emit ")"
                    if i < numParams then emit ",\n" else return ()
               | (key :-> val, i) <- zip params [1..] ]
      emit ("i" ++ show (netInstId net))
      let args = zip ins (netInputs net) ++
                   [ (o, (netInstId net, n))
                   | (o, n) <- zip (map fst outs) [0..] ]
      let numArgs = length args
      emit "("
      if numArgs == 0
        then return ()
        else do
          sequence_
             [ do emit "." >> emit name
                  emit "(" >> emitWire wire >> emit ")"
                  if i < numArgs then emit ",\n" else return ()
             | ((name, wire), i) <- zip args [1..] ]
      emit ");\n"

    emitOutputInst net s = do
      emit ("assign " ++ s ++ " = ")
      emitInput (netInputs net !! 0)
      emit ";\n"

    emitInputInst net s = do
      emit "assign "
      emitWire (netInstId net, 0)
      emit " = "
      emit s
      emit ";\n"

    emitInst net =
      case netPrim net of
        Const w i          -> return ()
        Add w              -> emitInfixOpInst "+" net
        Sub w              -> emitInfixOpInst "-" net
        Mul w              -> emitInfixOpInst "*" net
        Div w              -> emitInfixOpInst "/" net
        Mod w              -> emitInfixOpInst "%" net
        Not w              -> emitPrefixOpInst "~" net
        And w              -> emitInfixOpInst "&" net
        Or  w              -> emitInfixOpInst "|" net
        Xor w              -> emitInfixOpInst "^" net
        ShiftLeft w        -> emitInfixOpInst "<<" net
        ShiftRight w       -> emitInfixOpInst ">>" net
        Equal w            -> emitInfixOpInst "==" net
        NotEqual w         -> emitInfixOpInst "!=" net
        LessThan w         -> emitInfixOpInst "<" net
        LessThanEq w       -> emitInfixOpInst "<=" net
        Register i w       -> return ()
        RegisterEn i w     -> return ()
        ReplicateBit w     -> emitReplicateInst w net
        ZeroExtend wi wo   -> emitZeroExtendInst net wi wo
        SignExtend wi wo   -> emitSignExtendInst net wi wo
        SelectBits w hi lo -> emitSelectBitsInst net hi lo
        Concat aw bw       -> emitConcatInst net
        Mux w              -> emitMuxInst net
        CountOnes w        -> emitPrefixOpInst "$countones" net
        Identity w         -> emitPrefixOpInst "" net
        Display args       -> return ()
        Finish             -> return ()
        Input w s          -> emitInputInst net s
        Output w s         -> emitOutputInst net s
        Custom p is os ps  -> emitCustomInst net p is os ps
 
    emitAlways net =
      case netPrim net of
        Register init w -> do
          emitWire (netInstId net, 0)
          emit " <= "
          emitInput (netInputs net !! 0)
          emit ";\n"
        RegisterEn init w -> do
          emit "if ("
          emitInput (netInputs net !! 0)
          emit " == 1) "
          emitWire (netInstId net, 0)
          emit " <= "
          emitInput (netInputs net !! 1)
          emit ";\n"
        Display args -> do
          emit "if ("
          emitInput (netInputs net !! 0)
          emit " == 1) $display(\""
          emitDisplayFormat args
          emit ","
          emitDisplayArgs args (tail (netInputs net))
          emit ");\n"
        Finish -> do
          emit "if ("
          emitInput (netInputs net !! 0)
          emit " == 1) $finish;\n"
        other -> return ()

    emitDisplayFormat [] = emit "\\n\""
    emitDisplayFormat (DisplayArgString s : args) = do
      emit "%s"
      emitDisplayFormat args
    emitDisplayFormat (DisplayArgBit w : args) = do
      emit "%d"
      emitDisplayFormat args

    emitDisplayArgs [] _ = return ()
    emitDisplayArgs (DisplayArgString s : args) wires = do
      emit ("\"" ++ s ++ "\"")
      if null args then return () else emit ","
      emitDisplayArgs args wires
    emitDisplayArgs (DisplayArgBit w : args) (wire:wires) = do
      emitInput wire
      if null args then return () else emit ","
      emitDisplayArgs args wires
