-- Emit netlist in C++ format

module Blarney.EmitCXX
  ( CXXGenParams(..)
  , emitCXXTop
  ) where

import Prelude
import Blarney.Unbit
import Blarney.DataFlow
import Blarney.Partition
import Blarney.IfThenElse
import System.IO
import System.Process
import Data.Bits
import Blarney.RTL
import qualified Data.IntSet as IS

-- Parameters for C++ generator
data CXXGenParams =
  CXXGenParams {
    cxxTargetDir       :: String -- Target directory
  , cxxModName         :: String -- Module name
  , cxxMaxLinesPerFile :: Int    -- Max lines of code per source file
  }

-- Write C++ files to directory
emitCXXTop :: RTL () -> String -> String -> IO ()
emitCXXTop top mod dir =
    do nl <- netlist top
       system ("mkdir -p " ++ dir)

       let netlist = sequentialise (dataFlow nl)

       -- Create output directory
       system ("mkdir -p " ++ cxxTargetDir params)

       -- Emit C++ files
       writeFiles params netlist

       -- Emit C++ file containing main function
       writeMain params

       -- Emit makefile
       writeMakefile params
  where
    fileName = dir ++ "/" ++ mod ++ ".v"
    params =
      CXXGenParams {
        cxxTargetDir       = dir
      , cxxModName         = mod
      , cxxMaxLinesPerFile = 8192
      }

-- List of lines of code
type Code = [String]

-- Emit wire
emitWire :: WireId -> String
emitWire (instId, outNum) = 
  "s.v" ++ show instId ++ "_" ++ show outNum

-- Emit wire name
emitWireName :: WireId -> String
emitWireName (instId, outNum) = 
  "v" ++ show instId ++ "_" ++ show outNum

-- Emit net input wire
emitInput :: WireId -> String
emitInput = emitWire

-- Is given width perfectly representable as a C++ type?
isStdWidth :: Width -> Bool
isStdWidth w = w `elem` [8, 16, 32, 64]

-- Determine C++ representation for given width
typeOf :: Width -> String
typeOf w
  | w <= 8 = "uint8_t"
  | w <= 16 = "uint16_t"
  | w <= 32 = "uint32_t"
  | w <= 64 = "uint64_t"
  | otherwise = "uint32_t*"

-- Number if 32-bit chunks needed to represent value of given width
numChunks :: Width -> Int
numChunks w = (w+31) `div` 32

-- C++ integer literal (for widths <= 64 bits)
lit :: Show a => a -> Int -> String
lit i w = show i ++ (if w > 32 then "ull" else "")

-- C++ integer all-set mask of given width (for widths <= 64 bits)
mask :: Width -> String
mask w = let i = (1 :: Integer) `shiftL` w in lit (i-1) w 

-- When width is larger than 64 bits, copy the pointer rather
-- than the array pointed at
useAlias (Mux _)      = True
useAlias (Identity _) = True
useAlias (Output _ _) = True
useAlias other        = False

-- Emit C++ variable declaration
emitDecl :: Prim -> WireId -> Width -> String
emitDecl prim wire w
  | w <= 64 = typeOf w ++ " " ++ emitWireName wire ++ ";"
  | useAlias prim = "uint32_t* " ++ emitWireName wire ++ ";"
  | otherwise = "uint32_t " ++ emitWireName wire ++
      "[" ++ show (numChunks w) ++ "];"

-- Emit C++ I/O declaration
emitIODecl :: Prim -> Code
emitIODecl (Input w str)
  | w <= 64 = [typeOf w ++ " " ++ str ++ ";"]
  | otherwise = ["uint32_t " ++ str ++
      "[" ++ show (numChunks w) ++ "];"]
emitIODecl (Output w str)
  | w <= 64 = [typeOf w ++ " " ++ str ++ ";"]
  | otherwise = ["uint32_t* " ++ str ++ ";"]
emitIODecl other = []

-- Emit RAM declaration
emitRAMDecl :: Net -> Code
emitRAMDecl net =
  case netPrim net of
    RAM init aw dw -> ram init aw dw ramName
    TrueDualRAM init aw dw -> ram init aw dw ramName
    RegFileMake init aw dw id -> ram init aw dw (regFileName id)
    other -> []
  where
    ramName = "array" ++ show (netInstId net)
    regFileName id = "regFile" ++ show id
    ram init aw dw name
        | dw <= 64 =
           [typeOf dw ++ " " ++ name ++
                         "[" ++ show (2^aw) ++ "];"]
        | otherwise =
           ["uint32_t " ++ name ++ "[" ++ show (2^aw) ++ "]" ++
                         "[" ++ show (numChunks dw) ++ "];"]

-- Emit C++ variable declarations for a given net
emitDecls :: Net -> Code
emitDecls net =
  emitRAMDecl net ++
  emitIODecl (netPrim net) ++
     [ emitDecl (netPrim net) (netInstId net, n) w
     | (n, w) <- zip [0..] (netOutputWidths net) ]

-- Emit C++ extern variable declarations for a given net
emitExterns :: Net -> Code
emitExterns = map ("extern " ++) . emitDecls

-- Emit C++ variable initialisation code
emitInit :: WireId -> Integer -> Width -> Code
emitInit wire i w
  | i == 0 = []
  | w <= 64 = [emitWire wire ++ " = " ++ lit i w ++ ";"]
  | otherwise = emitInitBU wire i 0 (numChunks w)
  where
    emitInitBU wire i c 0 = []
    emitInitBU wire i c n
      | lower == 0 = emitInitBU wire (i `shiftR` 32) (c+1) (n-1)
      | otherwise = 
          (emitWire wire ++ "[" ++ show c ++ "] = " ++ show lower ++ ";") :
            emitInitBU wire (i `shiftR` 32) (c+1) (n-1)
      where lower = i .&. 0xffffffff

-- Emit C++ RAM initialisation code
emitInitRAM :: String -> Maybe String -> Width -> Width -> Code
emitInitRAM id Nothing aw dw = []
emitInitRAM id (Just file) aw dw
  | dw <= 64 =
      [ "initRAM<" ++ typeOf dw ++ ">(" ++ show file ++ ", "
     ++ id ++ ", " ++ show (2^aw) ++ ");"]
  | otherwise =
      [ "initRAMBU<" ++ show (numChunks dw) ++ ">("
     ++ show file ++ ", " ++ id ++ ", " ++ show (2^aw) ++ ");"]

-- Emit C++ variable initialisation code for given net
emitInits :: Net -> Code
emitInits net =
  case netPrim net of
    Const w i               -> emitInit (netInstId net, 0) i w
    Register i w            -> emitInit (netInstId net, 0) i w
    RegisterEn i w          -> emitInit (netInstId net, 0) i w
    RAM init aw dw          -> emitInitRAM ramName init aw dw
    TrueDualRAM init aw dw  -> emitInitRAM ramName init aw dw
    RegFileMake "" aw dw id -> []
    RegFileMake i aw dw id  -> emitInitRAM (regFileName id) (Just i) aw dw
    other                   -> []
  where
    ramName = "s.array" ++ show (netInstId net)
    regFileName id = "s.regFile" ++ show id

-- Create header file containing externs
writeStateHeader :: String -> String -> [Net] -> IO ()
writeStateHeader modName fileName nets = do
  h <- openFile fileName WriteMode
  hPutStrLn h ("#ifndef _GLOBALS_" ++ modName ++ "_H_")
  hPutStrLn h ("#define _GLOBALS_" ++ modName ++ "_H_")
  hPutStrLn h "#include <stdint.h>"
  hPutStrLn h ("struct " ++ modName ++ " {")
  hPutStrLn h "bool finished;"
  mapM_ (mapM_ (hPutStrLn h) . emitDecls) nets
  hPutStrLn h "};"
  hPutStrLn h "#endif"
  hClose h

-- Split C++ function over multiple files
writeMulti :: CXXGenParams  -- Code gen parameters
           -> String        -- Name of function
           -> Code          -- Function body
           -> IO ()
writeMulti params name code = write code Nothing 0 0
  where
    dir = cxxTargetDir params
    mod = cxxModName params
    linesPerFile = cxxMaxLinesPerFile params
    fun n = n ++ "(" ++ mod ++ "& s)"

    write code Nothing i j = do
      -- Open a new file
      h <- openFile (dir ++ "/" ++ name ++ "_" ++ show i ++ ".cpp") WriteMode
      hPutStrLn h "#include <stdio.h>"
      hPutStrLn h "#include <BitVec.h>"
      hPutStrLn h ("#include \"" ++ mod ++ "_state.h\"")
      hPutStrLn h ("void " ++ fun (name ++ "_" ++ show i) ++ "{\n")
      write code (Just h) i 0
    write [] (Just h) i j = do
      -- Write top-level file
      hPutStrLn h "}"
      let prot = ["void " ++ fun (name ++ "_" ++ show n) ++ ";" | n <- [0..i]]
              ++ ["void " ++ fun name ++ ";"]
      let top  = ["void " ++ fun name ++ " {"]
              ++ [(name ++ "_" ++ show n) ++ "(s);" | n <- [0..i]]
              ++ ["}"]
      mapM_ (hPutStrLn h) prot
      mapM_ (hPutStrLn h) top
      hClose h
      h <- openFile (dir ++ "/" ++ name ++ ".h") WriteMode
      mapM_ (hPutStrLn h) prot
      hClose h
    write (line:code) (Just h) i j
      | j == linesPerFile = do
          hPutStrLn h "}"
          hClose h
          write (line:code) Nothing (i+1) 0
      | otherwise = do
          hPutStrLn h line
          write code (Just h) i (j+1)

-- Emit C++ files
writeFiles :: CXXGenParams -> [Net] -> IO ()
writeFiles params nets = do
  -- Header file containing state variables
  writeStateHeader
    (cxxModName params)
    (cxxTargetDir params ++ "/" ++ cxxModName params ++ "_state.h")
    nets
  -- Write initialisation functions
  writeMulti params (cxxModName params ++ "_init") $
    concatMap emitInits nets
  -- Write step functions
  writeMulti params (cxxModName params ++ "_step") $
    concatMap emitInst nets
  -- Write update functions
  writeMulti params (cxxModName params ++ "_update") $
    concatMap emitUpdates nets

-- Generic infix operator instance
emitInfixOpInst :: String -> String -> Net -> Width -> Bool -> String
emitInfixOpInst op bigOp net w applyMask
  | w <= 64 =
         emitWire (netInstId net, 0)
      ++ " = ("
      ++ emitInput (netInputs net !! 0)
      ++ " " ++ op ++ " "
      ++ emitInput (netInputs net !! 1)
      ++ ")"
      ++ (if not (isStdWidth w) && applyMask
            then " & " ++ mask w else "")
      ++ ";"
  | otherwise =
         bigOp
      ++ "("
      ++ emitInput (netInputs net !! 0)
      ++ ","
      ++ emitInput (netInputs net !! 1)
      ++ ","
      ++ emitWire (netInstId net, 0)
      ++ ","
      ++ show w ++ ");"

-- Generic prefix operator instance
emitPrefixOpInst :: String -> String -> Net -> Width -> Bool -> String
emitPrefixOpInst op bigOp net w applyMask
  | w <= 64 =
         emitWire (netInstId net, 0)
      ++ " = "
      ++ op
      ++ "("
      ++ emitInput (netInputs net !! 0)
      ++ ")"
      ++ (if not (isStdWidth w) && applyMask
            then " & " ++ mask w else "")
      ++ ";"
  | otherwise = do
         bigOp
      ++ "("
      ++ emitInput (netInputs net !! 0)
      ++ ","
      ++ emitWire (netInstId net, 0)
      ++ ","
      ++ show w ++ ");"

-- Generic comparator instance
emitCmpInst :: String -> String -> Net -> Width -> String
emitCmpInst op bigOp net w
  | w <= 64 =
         emitWire (netInstId net, 0)
      ++ " = "
      ++ emitInput (netInputs net !! 0)
      ++ op
      ++ emitInput (netInputs net !! 1)
      ++ ";"
  | otherwise =
         emitWire (netInstId net, 0)
      ++ " = "
      ++ bigOp
      ++ "("
      ++ emitInput (netInputs net !! 0)
      ++ ","
      ++ emitInput (netInputs net !! 1)
      ++ ","
      ++ show w ++ ");"

-- Bit replicator instance
emitReplicateInst :: Width -> Net -> String
emitReplicateInst w net
  | w <= 64 =
         emitWire (netInstId net, 0)
      ++ " = "
      ++ emitInput (netInputs net !! 0)
      ++ " ? "
      ++ mask w
      ++ " : "
      ++ lit 0 w
      ++ ";\n"
  | otherwise =
         "replicateBU("
      ++ emitInput (netInputs net !! 0)
      ++ ","
      ++ emitWire (netInstId net, 0)
      ++ ","
      ++ show w ++ ");"

-- Zero-extension instance
emitZeroExtendInst :: Net -> Width -> Width -> String
emitZeroExtendInst net inWidth outWidth
  | outWidth <= 64 =
         emitWire (netInstId net, 0)
      ++ " = "
      ++ emitInput (netInputs net !! 0)
      ++ ";\n"
  | inWidth <= 64 =
         "toBU("
      ++ emitInput (netInputs net !! 0)
      ++ ","
      ++ emitWire (netInstId net, 0)
      ++ ","
      ++ show outWidth ++ ");"
  | otherwise =
          "zeroExtBU("
      ++ emitInput (netInputs net !! 0)
      ++ ","
      ++ emitWire (netInstId net, 0)
      ++ ","
      ++ show inWidth
      ++ ","
      ++ show outWidth ++ ");"

-- Sign-extension instance
emitSignExtendInst :: Net -> Width -> Width -> String
emitSignExtendInst net inWidth outWidth
  | outWidth <= 64 =
      let ones = ((1 :: Integer) `shiftL` (outWidth - inWidth)) - 1
          ext  = lit (ones `shiftL` inWidth) outWidth
          msb  = lit ((1 :: Integer) `shiftL` (inWidth-1)) inWidth
       in emitWire (netInstId net, 0)
       ++ " = "
       ++ emitInput (netInputs net !! 0)
       ++ " | (("
       ++ emitInput (netInputs net !! 0)
       ++ " & " ++ msb ++ ") ? "
       ++ ext ++ " : " ++ lit 0 outWidth
       ++ ");"
  | inWidth <= 64 =
         "toBU("
      ++ emitInput (netInputs net !! 0)
      ++ ","
      ++ emitWire (netInstId net, 0)
      ++ ","
      ++ show outWidth ++ ");\n"
      ++ "signExtBU("
      ++ emitWire (netInstId net, 0)
      ++ ","
      ++ emitWire (netInstId net, 0)
      ++ ","
      ++ show inWidth
      ++ ","
      ++ show outWidth ++ ");"
  | otherwise =
         "signExtBU("
      ++ emitInput (netInputs net !! 0)
      ++ ","
      ++ emitWire (netInstId net, 0)
      ++ ","
      ++ show inWidth
      ++ ","
      ++ show outWidth ++ ");"

-- Bit-selection instance
emitSelectBitsInst :: Net -> Width -> Width -> Width -> String
emitSelectBitsInst net w hi lo
  | w <= 64 =
         emitWire (netInstId net, 0)
      ++ " = ("
      ++ emitInput (netInputs net !! 0)
      ++ " >> " ++ show lo
      ++ ") & "
      ++ mask (1+hi-lo)
      ++ ";"
  | hi == lo =
         emitWire (netInstId net, 0)
      ++ " = getBitBU("
      ++ emitInput (netInputs net !! 0)
      ++ ","
      ++ show lo ++ ");"
  | (hi-lo) < 64 =
        "{ uint64_t _tmp = fromShiftedBU("
      ++ emitInput (netInputs net !! 0)
      ++ ","
      ++ show lo
      ++ ","
      ++ show w ++ "); "
      ++ emitWire (netInstId net, 0)
      ++ " = _tmp & "
      ++ mask (1+hi-lo)
      ++ "; }"
  | otherwise = do
         "getBitsBU("
      ++ emitInput (netInputs net !! 0)
      ++ ","
      ++ emitWire (netInstId net, 0)
      ++ ","
      ++ show hi
      ++ ","
      ++ show lo ++ ");"


emitConcatInst :: Net -> Width -> Width -> String
emitConcatInst net aw bw
  | (aw+bw) <= 64 =
         emitWire (netInstId net, 0)
      ++ " = ((uint64_t) "
      ++ emitInput (netInputs net !! 0)
      ++ " << " ++ show bw
      ++ ") | "
      ++ emitInput (netInputs net !! 1)
      ++ ";"
  | aw > 64 && bw > 64 =
         "concatBU("
      ++ emitInput (netInputs net !! 0)
      ++ ", "
      ++ emitInput (netInputs net !! 1)
      ++ ", "
      ++ emitWire (netInstId net, 0)
      ++ ", "
      ++ show aw
      ++ ", "
      ++ show bw
      ++ ");"
  | aw <= 64 && bw <= 64 =
      "{ "
      ++ lift (netInputs net !! 0) "_tmp0" aw
      ++ lift (netInputs net !! 1) "_tmp1" bw
      ++ "concatBU(_tmp0, _tmp1, "
      ++ emitWire (netInstId net, 0)
      ++ ", "
      ++ show aw
      ++ ", "
      ++ show bw
      ++ "); }"
  | aw <= 64 =
         "{ "
      ++ lift (netInputs net !! 0) "_tmp0" aw
      ++ "concatBU(_tmp0, "
      ++ emitInput (netInputs net !! 1)
      ++ ", "
      ++ emitWire (netInstId net, 0)
      ++ ", "
      ++ show aw
      ++ ", "
      ++ show bw
      ++ "); }"
  | otherwise = do
         "{ "
      ++ lift (netInputs net !! 1) "_tmp1" aw
      ++ "concatBU("
      ++ emitInput (netInputs net !! 1)
      ++ ", _tmp1, "
      ++ emitWire (netInstId net, 0)
      ++ ", "
      ++ show aw
      ++ ", "
      ++ show bw
      ++ "); }"
  where
    lift inp newInp w =
         "uint32_t "
      ++ newInp
      ++ "[2]; toBU("
      ++ emitInput inp
      ++ ", " ++ newInp ++ ", "
      ++ show w
      ++ "); "

emitCountOnes :: Net -> Width -> String
emitCountOnes net w
  | w <= 64 = 
         emitWire (netInstId net, 0)
      ++ " = countOnes("
      ++ emitInput (netInputs net !! 0)
      ++ ");\n"
  | otherwise =
         emitWire (netInstId net, 0)
      ++ " = countOnesBU("
      ++ emitInput (netInputs net !! 0)
      ++ ", "
      ++ show w
      ++ ");"

emitMuxInst :: Net -> String
emitMuxInst net =
     emitWire (netInstId net, 0)
  ++ " = "
  ++ emitInput (netInputs net !! 0)
  ++ " ? "
  ++ emitInput (netInputs net !! 1)
  ++ " : "
  ++ emitInput (netInputs net !! 2)
  ++ ";"

emitIdentityInst :: Net -> String
emitIdentityInst net =
     emitWire (netInstId net, 0)
  ++ " = "
  ++ emitInput (netInputs net !! 0)
  ++ ";"

emitFinish :: Net -> String
emitFinish net =
    "if ("
  ++ emitInput (netInputs net !! 0)
  ++ ") s.finished = true;"

emitInputPrim :: String -> Net -> Width -> String
emitInputPrim str net w
  | w <= 64 =
         emitWire (netInstId net, 0)
      ++ " = "
      ++ str
      ++ ";"
  | otherwise =
         "copyBU("
      ++ str
      ++ ", "
      ++ emitWire (netInstId net, 0)
      ++ ", "
      ++ show w
      ++ ");"

emitOutputPrim :: String -> Net -> String
emitOutputPrim str net =
     str 
  ++ " = "
  ++ emitInput (netInputs net !! 0)
  ++ ";"

emitCopy :: Net -> WireId -> Width -> String
emitCopy net inp w 
  | w <= 64 = 
         emitWire (netInstId net, 0)
      ++ " = "
      ++ emitInput inp
      ++ ";"
  | otherwise =
         "copyBU("
      ++ emitInput inp
      ++ ", "
      ++ emitWire (netInstId net, 0)
      ++ ", "
      ++ show w
      ++ ");"

emitCopyEn :: Net -> WireId -> WireId -> Width -> String
emitCopyEn net en inp w =
     "if ("
  ++ emitInput en
  ++ ") { "
  ++ emitCopy net inp w
  ++ " }"

emitRAMUpdate :: Net -> Width -> Width -> Int -> String
emitRAMUpdate net aw dw port
  | aw > 64 = error "C++ generator: RAM address width must be <= 64"
  | dw <= 64 =
          -- Update array
          "if (" ++ emitInput (netInputs net !! (base+2)) ++ ") { "
       ++ "s.array" ++ show (netInstId net)
       ++ "[" ++ emitInput (netInputs net !! base) ++ "]"
       ++ " = " ++ emitInput (netInputs net !! (base+1)) ++ "; "
          -- Update RAM output
       ++ emitWire (netInstId net, port) ++ " = "
       ++ emitInput (netInputs net !! (base+1))
       ++ "; }\n"
  | otherwise =
          -- Update array
          "if (" ++ emitInput (netInputs net !! (base+2)) ++ ") { "
       ++ "copyBU(" ++ emitInput (netInputs net !! (base+1)) ++ ", s.array"
       ++ show (netInstId net) ++ "["
       ++ emitInput (netInputs net !! base) ++ "]," ++ show dw ++ "); "
          -- Update RAM output
       ++ "copyBU(" ++ emitInput (netInputs net !! (base+1)) ++ ", "
       ++ emitWire (netInstId net, port) ++ ","
       ++ show dw ++ "); }\n"
  where base = 3*port

emitRAMLookup :: Net -> Width -> Width -> Int -> String
emitRAMLookup net aw dw port
  | dw <= 64 =
           emitWire (netInstId net, port) ++ " = "
       ++ "s.array" ++ show (netInstId net)
       ++ "[" ++ emitInput (netInputs net !! base) ++ "];"
  | otherwise =
          "copyBU(s.array" ++ show (netInstId net) ++ "["
       ++ emitInput (netInputs net !! base) ++ "], "
       ++ emitWire (netInstId net, port) ++ ","
       ++ show dw ++ ");"
  where base = 3*port

emitRegFileWrite :: Net -> Width -> Width -> Int -> String
emitRegFileWrite net aw dw id
  | aw > 64 = error "C++ generator: RegFile address width must be <= 64"
  | dw <= 64 =
          -- Update array
          "if (" ++ emitInput (netInputs net !! 0) ++ ") { "
       ++ "s.regFile" ++ show id
       ++ "[" ++ emitInput (netInputs net !! 1) ++ "]"
       ++ " = " ++ emitInput (netInputs net !! 2) ++ "; "
       ++ "}\n"
  | otherwise =
          -- Update array
          "if (" ++ emitInput (netInputs net !! 0) ++ ") { "
       ++ "copyBU(" ++ emitInput (netInputs net !! 1) ++ ", s.regFile"
       ++ show id ++ "["
       ++ emitInput (netInputs net !! 2) ++ "]," ++ show dw ++ "); "
       ++ "); }\n"

emitRegFileRead :: Net -> Width -> Int -> String
emitRegFileRead net dw id
  | dw <= 64 =
           emitWire (netInstId net, 0) ++ " = "
       ++ "s.regFile" ++ show id
       ++ "[" ++ emitInput (netInputs net !! 0) ++ "];"
  | otherwise =
          "copyBU(s.regFile" ++ show id ++ "["
       ++ emitInput (netInputs net !! 0) ++ "], "
       ++ emitWire (netInstId net, 0) ++ ","
       ++ show dw ++ ");"

emitDisplay :: Net -> [DisplayArg] -> String
emitDisplay net args =
     "if ("
  ++ emitInput (netInputs net !! 0)
  ++ ") { "
  ++ emitDisp args (tail (netInputs net))
  ++ "}"
  where
    emitDisp [] [] = "printf(\"\\n\");"
    emitDisp (DisplayArgString s : args) inps =
         "printf(\"%s\", " ++ show s ++ ");"
      ++ emitDisp args inps
    emitDisp (DisplayArgBit w : args) (inp:inps)
      | w <= 32   = "printf(\"0x%x\", " ++ emitInput inp ++ "); "
                 ++ emitDisp args inps
      | w <= 64   = "printf(\"0x%lx\", " ++ emitInput inp ++ "); "
                 ++ emitDisp args inps
      | otherwise = "printBU(" ++ emitInput inp ++ ", "
                 ++ show w ++ "); "
                 ++ emitDisp args inps

emitUpdates :: Net -> Code
emitUpdates net =
  case netPrim net of
    Register i w           -> [emitCopy net (netInputs net !! 0) w]
    RegisterEn i w         -> [emitCopyEn net (netInputs net !! 0)
                                              (netInputs net !! 1) w]
    RAM init aw dw         -> [emitRAMLookup net aw dw 0,
                               emitRAMUpdate net aw dw 0]
    TrueDualRAM init aw dw -> [emitRAMLookup net aw dw 0,
                               emitRAMLookup net aw dw 1,
                               emitRAMUpdate net aw dw 0,
                               emitRAMUpdate net aw dw 1]
    RegFileWrite aw dw id  -> [emitRegFileWrite net aw dw id]
    other                  -> []

emitInst :: Net -> Code
emitInst net =
  case netPrim net of
    Const w i              -> []
    Add w                  -> [emitInfixOpInst "+" "addBU" net w True]
    Sub w                  -> [emitInfixOpInst "-" "subBU" net w True]
    Mul w                  -> [emitInfixOpInst "*" "mulBU" net w True]
    Div w                  -> [emitInfixOpInst "/" "divBU" net w False]
    Mod w                  -> [emitInfixOpInst "%" "modBU" net w False]
    Not w                  -> [emitPrefixOpInst "~" "notBU" net w True]
    And w                  -> [emitInfixOpInst "&" "andBU" net w False]
    Or  w                  -> [emitInfixOpInst "|" "orBU" net w False]
    Xor w                  -> [emitInfixOpInst "^" "xorBU" net w False]
    ShiftLeft w            -> [emitInfixOpInst "<<" "leftBU" net w True]
    ShiftRight w           -> [emitInfixOpInst ">>" "rightBU" net w False]
    Equal w                -> [emitCmpInst "==" "eqBU" net w]
    NotEqual w             -> [emitCmpInst "!=" "neqBU" net w]
    LessThan w             -> [emitCmpInst "<" "ltBU" net w]
    LessThanEq w           -> [emitCmpInst "<=" "leBU" net w]
    Register i w           -> []
    RegisterEn i w         -> []
    RAM init aw dw         -> []
    TrueDualRAM init aw dw -> []
    ReplicateBit w         -> [emitReplicateInst w net]
    ZeroExtend wi wo       -> [emitZeroExtendInst net wi wo]
    SignExtend wi wo       -> [emitSignExtendInst net wi wo]
    SelectBits w hi lo     -> [emitSelectBitsInst net w hi lo]
    Concat aw bw           -> [emitConcatInst net aw bw]
    Mux w                  -> [emitMuxInst net]
    CountOnes w            -> [emitCountOnes net (2^(w-1))]
    Identity w             -> [emitIdentityInst net]
    Display args           -> [emitDisplay net args]
    Finish                 -> [emitFinish net]
    Input w str            -> [emitInputPrim str net w]
    Output w str           -> [emitOutputPrim str net]
    Custom p is os ps      -> []
    RegFileRead w id       -> [emitRegFileRead net w id]
    RegFileMake i aw dw id -> []
    RegFileWrite aw dw id  -> []

writeMain :: CXXGenParams -> IO ()
writeMain params = do
      let filename = cxxTargetDir params ++ "/main.cpp"
      let mod = cxxModName params
      h <- openFile filename WriteMode
      mapM (hPutStrLn h) $
        [ "#include <stdio.h>"
        , "#include <string.h>"
        , "#include <BitVec.h>"
        , "#include \"" ++ mod ++ "_state.h\""
        , "#include \"" ++ mod ++ "_init.h\""
        , "#include \"" ++ mod ++ "_step.h\""
        , "#include \"" ++ mod ++ "_update.h\""
        , "int main () {"
        , "  " ++ mod ++ " s;"
        , "  memset(&s, 0, sizeof(" ++ mod ++ "));"
        , "  " ++ mod ++ "_init(s);"
        , "  while (! s.finished) { " ++ mod ++ "_step(s); " 
                                      ++ mod ++ "_update(s); }"
        , "  return 0;"
        , "}"
        ]
      hClose h

writeMakefile :: CXXGenParams -> IO ()
writeMakefile params = do
  let filename = cxxTargetDir params ++ "/Makefile"
  h <- openFile filename WriteMode
  mapM (hPutStrLn h)
    [ "CC=clang++"
    , "ifndef BLARNEY_ROOT"
    , "$(error Please set BLARNEY_ROOT)"
    , "endif"
    , "main: $(patsubst %.cpp,%.o,$(wildcard *.cpp)) BitVec.o"
    , "\t$(CC) *.o -o main"
    , "BitVec.o:"
    , "\t$(CC) -c -O2 -I $(BLARNEY_ROOT)/C " ++
      "$(BLARNEY_ROOT)/C/BitVec.cpp -o BitVec.o"
    , "%.o: %.cpp"
    , "\t$(CC) -O -I $(BLARNEY_ROOT)/C -c $< -o $@"
    , ".PHONY: clean"
    , "clean:"
    , "\trm -f *.o main"
    ]
  hClose h
