module Blarney.EmitNetlist
  ( printNetlist
  , writeNetlist
  ) where

import Blarney.Unbit
import System.IO

-- Print netlist to stdout
printNetlist :: [Net] -> IO ()
printNetlist = hWriteNetlist stdout

-- Dump netlist to a file
writeNetlist :: String -> [Net] -> IO ()
writeNetlist filename netlist = do
  h <- openFile filename WriteMode
  hWriteNetlist h netlist
  hClose h

-- Write netlist to a given file handle
hWriteNetlist :: Handle -> [Net] -> IO ()
hWriteNetlist h netlist = do
    mapM_ emitNet netlist
  where
    emit = hPutStr h
    emitNet net = emit $
      show (netInstId net) ++ " " ++
      "(" ++ show (netPrim net) ++ ") " ++
      show (netInputs net) ++ "\n"
