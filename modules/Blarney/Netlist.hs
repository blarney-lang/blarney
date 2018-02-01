module Blarney.Netlist
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

    emitNet net = do
      emit (show (show (netInstId net)))
      emit " "
      emit (netName net)
      emit " ["
      emitList [(show id, pin) | (id, pin) <- netInputs net]
      emit "] ["
      emitList (netParams net)
      emit "] "
      emit (show (netWidth net))
      emit "\n"

    emitList [] = return ()
    emitList (x:xs) = do
      emit (show x)
      if null xs then return () else emit ", "
      emitList xs
