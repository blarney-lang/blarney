-- Emit netlist in C format

module Blarney.EmitC
  ( printC
  , writeC
  , sequentialise
  ) where

import Blarney.Unbit
import System.IO
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
            Just w  -> let net = Net { netPrim   = Identity w
                                     , netInstId = id
                                     , netInputs = [i] }
                       in  (id+1, [net], (id, 0))

hWriteC :: Handle -> [Net] -> IO ()
hWriteC h netlist = do
    mapM_ (\net -> emit (show net ++ "\n")) netlist
  where
    emit = hPutStr h
