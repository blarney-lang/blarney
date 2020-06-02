{-|
Module      : Blarney.Netlist.Passes.ZeroWidthNetIgnore
Description : A blarney netlist pass to ignore zero-width nets
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental
-}

module Blarney.Netlist.Passes.ZeroWidthNetIgnore (
  zeroWidthNetIgnore
) where

import Prelude
import Data.IORef
import Control.Monad
import Data.Array.MArray

import Blarney.Netlist.Passes.Utils

-- | Transform a 'Net' with non-zero-width output and at least one zero-width
--   input into an equivalent 'Net' with no reference to any other zero-width
--   output 'Net' as its input
zeroWidthNetTransform :: Net -> (Net, Bool)
-- straight transformation cases
zeroWidthNetTransform net@Net{ netPrim = Equal 0 } =
  (net { netPrim = Const 1 1, netInputs = [] }, True)
zeroWidthNetTransform net@Net{ netPrim = NotEqual 0 } =
  (net { netPrim = Const 1 0, netInputs = [] }, True)
zeroWidthNetTransform net@Net{ netPrim = LessThan 0 } =
  (net { netPrim = Const 1 0, netInputs = [] }, True)
zeroWidthNetTransform net@Net{ netPrim = LessThanEq 0 } =
  (net { netPrim = Const 1 1, netInputs = [] }, True)
zeroWidthNetTransform net@Net{ netPrim = ZeroExtend 0 w } =
  (net { netPrim = Const w 0, netInputs = [] }, True)
zeroWidthNetTransform net@Net{ netPrim = SignExtend 0 w } =
  (net { netPrim = Const w 0, netInputs = [] }, True)
zeroWidthNetTransform net@Net{ netPrim = SelectBits 0 hi lo } =
  (net { netPrim = Const (hi-lo) 0, netInputs = [] }, True)
zeroWidthNetTransform net@Net{ netPrim = Concat w0 w1, netInputs = [i0, i1] }
  | w0 == 0 && w1 /= 0 = (net { netPrim = Identity w1, netInputs = [i1] }, True)
  | w0 /= 0 && w1 == 0 = (net { netPrim = Identity w0, netInputs = [i0] }, True)
  | otherwise = (net, False)
zeroWidthNetTransform net@Net{ netPrim = Display args, netInputs = inpts } =
  let bitArgs = [ ba | ba@(DisplayArgBit w) <- args ]
      f (DisplayArgBit 0) i = (InputTree (Const 0 0) [], True)
      f _ i = (i, False)
      (tmps, changes) = unzip $ zipWith f bitArgs (tail inpts)
      inpts' = head inpts : tmps
  in (net { netInputs = inpts' }, or changes)
zeroWidthNetTransform net@Net{ netPrim = prim@Custom{ customInputs = primIns
                                                    , customOutputs = primOuts }
                             , netInputs = netIns }
  | any (\(_, x) -> x == 0) primIns ||
    any (\(_, y) -> y == 0) primOuts =
    ( net { netPrim = prim { customInputs = primIns'
                           , customOutputs = primOuts' }
          , netInputs = netIns' }
    , True )
  | otherwise = (net, False)
  where ins = zip netIns primIns
        ins' = [x | x@(_, (_, w)) <- ins, w /= 0]
        (netIns', primIns') = unzip ins'
        primOuts' = [x | x@(_, w) <- primOuts, w /= 0]
-- TODO currently unsupported cases that could be transformed
zeroWidthNetTransform net@Net{ netPrim = BRAM { ramAddrWidth = 0 } } =
  error "zeroWidthNetTransform unsupported on BRAM Prim"
zeroWidthNetTransform net@Net{ netPrim = TrueDualBRAM { ramAddrWidth = 0 } } =
  error "zeroWidthNetTransform unsupported on TrueDualBRAM Prim"
zeroWidthNetTransform
  net@Net{ netPrim = RegFileRead RegFileInfo{ regFileAddrWidth = 0 } } =
    error "zeroWidthNetTransform unsupported on RegFileRead Prim"
-- do nothing cases for all others
zeroWidthNetTransform net = (net, False)

-- | Tell if a root 'Net' can be optimized away during zero-witdh elimination,
-- that is a root 'Net' with no non-zero-width inputs or outputs
zeroWidthRootNetEliminationRule Net{ netPrim = prim } =
  primIsRoot prim && not (primDontKill prim) &&
  not (any (\(_, w) -> w /= 0) ins || any (\(_, w) -> w /= 0) outs)
  where ins = primInputs prim
        outs = primOutputs prim

-- | Ignore 0-width Nets pass
zeroWidthNetIgnore :: MNetlistPass Bool
zeroWidthNetIgnore nl = do
  pairs <- getAssocs nl -- list of nets with their index
  changed <- newIORef False -- keep track of modifications to the 'Netlist'
  -- For each 'Net' (in particular, those with a non-zero-width output) with at
  -- least one zero-width input, transform it into a 'Net' with no reference to
  -- any zero-width output 'Net'.
  forM_ [(a,b) | x@(a, Just b) <- pairs] $ \(idx, net@Net{netPrim = prim}) -> do
    let (net', netChanged) = zeroWidthNetTransform net
    when netChanged $ do writeArray nl idx (Just net')
                         writeIORef changed True
  -- Remove each zero-width root 'Net'
  forM_ [ i | x@(i, Just n) <- pairs
            , zeroWidthRootNetEliminationRule n ] $ \idx -> do
    writeArray nl idx Nothing
    writeIORef changed True
  -- finish pass
  -- DEBUG HELP -- x <- readIORef changed
  -- DEBUG HELP -- putStrLn $ "ignoreZeroWidthNet pass changed? " ++ show x
  readIORef changed

