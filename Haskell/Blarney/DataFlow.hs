{-

This module introduces data-flow ordering on netlists.  It does so
using a depth-first (post-order) traversal starting from each root.  A
root is: (1) a component with no outputs; or (2) the child of a
register.  A leaf is: (1) any component with no inputs; (2) a
register.  A visited set is used to ensure that the same subtree is
not traversed more than once.  An ancestor set is used to detect
cycles.  Such cycles must be combinatorial cycles since registers are
considered as leaves.

-}

module Blarney.DataFlow
  ( dataFlow
  ) where

import Blarney.Unbit
import qualified Blarney.JList as JL
import qualified Data.IntSet as S
import Data.Array as A

-- Is given net a leaf?
isLeaf :: Net -> Bool
isLeaf net = 
  case netPrim net of
    Register i w   -> True
    RegisterEn i w -> True
    Const i w      -> True
    other          -> False

-- Return nets in data-flow order
dataFlow :: [Net] -> [Net]
dataFlow nets =
    JL.toList 
  . fst
  . dfsList S.empty S.empty
  $ roots
  where
    -- Number of nets
    n = length nets

    -- Array mapping net ids to nets
    netArray = array (0, n-1) [(netInstId net, net) | net <- nets]

    -- Lookup net
    lookup i = netArray A.! i

    -- Roots of netlist
    roots = concatMap root nets

    -- Is given net a root?
    root net =
      case netPrim net of
        Register i w   -> map (lookup . fst) (netInputs net)
        RegisterEn i w -> map (lookup . fst) (netInputs net)
        Display args   -> [net]
        Finish         -> [net]
        other          -> []

    -- DFS from a list of root nodes
    dfsList as vs nets =
      case nets of
        []   -> (JL.Zero, vs)
        n:ns -> let (left, vs')  = dfs as vs n
                    (rest, vs'') = dfsList as vs' ns
                in  (left JL.:+: rest, vs'')

    -- DFS from a single root node
    dfs as vs net
      | id `S.member` as = error "Combinatorial cycle detected"
      | id `S.member` vs = (JL.Zero, vs)
      | isLeaf net       = (JL.One net, S.insert id vs)
      | otherwise        = (cs JL.:+: JL.One net, vs')
      where
        id        = netInstId net
        children  = map (lookup . fst) (netInputs net)
        (cs, vs') = dfsList (S.insert id as) (S.insert id vs) children
