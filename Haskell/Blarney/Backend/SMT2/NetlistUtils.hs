{-# LANGUAGE NoRebindableSyntax #-}

{-|
Module      : Blarney.Backend.SMT2.NetlistUtils
Description : Netlist utility functions for SMT2 pretty printing
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental

This module provides blarney Netlist pretty printing utilities for SMT2 code
generation
-}

module Blarney.Backend.SMT2.NetlistUtils (
  declareNLDatatype
, defineNLTransition
, defineChainTransition
) where

-- Standard imports
import Data.List
import Data.STRef
import Data.Array.ST
import Control.Monad.ST
import Text.PrettyPrint
import Data.Array.IArray
import Prelude hiding ((<>))
import qualified Data.Set as Set

-- Blarney imports
import Blarney.Netlist
import Blarney.Backend.SMT2.Utils

-- | Declare a datatype whose fields are specific to the provided 'Netlist'
declareNLDatatype :: Netlist -> [InstId] -> String -> Doc
declareNLDatatype nl netIds dtNm =
  declareDataType dtNm [] [("mk" ++ dtNm, map mkField netIds)]
  where mkField nId = let wId = (nId, Nothing)
                      in  (wireName nl wId, strSort nl wId)

-- | Declare the transistion function for a given root 'Net' in the provided
--   'Netlist'
defineNLTransition :: Netlist -> Net -> [InstId] -> String -> Doc
defineNLTransition nl n@Net { netPrim = p, netInputs = ins } state name
  | case p of Output _ _ -> True
              Assert _ _ -> True
              _          -> False =
    defineFun (text name) fArgs fRet fBody
  | otherwise = error $ "SMT2 backend error: cannot defineTransition on " ++
                        show n
  where inVar = "inpts"
        stVar = "prev"
        ctx = (nl, inVar, stVar)
        fArgs = [ (text inVar, text "Inputs")
                , (text stVar,  text "State") ]
        fRet  = text "(Tuple2 Bool State)"
        fBody = withBoundNets ctx filtered $ newTuple2 assertE stateUpdtE
        newTuple2 a b = applyOp (text "mkTuple2") [a, b]
        dontPrune i = case netPrim (getNet nl i) of Input      _ _ -> False
                                                    RegisterEn _ _ -> False
                                                    Register   _ _ -> False
                                                    Output     _ _ -> False
                                                    Assert     _ _ -> False
                                                    _              -> True
        sorted = topologicalSort nl
        filtered = [i | i <- sorted, dontPrune i]
        assertE = bvIsTrue $ showNetInput ctx (head ins)
        stateUpdtE
          | null state = text "mkState"
          | otherwise  = applyOp (text "mkState") $ regsInpts state
        regsInpts = map (regInpts . getNet nl)
        regInpts Net{ netInstId = idx
                    , netPrim = RegisterEn _ _
                    , netInputs = [en, regIn] } =
          applyOp (text "ite") [ bvIsTrue $ showNetInput ctx en
                               , showNetInput ctx regIn
                               , psep [showWire nl (idx, Nothing), text stVar] ]
        regInpts Net{ netPrim = Register _ _, netInputs = [inpt] } =
          showNetInput ctx inpt

-- | Define the repeated application of a function
defineChainTransition :: String -> String -> Doc
defineChainTransition tName cName =
  defineFunRec (text cName)
               [ (text "inpts", text "(ListX Inputs)")
               , (text "prevS", text "State") ]
               (text "(Tuple2 (ListX Bool) (ListX State))") fBody
  where fBody = matchBind (text "inpts")
                          [ (text "nil", lastRet)
                          , (text "(cons h t)", matchInvokeT) ]
        lastRet = applyOp (text "mkTuple2")
                          [ qualify (text "nil") (text "(ListX Bool)")
                          , applyOp (text "cons")
                                    [ text "prevS"
                                    , qualify (text "nil")
                                              (text "(ListX State)") ] ]
        matchInvokeT = matchBind (applyOp (text tName) [text "h", text "prevS"])
                                 [(text "(mkTuple2 ok nextS)", matchRecCall)]
        matchRecCall = matchBind (applyOp (text cName) [text "t", text "nextS"])
                                 [( text "(mkTuple2 oks ss)", recRet)]
        recRet = applyOp (text "mkTuple2")
                         [ applyOp (text "cons") [text "ok", text "oks"]
                         , applyOp (text "cons") [text "nextS", text "ss"] ]

-- internal helpers

-- | Pretty print the provided blarney 'Prim' with its inputs
showPrim :: Prim -> [Doc] -> Doc
-- nullary primitives
showPrim (Const w n)  [] = int2bv w n
showPrim (DontCare w) [] = int2bv w 0 -- XXX TODO make toplevel existential var -- existsBind [(char 'x', bvSort w)] (char 'x')
-- unary primitives
showPrim (Identity _) [i] = i
showPrim (ReplicateBit w) [i] = applyOp (text "concat") (replicate w i)
showPrim (ZeroExtend iw ow) [i] = applyOp (text "concat") [int2bv (ow-iw) 0, i]
showPrim (SignExtend iw ow) [i] =
  letBind [(sgn_var, sgn)] $ applyOp (text "concat")
                                     [ sep $ replicate (ow - iw) sgn_var, i ]
  where sgn = bvSlice msb_idx msb_idx i
        sgn_var = char 's'
        msb_idx = iw - 1
showPrim (SelectBits _ hi lo) [i] = bvSlice hi lo i
showPrim (Not _) ins = applyOp (text "bvnot")  ins
-- binary / multary primitives
showPrim (Add _)               ins = applyOp (text "bvadd")  ins
showPrim (Sub _)               ins = applyOp (text "bvsub")  ins
showPrim (Mul _ _ _)           ins = applyOp (text "bvmul")  ins
showPrim (Div _)               ins = applyOp (text "bvdiv")  ins
showPrim (Mod _)               ins = applyOp (text "bvmod")  ins
showPrim (And _)               ins = applyOp (text "bvand")  ins
showPrim (Or _)                ins = applyOp (text "bvor")   ins
showPrim (Xor _)               ins = applyOp (text "bvxor")  ins
showPrim (ShiftLeft _ _)       ins = applyOp (text "bvshl")  ins
showPrim (ShiftRight _ _)      ins = applyOp (text "bvlshr") ins
showPrim (ArithShiftRight _ _) ins = applyOp (text "bvashr") ins
showPrim (Concat _ _)          ins = applyOp (text "concat") ins
showPrim (LessThan _)          ins = bool2bv $ applyOp (text "bvult")  ins
showPrim (LessThanEq _)        ins = bool2bv $ applyOp (text "bvule")  ins
showPrim (Equal _)    ins = bool2bv $ applyOp (text "=") ins
showPrim (NotEqual w) ins = applyOp (text "bvnot") [showPrim (Equal w) ins]
showPrim (Mux n w) (sel:ins) = mux $ zip [0..] ins
  where selSz = (ceiling . logBase 2 . fromIntegral) n
        mux ((_,e):[]) = e
        mux ((i,e):xs) = applyOp (text "ite")
                                 [ applyOp (text "=") [sel, int2bv selSz i]
                                 , e, mux xs ]
-- unsupported primitives
showPrim p ins = error $
  "Blarney.Backend.SMT2.NetlistUtils: cannot showPrim Prim '" ++ show p ++ "'"

-- | Derive the wire name for the net output (= net id + net output name)
wireName :: Netlist -> WireId -> String
wireName nl (iId, m_outnm) = name ++ richNm ++ outnm ++ "_" ++ show iId
  where outnm = case m_outnm of Just nm -> nm
                                _       -> ""
        net = getNet nl iId
        richNm = case netPrim net of Input      _ nm -> "inpt_" ++ nm
                                     RegisterEn _ _  -> "reg"
                                     Register   _ _  -> "reg"
                                     _               -> ""
        name = genName $ netNameHints net

-- | Generate a 'String' from a 'NameHints'
genName :: NameHints -> String
genName hints
  | Set.null hints = "v"
  | otherwise = intercalate "_" $ filter (not . null) [prefx, root, sufx]
                where nms = Set.toList hints
                      prefxs = [nm | x@(NmPrefix _ nm) <- nms]
                      roots  = [nm | x@(NmRoot   _ nm) <- nms]
                      sufxs  = [nm | x@(NmSuffix _ nm) <- nms]
                      prefx  = intercalate "_" prefxs
                      root   = intercalate "_" roots
                      sufx   = intercalate "_" sufxs

-- | Pretty print a wire name using 'wireName'
showWire :: Netlist -> WireId -> Doc
showWire nl wId = text $ wireName nl wId

-- | Pretty print a 'NetInput' using 'showWire' and 'showPrim'
showNetInput :: (Netlist, String, String) -> NetInput -> Doc
showNetInput (nl, inpts, state) (InputWire (nId, m_nm)) =
  case netPrim $ getNet nl nId of
    Input      _ _ -> parens $ showWire nl (nId, m_nm) <+> text inpts
    RegisterEn _ _ -> parens $ showWire nl (nId, m_nm) <+> text state
    Register   _ _ -> parens $ showWire nl (nId, m_nm) <+> text state
    _              -> showWire nl (nId, m_nm)
showNetInput ctx (InputTree p ins) = showPrim p (showNetInput ctx <$> ins)

-- | Pretty print the net at the given 'InstId' in the give 'Netlist' using
--   'showNetInput'
showNet :: (Netlist, String, String) -> InstId -> Doc
showNet ctx@(nl, _, _) instId =
  showPrim (netPrim net) (showNetInput ctx <$> netInputs net)
  where net = getNet nl instId

-- | Wrap the 'Doc' to pretty print in nested let binding (using 'nestLetBind')
--   introducing one bouund variable for each net in the given '[InstId]'
withBoundNets :: (Netlist, String, String) -> [InstId] -> Doc -> Doc
withBoundNets _ [] doc = doc
withBoundNets ctx@(nl, _, _) instIds doc = nestLetBind decls doc
  where decls = map (\i -> [(showWire nl (i, Nothing), showNet ctx i)]) instIds

-- | Return a 'String' representation of the SMT2 sort of the net with the given
--   'WireId'
strSort :: Netlist -> WireId -> String
strSort nl (instId, m_outnm) = bvSortStr w
  where w = primOutWidth (netPrim $ getNet nl instId) m_outnm

-- topological stort of a netlist
-- TODO: move to Netlist passes module
--------------------------------------------------------------------------------

-- | the 'Mark' type is only useful to the 'topologicalSort' function and should
--   not be exported
data Mark = Unmarked | Temporary | Permanent
-- | get a topologically sorted '[InstId]' for the given 'Netlist'
topologicalSort :: Netlist -> [InstId]
topologicalSort nl = runST do
  -- initialise state for the topological sorting
  visited <- newArray (bounds nl) Unmarked -- track visit through the netlist
  sorted  <- newSTRef [] -- sorted list as a result
  roots  <- newSTRef relevantRoots -- queue of roots to explore next
  -- run the internal topological sort while there are roots to explore
  whileM_ (notEmpty roots) do
    root <- pop roots -- consume a root
    topoSort visited sorted roots root -- explore from the consumed root
  -- return the sorted list of InstId
  reverse <$> readSTRef sorted
  -- helpers
  where
    -- identify Netlist's outputs and state holding elements
    relevantRoots = [ netInstId n | Just n@Net{netPrim = p} <- elems nl
                                  , case p of RegisterEn _ _ -> True
                                              Register   _ _ -> True
                                              Output     _ _ -> True
                                              Assert     _ _ -> True
                                              _              -> False ]

    whileM_ :: Monad m => m Bool -> m a -> m ()
    whileM_ pred act = pred >>= \x -> if x then act >> whileM_ pred act else return ()
    -- identify leaf net
    isLeaf :: Net -> Bool
    isLeaf Net{ netPrim = Input      _ _ } = True
    isLeaf Net{ netPrim = RegisterEn _ _ } = True
    isLeaf Net{ netPrim = Register   _ _ } = True
    isLeaf _                               = False
    -- helpers to use an STRef [a] as a stack
    push stck elem = modifySTRef' stck $ \xs -> elem : xs
    pushN stck elems = modifySTRef' stck $ \xs -> elems ++ xs
    pop stck = do top <- head <$> readSTRef stck
                  modifySTRef' stck $ \xs -> tail xs
                  return top
    notEmpty stck = do l <- readSTRef stck
                       return (not . null $ l)
    -- the actual recursive topological sort algorithm
    topoSort :: STArray s InstId Mark -> STRef s [InstId] -> STRef s [InstId]
             -> InstId
             -> ST s ()
    topoSort visited sorted roots netId = do
      let net = getNet nl netId
      -- retrieve the 'visited' array entry for the current net
      netVisit <- readArray visited netId
      case netVisit of
        -- For already visited nets, do not do anything
        Permanent -> return ()
        -- For nets under visit, we identified a combinational cycle
        -- (unsupported ==> error out)
        Temporary -> error $ "SMT2 backend error: " ++
                             "combinational cycle detected -- " ++ show net
        -- For new visits:
        Unmarked -> do
          let allInputIds = concatMap (map fst . netInputWireIds)
                                      (netInputs net)
          -- For leaf nets, strop here and mark inputs as roots for next
          -- toplevel call
          if isLeaf net then do pushN roots allInputIds
          -- For non leaf nets, mark net as temporarily under visit and explore
          -- all inputs recursively
          else do writeArray visited netId Temporary
                  mapM_ (topoSort visited sorted roots) allInputIds
          -- Mark net as permanent and insert it into the sorted list
          writeArray visited netId Permanent
          push sorted netId
