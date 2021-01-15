{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE NoRebindableSyntax #-}

{-|
Module      : Blarney.Backend.SMT.NetlistUtils
Description : Netlist utility functions for SMT pretty printing
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

This module provides blarney Netlist pretty printing utilities for SMT code
generation
-}

module Blarney.Backend.SMT.NetlistUtils (
  declareNLDatatype
, mkNLDatatype
, defineNLTransition
, defineChainTransition
, assertBounded
, assertInduction
, wireName
) where

-- Standard imports
import Data.List
import Text.PrettyPrint
import Prelude hiding ((<>))
import qualified Data.Set as Set

-- Blarney imports
import Blarney.Netlist
import Blarney.Backend.SMT.Utils
import Blarney.Backend.SMT.BasicDefinitions

-- | Declare a datatype whose fields are specific to the provided 'Netlist'
declareNLDatatype :: Netlist -> [InstId] -> String -> Doc
declareNLDatatype nl netIds dtNm =
  declareDataType dtNm [] [("mk" ++ dtNm, map mkField netIds)]
  where mkField nId = let wId = (nId, Nothing)
                      in  (wireName nl wId, strSort nl wId)

-- | Invoke the constructor for a datatype declared with 'declareNLDatatype'
mkNLDatatype :: String -> [Doc] -> Doc
mkNLDatatype dtNm [] = text $ "mk" ++ dtNm
mkNLDatatype dtNm dtFields = applyOp (text $ "mk" ++ dtNm) dtFields

-- | Declare the transition function for a given root 'Net' in the provided
--   'Netlist'
defineNLTransition :: Netlist
                   -> Net
                   -> (String, String)
                   -> [InstId]
                   -> [InstId]
                   -> String
                   -> Doc
defineNLTransition nl n@Net { netPrim = p, netInputs = ins }
                   (inptType, stType) sortedNl state name
  | case p of Output _ _ -> True
              Assert _   -> True
              _          -> False =
    defineFun (text name) fArgs fRet fBody
  | otherwise = error $ "Blarney.Backend.SMT.NetlistUtils: " ++
                        "cannot defineNLTransition on " ++ show n
  where inVar = "inpts"
        stVar = "prev"
        ctx = (nl, inVar, stVar)
        fArgs = [ (text inVar, text inptType)
                , (text stVar,  text stType) ]
        fRet  = text $ "(Tuple2 Bool " ++ stType ++ ")"
        fBody = withBoundNets ctx filteredNl $ newTuple2 assertE stateUpdtE
        newTuple2 a b = applyOp (text "mkTuple2") [a, b]
        dontPrune i = case netPrim (getNet nl i) of Input      _ _ -> False
                                                    RegisterEn _ _ -> False
                                                    Register   _ _ -> False
                                                    Output     _ _ -> False
                                                    Assert     _   -> False
                                                    _              -> True
        filteredNl = [i | i <- sortedNl, dontPrune i]
        assertE = case p of
          Output _ _ -> bvIsTrue $ showNetInput ctx (head ins)
          Assert _ ->
            applyOp (text "=>") $ map (bvIsTrue . showNetInput ctx) ins
        stateUpdtE = mkNLDatatype stType $ regsInpts state
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
defineChainTransition :: String -> (String, String) -> String -> Doc
defineChainTransition tName argTypes cName =
  defineChain cName (tName, argTypes, "Bool")

-- | Define inputs and assertion of the bounded property represented by the
--   provided chaining function, with a given initial state, and for a given
--   depth
assertBounded :: String
              -> (String, String)
              -> [(Integer, InputWidth)]
              -> Int
              -> Doc
assertBounded cFun (inptType, stType) initS depth = decls $+$ assertion
  where inpts = [ "in" ++ show i | i <- [0 .. depth-1] ]
        decls = vcat $
                  map (\i -> text $ "(declare-const "++i++" "++inptType++")")
                      inpts
        assertion = applyOp (text "assert") [ letBind bindArgs matchInvoke ]
        bindArgs = [ (text "inpts", mkListX inpts inptType)
                   , (text "initS", createState initS) ]
        createState xs = mkNLDatatype stType (map (\(v, w) -> int2bv w v) xs)
        matchInvoke = matchBind (applyOp (text cFun)
                                         [text "inpts", text "initS"])
                                [( text "(mkTuple2 oks ss)"
                                 , applyOp (text "not")
                                           [applyOp (text "andReduce")
                                                    [text "oks"]] )]

-- | Define inputs and assertion of the induction step for proof by induction of
--   the provided chaining function for the checked property, for a given
--   induction depth and with optional state restriction.
--   To assert a base case, the 'assertBounded' function can be used.
assertInduction :: String -> (String, String) -> Int -> Bool -> Doc
assertInduction cFun (inptType, stType) depth restrict = decls $+$ assertion
  where inpts = [ "in" ++ show i | i <- [0 .. depth] ]
        decls = vcat $ (text $ "(declare-const startS " ++ stType ++ ")") :
                  map (\i -> text $ "(declare-const "++i++" "++inptType++")")
                      inpts
        assertion = applyOp (text "assert") [ letBind bindArgs matchInvoke ]
        bindArgs = [ (text "inpts", mkListX inpts inptType) ]
        matchInvoke = matchBind (applyOp (text cFun)
                                         [text "inpts", text "startS"])
                                [( text "(mkTuple2 oks ss)"
                                 , applyOp (text "not") [propHolds] )]
        propHolds =
          applyOp (text "impliesReduce")
                  if not restrict then [ text "oks" ]
                  else [ applyOp (text "cons")
                                 [ applyOp
                                     (text $ "allDifferent_ListX_" ++ stType)
                                     [applyOp (text $ "init_ListX_" ++ stType)
                                              [text "ss"]]
                                 , text "oks" ]]

-- | Derive the name of the signal referred to by the given 'WireId'
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
  "Blarney.Backend.SMT.NetlistUtils: cannot showPrim Prim '" ++ show p ++ "'"

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

-- | Return a 'String' representation of the SMT sort of the net with the given
--   'WireId'
strSort :: Netlist -> WireId -> String
strSort nl (instId, m_outnm) = bvSortStr w
  where w = primOutWidth (netPrim $ getNet nl instId) m_outnm
