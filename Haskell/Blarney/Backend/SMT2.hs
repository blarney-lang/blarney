{-# LANGUAGE NoRebindableSyntax #-}

{-|
Module      : Blarney.Backend.SMT2
Description : SMT2 generation
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental

Convert Blarney Netlist to SMT2 scripts.
-}

module Blarney.Backend.SMT2 (
  genSMT2Script
) where

-- Standard imports
import Data.Char
import Data.List
import System.IO
import Data.Maybe
import Data.STRef
import Data.Array.ST
import System.Process
import Control.Monad.ST
import Text.PrettyPrint
import Data.Array.IArray
import Prelude hiding ((<>))
import Numeric (showIntAtBase)
import qualified Data.Set as Set

-- Blarney imports
import Blarney.Netlist

-- Toplevel API
--------------------------------------------------------------------------------

-- | Convert given Blarney Netlist to an SMT2 script
genSMT2Script :: Netlist -- ^ Blarney netlist
              -> String  -- ^ Script name
              -> String  -- ^ Output directory
              -> IO ()
genSMT2Script nl nm dir =
  do system ("mkdir -p " ++ dir)
     h <- openFile fileName WriteMode
     hPutStr h (render $ showSMT2Script nm nl)
     hClose h
  where fileName = dir ++ "/" ++ nm ++ ".smt2"

-- Internals
--------------------------------------------------------------------------------

showSMT2Script :: String -> Netlist -> Doc
showSMT2Script nm nl = sep (catMaybes $ map defineNode netSMT2s)
  where nets = catMaybes $ elems nl
        netSMT2s = map (genNetSMT2 nl) [ n | n <- nets, case netPrim n of
                                                          --Input      _ _ -> True
                                                          --Register   _ _ -> True
                                                          --RegisterEn _ _ -> True
                                                          Output     _ _ -> True
                                                          _ -> False ]

-- basic internal helpers
--------------------------------------------------------------------------------

showAtBase :: (Integral a, Show a) => a -> a -> Doc
showAtBase b n = text $ showIntAtBase b intToDigit n ""
showHex :: (Integral a, Show a) => a -> Doc
showHex n = showAtBase 16 n
showBin :: (Integral a, Show a) => a -> Doc
showBin n = showAtBase 2 n
showBVHexLit :: Integer -> Doc
showBVHexLit n = text "#x" <> showHex n
showBVBinLit :: Integer -> Doc
showBVBinLit n = text "#b" <> showBin n
showBVSlice :: Int-> Int-> Doc -> Doc
showBVSlice hi lo bv =
  parens $ (parens $ char '_' <+> text "extract" <+> int hi <+> int lo) <+> bv
showBVSort :: Int -> Doc
showBVSort n = parens $ char '_' <+> text "BitVec" <+> int n
genName :: NameHints -> Doc
genName hints
  | Set.null hints = char 'v'
  | otherwise = text $ intercalate "_" $ filter (not . null) [prefx, root, sufx]
                where nms = Set.toList hints
                      prefxs = [nm | x@(NmPrefix _ nm) <- nms]
                      roots  = [nm | x@(NmRoot   _ nm) <- nms]
                      sufxs  = [nm | x@(NmSuffix _ nm) <- nms]
                      prefx  = intercalate "_" prefxs
                      root   = intercalate "_" roots
                      sufx   = intercalate "_" sufxs

int2bv :: Int -> Integer -> Doc
int2bv w n =
  parens $ parens (char '_' <+> text "int2bv" <+> int w) <+> int (fromInteger n)
bvIsFalse :: Doc -> Doc
bvIsFalse doc = parens $ text "=" <+> doc <+> text "#b0"
bvIsTrue :: Doc -> Doc
bvIsTrue doc = parens $ text "=" <+> doc <+> text "#b1"
bv2Bool :: Doc -> Doc
bv2Bool = bvIsTrue
bool2BV :: Doc -> Doc
bool2BV doc = parens $ text "ite" <+> doc <+> text "#b1" <+> text "#b0"
parenList :: [(Doc, Doc)] -> Doc
parenList = parens . sep . map (\(v, s) -> parens $ sep [v, s])
-- binders
quantify :: Doc -> [(Doc, Doc)] -> Doc -> Doc
quantify _ [] doc = doc
quantify q xs doc = parens $ sep [ q, parenList xs, doc ]
univQuant :: [(Doc, Doc)] -> Doc -> Doc
univQuant = quantify $ text "forall"
existQuant :: [(Doc, Doc)] -> Doc -> Doc
existQuant = quantify $ text "exists"
letBind :: [(Doc, Doc)] -> Doc -> Doc
letBind [] doc = doc
letBind ((lhs, rhs):xs) doc =
  parens $  text "let" <+> parens (parens (sep [lhs, rhs]))
         $$ nest (-1) (letBind xs doc)
matchBind :: Doc -> [(Doc, Doc)] -> Doc
matchBind doc [] = doc
matchBind doc matches = parens $ sep [ text "match", doc, parenList matches ]
parBind :: [Doc] -> Doc -> Doc
parBind [] doc = doc
parBind params doc = parens $ text "par" <+> sep [ parens (sep params), doc ]

-- advanced internal helpers (rely on the netlist to lookup information)
--------------------------------------------------------------------------------

getNet :: Netlist -> InstId -> Net
getNet nl i =
  fromMaybe (error $ "SMT2 backend error: " ++
                     "access to non existing Net at instId " ++ show i)
            (nl ! i)

showNet :: Netlist -> InstId -> Doc
showNet nl instId = showPrim nl (netPrim net) (netInputs net)
  where net = getNet nl instId

showPrim :: Netlist -> Prim -> [NetInput] -> Doc
showPrim nl (Const w n) [] = int2bv w n
showPrim nl (DontCare w) [] = parens $  text "exists ((x " <> showBVSort w <> text ")) (x)"
showPrim nl (Add _) ins = parens $ text "bvadd" <+> sep (map (showNetInput nl) ins)
showPrim nl (Sub _) ins = parens $ text "bvsub" <+> sep (map (showNetInput nl) ins)
showPrim nl (Mul _ _ _) ins = parens $ text "bvmul" <+> sep (map (showNetInput nl) ins)
showPrim nl (Div _) ins = parens $ text "bvdiv" <+> sep (map (showNetInput nl) ins)
showPrim nl (Mod _) ins = parens $ text "bvmod" <+> sep (map (showNetInput nl) ins)
showPrim nl (And _) ins = parens $ text "bvand" <+> sep (map (showNetInput nl) ins)
showPrim nl (Or _) ins = parens $ text "bvor" <+> sep (map (showNetInput nl) ins)
showPrim nl (Xor _) ins = parens $ text "bvxor" <+> sep (map (showNetInput nl) ins)
showPrim nl (Not _) ins = parens $ text "bvneg" <+> sep (map (showNetInput nl) ins)
showPrim nl (ShiftLeft _ _) ins = parens $ text "bvshl" <+> sep (map (showNetInput nl) ins)
showPrim nl (ShiftRight _ _) ins = parens $ text "bvlshr" <+> sep (map (showNetInput nl) ins)
showPrim nl (ArithShiftRight _ _) ins = parens $ text "bvashr" <+> sep (map (showNetInput nl) ins)
showPrim nl (Equal _) ins = bool2BV $ parens $ char '=' <+> sep (map (showNetInput nl) ins)
showPrim nl (NotEqual w) ins = parens $ text "bvnot" <+> showPrim nl (Equal w) ins
showPrim nl (LessThan _) ins = parens $ text "bvult" <+> sep (map (showNetInput nl) ins)
showPrim nl (LessThanEq _) ins = parens $ text "bvule" <+> sep (map (showNetInput nl) ins)
showPrim nl (ReplicateBit w) [e0] = parens $ text "concat" <+> sep (map (showNetInput nl) $ replicate w e0) -- XXX check that we only ever get single bit inputs
showPrim nl (ZeroExtend iw ow) [e0] =
  parens $ text "concat" <+> int2bv (ow - iw) 0 <+> showNetInput nl e0
showPrim nl (SignExtend iw ow) [e0] = -- XX TODO could benefit from using a let quantifier for the sign bit?
  parens $ text "concat" <+> sep (replicate (ow - iw) sgn) <+> inpt
  where sgn = showBVSlice msb_idx msb_idx inpt
        msb_idx = iw - 1
        inpt = showNetInput nl e0
showPrim nl (SelectBits _ hi lo) [e0] = showBVSlice hi lo (showNetInput nl e0)
showPrim nl (Concat _ _) ins = parens $ text "concat" <+> sep (map (showNetInput nl) ins)
showPrim nl (Identity _) [e0] = showNetInput nl e0
showPrim nl p ins = error $
  "SMT2 backend error: cannot showPrim Prim '" ++ show p ++ "'"

showNetInput :: Netlist -> NetInput -> Doc
showNetInput nl (InputWire (nId, m_nm)) = case netPrim $ getNet nl nId of
  Input    _ _ -> parens $ showWire nl (nId, m_nm) <+> text "inpts"
  Register _ _ -> parens $ showWire nl (nId, m_nm) <+> text "prev"
  _            -> showWire nl (nId, m_nm)
showNetInput nl (InputTree p ins) = showPrim nl p ins

showWire :: Netlist -> WireId -> Doc
showWire nl (instId, m_outnm) = name <> richNm <> outnm <> char '_' <> int instId
  where outnm = case m_outnm of Just nm -> text nm
                                _       -> mempty
        net = getNet nl instId
        richNm = case netPrim net of Input      _ nm -> text $ "inpt_" ++ nm
                                     Register   _ _  -> text $ "reg"
                                     RegisterEn _ _  -> text $ "reg"
                                     _               -> empty
        name = genName $ netNameHints net

showSort :: Netlist -> WireId -> Doc
showSort nl (instId, m_outnm) = showBVSort w
  where w = primOutWidth (netPrim $ getNet nl instId) m_outnm

isLeafPrim :: Prim -> Bool
isLeafPrim (Input      _ _) = True
isLeafPrim (Register   _ _) = True
isLeafPrim (RegisterEn _ _) = True
isLeafPrim _                = False

showDefineNode :: Netlist -> Net -> Doc
showDefineNode nl n@Net { netInstId = instId, netPrim = p } | isLeafPrim p =
  parens $ text "declare-const"
           <+> showWire nl (instId, Nothing)
           <+> showBVSort w
  where w = primOutWidth p Nothing
showDefineNode _ n = error $ "SMT2 backend error: cannot showDefineNode on'" ++ show n ++ "'"

showAssertNode :: Netlist -> Net -> Doc
showAssertNode nl n@Net { netPrim = Output _ _, netInputs = [e0] } =
  parens $ text "assert" <+> wrapLet
  where dontPrune i = case netPrim (getNet nl i) of Input      _ _ -> False
                                                    Register   _ _ -> False
                                                    RegisterEn _ _ -> False
                                                    Output     _ _ -> False
                                                    _              -> True
        (sorted, _, _) = topologicalSort nl n
        filtered = [i | i <- sorted, dontPrune i]
        wrapLet = withLetBindings nl filtered assertExpr
        assertExpr = bvIsFalse $ showNetInput nl e0
showAssertNode nl n@Net { netPrim = Register _ _, netInputs = [e0] } =
  parens $ text "assert" <+> wrapLet
  where dontPrune i = case netPrim (getNet nl i) of Input      _ _ -> False
                                                    Register   _ _ -> False
                                                    RegisterEn _ _ -> False
                                                    Output     _ _ -> False
                                                    _              -> True
        (sorted, _, _) = topologicalSort nl n
        filtered = [i | i <- sorted, dontPrune i]
        wrapLet = withLetBindings nl filtered assertExpr
        assertExpr = bvIsFalse $ showNetInput nl e0
showAssertNode _ n = error $ "SMT2 backend error: cannot showAssertNode on'" ++ show n ++ "'"

showDeclareRecordType :: Doc -> [Doc] -> [(Doc, Doc)] -> Doc
showDeclareRecordType dtName dtParams dtFields =
  parens $ sep [ text "declare-datatype", dtName, dtDef ]
  where dtDef = parBind dtParams $ parens dtCons
        dtCons = parens $ sep $ (text "mk" <> dtName) : (map mkField dtFields)
        mkField (v, s) = parens $ v <+> s

showDeclareNLDatatype :: Netlist -> [InstId] -> Doc -> Doc
showDeclareNLDatatype nl netIds dtNm =
  showDeclareRecordType dtNm [] $ map mkField netIds
  where mkField nId = let wId = (nId, Nothing)
                      in  (showWire nl wId, showSort nl wId)

showDefineTransition :: Netlist -> Net -> [InstId] -> Doc -> Doc
showDefineTransition nl n@Net { netPrim = Output _ nm
                              , netInputs = [e0] } state name =
  parens $ sep [ text "define-fun", name, fArgs, fRet, fBody ]
  where fArgs = parens $ hsep [ parens (text "inpts Inputs")
                              , parens (text "prev State") ]
        fRet  = text "(Pair Bool State)"
        fBody = withLetBindings nl filtered $ newPair assertE stateUpdtE
        newPair a b = parens $ text "mkPair" <+> sep [a, b]
        dontPrune i = case netPrim (getNet nl i) of Input      _ _ -> False
                                                    Register   _ _ -> False
                                                    RegisterEn _ _ -> False
                                                    Output     _ _ -> False
                                                    _              -> True
        (sorted, _, _) = topologicalSort nl n
        filtered = [i | i <- sorted, dontPrune i]
        assertE = bvIsFalse (showNetInput nl e0)
        stateUpdtE
          | null state = text "mkState"
          | otherwise  = parens . sep $ text "mkState" : regsInpts state
        regsInpts = map \i -> (showNetInput nl . head . netInputs . getNet nl) i
showDefineTransition _ n _ _ =
  error $ "SMT2 backend error: cannot showDefineTransition on '" ++ show n ++ "'"

invokeTransition :: Doc -> Doc -> Doc -> Doc
invokeTransition tFun tInpts tInitSt = parens $ hsep [ tFun, tInpts, tInitSt ]

showBaseCase :: [(Integer, InputWidth)] -> Doc -> Doc
showBaseCase initState tFun =
      text "(push)"
  $+$ toplvlIns
  $+$ parens (sep [ text "assert"
                  , letBind initStateArg $
                      matchBind invoke
                        [( parens $ text "mkPair ok next"
                         , body )]])
  $+$ text "(check-sat)"
  $+$ text "(pop)"
  where toplvlIns = text "(declare-const inpts Inputs)"
        body = parens $ text "= ok true"
        createState [] = text "mkState"
        createState xs = parens $   text "mkState"
                                <+> sep (map (\(v, w) -> int2bv w v) xs)
        initStateArg = [(text "initState", createState initState)]
        invoke = invokeTransition tFun (text "inpts") (text "initState")

showInductionStep :: Doc -> Doc
showInductionStep tFun =
     text "(push)"
  $+$ toplvlIns
  $+$ parens (sep [ text "assert"
                  , univQuant [(text "initState", text "State")] $
                     matchBind invoke0
                      [( parens $ text "mkPair ok0 next0"
                       , matchBind invoke1
                          [( parens $ text "mkPair ok1 next1"
                           , body)] )]])
  $+$ text "(check-sat)"
  $+$ text "(pop)"
  where toplvlIns = vcat [ text "(declare-const inpts0 Inputs)"
                         , text "(declare-const inpts1 Inputs)" ]
        body = parens $ sep [ text "and"
                            , parens $ text "= ok0 true"
                            , parens $ text "= ok1 true" ]
        invoke0 = invokeTransition tFun (text "inpts0") (text "initState")
        invoke1 = invokeTransition tFun (text "inpts1") (text "next0")

showAll :: Netlist -> Net -> Doc
showAll nl n@Net { netPrim = Output _ nm, netInputs = [e0] } =
      char ';' <> text (replicate 79 '-')
  $+$ text "; Defining a generic Pair sort"
  $+$ showDeclareRecordType (text "Pair") [ text "X"
                                          , text "Y" ]
                                          [ (text "fst", text "X")
                                          , (text "snd", text "Y") ]
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Defining the Inputs record type specific to the current netlist"
  $+$ showDeclareNLDatatype nl inputs (text "Inputs")
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Defining the State record type specific to the current netlist"
  $+$ showDeclareNLDatatype nl (map fst state) (text "State")
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Defining the transition function specific to the current netlist"
  $+$ showDefineTransition nl n (map fst state) fName
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Base case"
  $+$ showBaseCase (map snd state) fName
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Induction step"
  $+$ showInductionStep fName
  where fName = text $ "t_" ++ nm
        (sorted, inputs, state) = topologicalSort nl n
showAll _ n = error $ "SMT2 backend error: cannot showAll on'" ++ show n ++ "'"

withLetBindings :: Netlist -> [InstId] -> Doc -> Doc
withLetBindings _ [] doc = doc
withLetBindings nl instIds doc = letBind decls doc
  where decls = map (\i -> (showWire nl (i, Nothing), showNet nl i)) instIds

-- topological stort of a netlist
--------------------------------------------------------------------------------

type Depth = Int
data Mark = Unmarked | Temporary Depth | Permanent

topologicalSort :: Netlist -> Net
                -> ([InstId], [InstId], [(InstId, (Integer, InputWidth))])
topologicalSort nl root = runST do
  -- initialise a mutable array to track visit through the netlist
  visited <- newArray (bounds nl) Unmarked
  -- initialise lists to return
  sorted  <- newSTRef [] -- tolologically order of the netlist
  inputs  <- newSTRef [] -- list of input nets
  state   <- newSTRef [] -- list of state holding nets
  -- run the internal topological sort
  topoSort visited sorted inputs state 0 $ netInstId root
  -- return the lists of InstIds
  sortedLst <- readSTRef sorted
  inputsLst <- readSTRef inputs
  stateLst  <- readSTRef state
  return (reverse sortedLst, inputsLst, stateLst)
  -- helpers
  where
    -- | the actual recursive topological sort algorithm
    topoSort :: STArray s InstId Mark
             -> STRef s [InstId]
             -> STRef s [InstId]
             -> STRef s [(InstId, (Integer, InputWidth))]
             -> Depth
             -> InstId
             -> ST s ()
    topoSort visited sorted inputs state curDepth netId = do
      -- retrieve the 'visited' array entry for the current net
      netVisit <- readArray visited netId
      case netVisit of
        -- For already visited nets, do not do anything
        Permanent -> return ()
        -- For nets under visit ...
        Temporary tmpDepth
        -- ... at a shallower depth, do not do anything
          | tmpDepth < curDepth -> return ()
        -- ... at the same depth (or deeper ?), we identified a
        -- combinational cycle (unsupported ==> error out)
          | otherwise -> do let net = getNet nl netId
                            error $ "SMT2 backend error: " ++
                                    "combinational cycle detected -- " ++
                                    show net
        -- For new visits, mark the current net as temporarily under visit,
        -- and explore all inputs recursively. Update to a permanent mark
        -- upon termination of children visits. Add the net to the returned
        -- list head
        Unmarked -> do
          let net = getNet nl netId
          let mark = case netPrim net of RegisterEn _ _ -> Permanent
                                         Register   _ _ -> Permanent
                                         _              -> Temporary curDepth
          writeArray visited netId $ mark
          let allInputIds = concatMap (map fst . netInputWireIds)
                                      (netInputs net)
          let newDepth = case netPrim net of RegisterEn _ _ -> curDepth + 1
                                             Register   _ _ -> curDepth + 1
                                             _              -> curDepth
          mapM_ (topoSort visited sorted inputs state newDepth) allInputIds
          writeArray visited netId Permanent
          -- update return lists
          insert sorted netId -- topological order list
          case netPrim net of
            -- For registers, update the list of state holding nets
            RegisterEn init w -> insert state (netId, (init, w))
            Register   init w -> insert state (netId, (init, w))
            -- For Inputs, update the list of input nets
            Input      _ _ -> insert inputs netId
            -- Do nothing else otherwise
            _              -> return ()
    insert lst elem = modifySTRef' lst $ \xs -> elem : xs

-- generate NetSMT2
--------------------------------------------------------------------------------
data NetSMT2 = NetSMT2 { defineNode :: Maybe Doc
                       , assertNode :: Maybe Doc
                       , checkNode  :: Maybe Doc }
genNetSMT2 :: Netlist -> Net -> NetSMT2
genNetSMT2 nl n = case netPrim n of
  --Input      _ _ -> dflt { defineNode = Just $ showDefineNode nl n }
  --Register   _ _ -> dflt { defineNode = Just $ showDefineNode nl n
  --                       , assertNode = Just $ showAssertNode nl n }
  --RegisterEn _ _ -> dflt { defineNode = Just $ showDefineNode nl n
  --                       , assertNode = Just $ showAssertNode nl n }
  Output     _ _ -> dflt { defineNode = Just $ showAll nl n }
                         --, assertNode = Just $ showAssertNode nl n
                         --, checkNode  = Just $ text "(check-sat)" }
  _          -> error $ "SMT2 backend error: genNetSMT2 called on " ++ show n ++
                        "- Only Input and Output primitives are supported"
  where dflt = NetSMT2 { defineNode = Nothing
                       , assertNode = Nothing
                       , checkNode  = Nothing }
