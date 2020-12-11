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
showSMT2Script nm nl =
      char ';' <+> text (replicate 78 '-')
  $+$ sep (catMaybes $ map defineNode netSMT2s)
  $+$ text "; satisfiability checking"
  $+$ char ';' <+> text (replicate 78 '-')
  $+$ sep (catMaybes $ map checkNode netSMT2s)
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
showBVSizedDecLit :: Int -> Int -> Doc
showBVSizedDecLit w n =
  parens (parens (char '_' <+> text "int2bv" <+> int w) <+> int n)
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
bvIsFalse :: Doc -> Doc
bvIsFalse doc = parens $ text "=" <+> doc <+> text "#b0"
bvIsTrue :: Doc -> Doc
bvIsTrue doc = parens $ text "=" <+> doc <+> text "#b1"
bv2Bool :: Doc -> Doc
bv2Bool = bvIsTrue
bool2BV :: Doc -> Doc
bool2BV doc = parens $ text "ite" <+> doc <+> text "#b1" <+> text "#b0"
univQuant :: [(Doc, Doc)] -> Doc -> Doc
univQuant [] doc = doc
univQuant xs doc = parens $ sep [ text "forall", vars, doc ]
  where vars = parens $ sep $ map (\(v, s) -> parens $ v <+> s) xs

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
showPrim nl (Const w n) [] = showBVSizedDecLit w (fromInteger n)
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
  parens $ text "concat" <+> showBVSizedDecLit (ow - iw) 0 <+> showNetInput nl e0
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

showDeclareDatatype :: Netlist -> [InstId] -> String -> Doc
showDeclareDatatype nl netIds nm =
  parens $ sep [ text "declare-datatype", dtName, parens dtCons ]
  where dtName = char 'T' <> text nm
        dtCons = parens $ sep $ (text "newT" <> text nm) : (map mkField netIds)
        mkField nId = let wId = (nId, Nothing)
                      in parens $ showWire nl wId <+> showSort nl wId

showDefineTransition :: Netlist -> Net -> Doc
showDefineTransition nl n@Net { netPrim = Output _ nm, netInputs = [e0] } =
     showDeclareDatatype nl inputs "inputs"
  $$ showDeclareDatatype nl state "state"
  $$ parens (sep [ text "define-fun", fName, fArgs, fRet, fBody ])
  $$ parens (sep [ text "assert", univQuant aArgs aBody ])
  where aArgs = [ (text "init",      text "Bool")
                , (text "inpts",     text "Tinputs")
                , (text "prev",      text "Tstate")
                , (text "next",      text "Tstate")
                , (text "predHolds", text "Bool") ]
        aBody = parens $   text "=>"
                       <+> parens (hsep [ fName
                                        , text "init"
                                        , text "inpts"
                                        , text "prev"
                                        , text "next"
                                        , text "predHolds" ])
                       <+> text "predHolds"
        fName = text $ "t_" ++ nm
        fArgs = parens $ hsep [ parens (text "init Bool")
                              , parens (text "inpts Tinputs")
                              , parens (text "prev Tstate")
                              , parens (text "next Tstate")
                              , parens (text "predHolds Bool") ]
        fRet  = text "Bool"
        fBody = withLetBindings nl filtered $
                                parens $ text "and" <+> sep [ assertExpr
                                                            , stateUpdtExpr ]
        dontPrune i = case netPrim (getNet nl i) of Input      _ _ -> False
                                                    Register   _ _ -> False
                                                    RegisterEn _ _ -> False
                                                    Output     _ _ -> False
                                                    _              -> True
        (sorted, inputs, state) = topologicalSort nl n
        filtered = [i | i <- sorted, dontPrune i]
        assertExpr = parens $ char '=' <+> text "predHolds"
                                       <+> bvIsFalse (showNetInput nl e0)
        stateUpdtExpr = empty -- TODO
showDefineTransition _ n = error $ "SMT2 backend error: cannot showAssertNode on'" ++ show n ++ "'"

withLetBindings :: Netlist -> [InstId] -> Doc -> Doc
withLetBindings _ [] doc = doc
withLetBindings nl (idx:idxs) doc =
  parens $ text "let" <+> parens (decl idx) $$ nest (-1) (withLetBindings nl idxs doc)
  where decl i = parens $ showWire nl (i, Nothing) <+> showNet nl i

-- topological stort of a netlist
--------------------------------------------------------------------------------

type Depth = Int
data Mark = Unmarked | Temporary Depth | Permanent

topologicalSort :: Netlist -> Net -> ([InstId], [InstId], [InstId])
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
             -> STRef s [InstId]
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
          writeArray visited netId $ Temporary curDepth
          let net = getNet nl netId
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
            RegisterEn _ _ -> insert state netId
            Register   _ _ -> insert state netId
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
  Output     _ _ -> dflt { defineNode = Just $ showDefineTransition nl n
                         --, assertNode = Just $ showAssertNode nl n
                         , checkNode  = Just $ text "(check-sat)" }
  _          -> error $ "SMT2 backend error: genNetSMT2 called on " ++ show n ++
                        "- Only Input and Output primitives are supported"
  where dflt = NetSMT2 { defineNode = Nothing
                       , assertNode = Nothing
                       , checkNode  = Nothing }
