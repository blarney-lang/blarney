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
      text "; model inputs definitions"
  $+$ text "; - Input primitives as named unconstrained functions"
  $+$ text "; - TODO ? DontCare primitives as named unconstrained functions"
  $+$ text "; - TODO ? Const as named function + assert node"
  $+$ char ';' <+> text (replicate 78 '-')
  $+$ sep (catMaybes $ map defineNode netSMT2s)
  $+$ text "; assertions for outputs"
  $+$ char ';' <+> text (replicate 78 '-')
  $+$ sep (catMaybes $ map assertNode netSMT2s)
  $+$ text "; satisfiability checking"
  $+$ char ';' <+> text (replicate 78 '-')
  $+$ sep (catMaybes $ map checkNode netSMT2s)
  where nets = catMaybes $ elems nl
        netSMT2s = map (genNetSMT2 nl) [ n | n <- nets, case netPrim n of
                                                          Input  _ _ -> True
                                                          Output _ _ -> True
                                                          _          -> False ]

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
genName :: NameHints -> Doc
genName hints
  | Set.null hints = char 'v'
  | otherwise = text $ intercalate "-" $ filter (not . null) [prefx, root, sufx]
                where nms = Set.toList hints
                      prefxs = [nm | x@(NmPrefix _ nm) <- nms]
                      roots  = [nm | x@(NmRoot   _ nm) <- nms]
                      sufxs  = [nm | x@(NmSuffix _ nm) <- nms]
                      prefx  = intercalate "-" prefxs
                      root   = intercalate "-" roots
                      sufx   = intercalate "-" sufxs
bvIsFalse :: Doc -> Doc
bvIsFalse doc = parens $ text "=" <+> doc <+> text "#b0"
bvIsTrue :: Doc -> Doc
bvIsTrue doc = parens $ text "=" <+> doc <+> text "#b1"
bv2Bool :: Doc -> Doc
bv2Bool = bvIsTrue
bool2BV :: Doc -> Doc
bool2BV doc = parens $ text "ite" <+> doc <+> text "#b1" <+> text "#b0"

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
-- TODO use forall quantifier ? -- showPrim nl (DontCare w) [] =
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
showNetInput nl (InputWire wId) = showWire nl wId
showNetInput nl (InputTree p ins) = showPrim nl p ins

showWire :: Netlist -> WireId -> Doc
showWire nl (instId, m_outnm) = name <> inNm <> outnm <> char '_' <> int instId
  where outnm = case m_outnm of Just nm -> text nm
                                _       -> mempty
        net = getNet nl instId
        inNm = case netPrim net of Input _ inptName -> text inptName
                                   _                -> empty
        name = genName $ netNameHints net

showDefineNode :: Netlist -> Net -> Doc
showDefineNode nl n@Net { netInstId = instId, netPrim = Input w _ } =
  parens $ text "declare-const"
           <+> showWire nl (instId, Nothing)
           <+> parens (char '_' <+> text "BitVec" <+> int w)
showDefineNode _ n = error $ "SMT2 backend error: cannot showDefineNode on'" ++ show n ++ "'"

showAssertNode :: Netlist -> Net -> Doc
showAssertNode nl n@Net { netPrim = Output _ _, netInputs = [e0] } =
  parens $ text "assert" <+> wrapLet
  where notInput i = case netPrim (getNet nl i) of Input _ _ -> False
                                                   _         -> True
        notOutput i = case netPrim (getNet nl i) of Output _ _ -> False
                                                    _         -> True
        sorted = topologicalSort nl n
        filtered = [i | i <- sorted, notInput i, notOutput i]
        wrapLet = withLetBindings nl filtered assertExpr
        assertExpr = bvIsFalse $ showNetInput nl e0
showAssertNode _ n = error $ "SMT2 backend error: cannot showAssertNode on'" ++ show n ++ "'"

withLetBindings :: Netlist -> [InstId] -> Doc -> Doc
withLetBindings _ [] doc = doc
withLetBindings nl (idx:idxs) doc =
  parens $ text "let" <+> parens (decl idx) $$ withLetBindings nl idxs doc
  where decl i = parens $ showWire nl (i, Nothing) <+> showNet nl i

-- topological stort of a netlist
--------------------------------------------------------------------------------

data Mark = Unmarked | Temporary | Permanent

topologicalSort :: Netlist -> Net -> [InstId]
topologicalSort nl root = runST do
  -- initialise a mutable array to track visit through the netlist
  visited <- newArray (bounds nl) Unmarked
  -- initialise a mutable list to build the sorted return list
  sorted  <- newSTRef []
  -- run the internal topological sort
  topoSort visited sorted $ netInstId root
  -- return the sorted list of InstIds
  reverse <$> readSTRef sorted
  -- helpers
  where topoSort :: STArray s InstId Mark
                 -> STRef s [InstId]
                 -> InstId
                 -> ST s ()
        topoSort visited sorted netId = do
          -- retrieve the 'visited' array entry for the current net
          netVisit <- readArray visited netId
          case netVisit of
            -- For already visited nets, do not do anything
            Permanent -> return ()
            -- If we encounter a net currently under visit, we identified a
            -- a cycle. (currently unsupported ==> error out)
            Temporary -> error $ "SMT2 backend error: not a DAG"
            -- For new visits, mark the current net as temporarily under visit,
            -- and explore all inputs recursively. Update to a permanent mark
            -- upon termination of children visits. Add the net to the returned
            -- list head
            Unmarked -> do
              writeArray visited netId Temporary
              let allInputIds = concatMap (map fst . netInputWireIds)
                                          (netInputs $ getNet nl netId)
              mapM_ (topoSort visited sorted) allInputIds
              writeArray visited netId Permanent
              modifySTRef sorted (\ids -> netId:ids)

-- generate NetSMT2
--------------------------------------------------------------------------------
data NetSMT2 = NetSMT2 { defineNode :: Maybe Doc
                       , assertNode :: Maybe Doc
                       , checkNode  :: Maybe Doc }
genNetSMT2 :: Netlist -> Net -> NetSMT2
genNetSMT2 nl n = case netPrim n of
  Input w s  -> dflt { defineNode = Just $ showDefineNode nl n }
  Output w s -> dflt { assertNode = Just $ showAssertNode nl n
                     , checkNode  = Just $ text "(check-sat)" }
  _          -> error $ "SMT2 backend error: genNetSMT2 called on " ++ show n ++
                        "- Only Input and Output primitives are supported"
  where dflt = NetSMT2 { defineNode = Nothing
                       , assertNode = Nothing
                       , checkNode  = Nothing }
