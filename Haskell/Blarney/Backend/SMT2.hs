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
import Control.Monad
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

-- | Convert given blarney 'Netlist' to an SMT2 script
genSMT2Script :: Netlist -- ^ blarney netlist
              -> String  -- ^ script name
              -> String  -- ^ output directory
              -> IO ()
genSMT2Script nl nm dir =
  do system ("mkdir -p " ++ dir)
     h <- openFile fileName WriteMode
     hPutStr h (render $ showAll nl (head outputs))
     hClose h
  where fileName = dir ++ "/" ++ nm ++ ".smt2"
        outputs = [ n | Just n@Net{netPrim=p} <- elems nl
                      , case p of Output _ _ -> True
                                  _          -> False ]

-- basic internal helpers for pretty printing SMT-lib bit vector code
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
parBind :: [Doc] -> Doc -> Doc
parBind [] doc = doc
parBind params doc = parens $ text "par" <+> sep [ parens (sep params), doc ]
flatBind :: Doc -> [(Doc, Doc)] -> Doc -> Doc
flatBind _ [] doc = doc
flatBind binder xs doc = parens $ binder <+> sep [ parenList xs, doc ]
forallBind :: [(Doc, Doc)] -> Doc -> Doc
forallBind = flatBind $ text "forall"
existsBind :: [(Doc, Doc)] -> Doc -> Doc
existsBind = flatBind $ text "exists"
matchBind :: Doc -> [(Doc, Doc)] -> Doc
matchBind doc [] = doc
matchBind doc matches = parens $ sep [ text "match", doc, parenList matches ]
letBind :: [(Doc, Doc)] -> Doc -> Doc
letBind = flatBind $ text "let"
nestLetBind :: [[(Doc, Doc)]] -> Doc -> Doc
nestLetBind [] doc = doc
nestLetBind (xs:xss) doc = letBind xs (nest (-5) (nestLetBind xss doc))
-- the 5 is the length of "let " + 1

-- internal helpers specific to blarney netlists
--------------------------------------------------------------------------------

-- | helper to retrieve a 'Net' from a 'Netlist' given an 'InstId'
--   TODO: move to Netlist module
getNet :: Netlist -> InstId -> Net
getNet nl i = fromMaybe err (nl ! i)
  where err = error $ "SMT2 backend error: " ++
                      "access to non existing Net at instId " ++ show i

-- | helper for 'showPrim' of multary primitives operations
multaryOp :: String -> [Doc] -> Doc
multaryOp name ins = parens $ text name <+> sep ins

-- | show SMT for netlist primitives
showPrim :: Prim -> [Doc] -> Doc
-- nullary primitives
showPrim (Const w n)  [] = int2bv w n
showPrim (DontCare w) [] = existsBind [(char 'x', showBVSort w)] (char 'x')
-- unary primitives
showPrim (Identity _) [i] = i
showPrim (ReplicateBit w) [i] = parens $ text "concat" <+> sep (replicate w i)
showPrim (ZeroExtend iw ow) [i] =
  parens $ text "concat" <+> int2bv (ow - iw) 0 <+> i
showPrim (SignExtend iw ow) [i] =
  letBind [(sgn_var, sgn)] $ parens $ sep [ text "concat"
                                          , sep $ replicate (ow - iw) sgn_var
                                          , i ]
  where sgn = showBVSlice msb_idx msb_idx i
        sgn_var = char 's'
        msb_idx = iw - 1
showPrim (SelectBits _ hi lo) [i] = showBVSlice hi lo i
-- binary / multary primitives
showPrim (Add _)               ins = multaryOp "bvadd"  ins
showPrim (Sub _)               ins = multaryOp "bvsub"  ins
showPrim (Mul _ _ _)           ins = multaryOp "bvmul"  ins
showPrim (Div _)               ins = multaryOp "bvdiv"  ins
showPrim (Mod _)               ins = multaryOp "bvmod"  ins
showPrim (And _)               ins = multaryOp "bvand"  ins
showPrim (Or _)                ins = multaryOp "bvor"   ins
showPrim (Xor _)               ins = multaryOp "bvxor"  ins
showPrim (Not _)               ins = multaryOp "bvneg"  ins
showPrim (ShiftLeft _ _)       ins = multaryOp "bvshl"  ins
showPrim (ShiftRight _ _)      ins = multaryOp "bvlshr" ins
showPrim (ArithShiftRight _ _) ins = multaryOp "bvashr" ins
showPrim (Concat _ _)          ins = multaryOp "concat" ins
showPrim (LessThan _)          ins = multaryOp "bvult"  ins
showPrim (LessThanEq _)        ins = multaryOp "bvule"  ins
showPrim (Equal _)    ins = bool2BV $ multaryOp "=" ins
showPrim (NotEqual w) ins = parens $ text "bvnot" <+> showPrim (Equal w) ins
-- unsupported primitives
showPrim p ins = error $
  "SMT2 backend error: cannot showPrim Prim '" ++ show p ++ "'"

-- | helper for 'showWire' to generate a 'Net' name
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

-- | derive the wire name for the net output (= net id + net output name)
showWire :: Netlist -> WireId -> Doc
showWire nl (iId, m_outnm) = name <> richNm <> outnm <> char '_' <> int iId
  where outnm = case m_outnm of Just nm -> text nm
                                _       -> mempty
        net = getNet nl iId
        richNm = case netPrim net of Input      _ nm -> text $ "inpt_" ++ nm
                                     Register   _ _  -> text $ "reg"
                                     RegisterEn _ _  -> text $ "reg"
                                     _               -> empty
        name = genName $ netNameHints net

-- | derive the SMT representation for a 'NetInput'
showNetInput :: Netlist -> NetInput -> Doc
showNetInput nl (InputWire (nId, m_nm)) = case netPrim $ getNet nl nId of
  Input    _ _ -> parens $ showWire nl (nId, m_nm) <+> text "inpts"
  Register _ _ -> parens $ showWire nl (nId, m_nm) <+> text "prev"
  _            -> showWire nl (nId, m_nm)
showNetInput nl (InputTree p ins) = showPrim p (showNetInput nl <$> ins)

-- | derive the SMT representation for a net
showNet :: Netlist -> InstId -> Doc
showNet nl instId = showPrim (netPrim net) (showNetInput nl <$> netInputs net)
  where net = getNet nl instId

withBoundNets :: Netlist -> [InstId] -> Doc -> Doc
withBoundNets _ [] doc = doc
withBoundNets nl instIds doc = nestLetBind decls doc
  where decls = map (\i -> [(showWire nl (i, Nothing), showNet nl i)]) instIds

showSort :: Netlist -> WireId -> Doc
showSort nl (instId, m_outnm) = showBVSort w
  where w = primOutWidth (netPrim $ getNet nl instId) m_outnm

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
        fBody = withBoundNets nl filtered $ newPair assertE stateUpdtE
        newPair a b = parens $ text "mkPair" <+> sep [a, b]
        dontPrune i = case netPrim (getNet nl i) of Input      _ _ -> False
                                                    Register   _ _ -> False
                                                    RegisterEn _ _ -> False
                                                    Output     _ _ -> False
                                                    _              -> True
        sorted = topologicalSort nl
        filtered = [i | i <- sorted, dontPrune i]
        assertE = bvIsFalse (showNetInput nl e0)
        stateUpdtE
          | null state = text "mkState"
          | otherwise  = parens . sep $ text "mkState" : regsInpts state
        regsInpts = map \i -> (showNetInput nl . head . netInputs . getNet nl) i
showDefineTransition _ n _ _ =
  error $ "SMT2 backend error: cannot showDefineTransition on " ++ show n

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
                  , matchBind invoke0
                      [( parens $ text "mkPair ok0 next0"
                      , matchBind invoke1
                          [( parens $ text "mkPair ok1 next1"
                          , body)] )]])
  $+$ text "(check-sat)"
  $+$ text "(pop)"
  where toplvlIns = vcat [ text "(declare-const inpts0    Inputs)"
                         , text "(declare-const inpts1    Inputs)"
                         , text "(declare-const initState State)" ]
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
  $+$ showDeclareNLDatatype nl inputIds (text "Inputs")
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Defining the State record type specific to the current netlist"
  $+$ showDeclareNLDatatype nl stateIds (text "State")
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Defining the transition function specific to the current netlist"
  $+$ showDefineTransition nl n stateIds fName
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Base case"
  $+$ showBaseCase stateInits fName
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Induction step"
  $+$ showInductionStep fName
  where fName = text $ "t_" ++ nm
        inputIds = [ netInstId n | Just n@Net{netPrim = p} <- elems nl
                                 , case p of Input _ _ -> True
                                             _         -> False ]
        stateElems = [ n | Just n@Net{netPrim = p} <- elems nl
                         , case p of RegisterEn _ _ -> True
                                     Register   _ _ -> True
                                     _              -> False ]
        stateIds = map netInstId stateElems
        stateInits = map stateInit stateElems
        stateInit Net{netPrim=RegisterEn init w} = (init, w)
        stateInit Net{netPrim=Register   init w} = (init, w)
        stateInit _ =
          error $ "SMT2 backend error: encountered non state net " ++ show n ++
                  " where one was expected"
showAll _ n = error $ "SMT2 backend error: cannot showAll on'" ++ show n ++ "'"

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
  -- run the internal topological sort from all relevant root
  mapM_ (topoSort visited sorted) relevantRoots
  -- return the sorted list of InstId
  reverse <$> readSTRef sorted
  -- helpers
  where
    -- identify Netlist's outputs and state holding elements
    relevantRoots = [ netInstId n | Just n@Net{netPrim = p} <- elems nl
                                  , case p of RegisterEn _ _ -> True
                                              Register   _ _ -> True
                                              Output     _ _ -> True
                                              _              -> False ]
    -- identify leaf net
    isLeaf :: Net -> Bool
    isLeaf Net{ netPrim = Input      _ _ } = True
    isLeaf Net{ netPrim = Register   _ _ } = True
    isLeaf Net{ netPrim = RegisterEn _ _ } = True
    isLeaf _                               = False
    -- list insertion helper
    insert lst elem = modifySTRef' lst $ \xs -> elem : xs
    -- the actual recursive topological sort algorithm
    topoSort :: STArray s InstId Mark -> STRef s [InstId] -> InstId
             -> ST s ()
    topoSort visited sorted netId = do
      -- retrieve the 'visited' array entry for the current net
      netVisit <- readArray visited netId
      case netVisit of
        -- For already visited nets, do not do anything
        Permanent -> return ()
        -- For nets under visit, we identified a combinational cycle
        -- (unsupported ==> error out)
        Temporary -> do let net = getNet nl netId
                        error $ "SMT2 backend error: " ++
                                "combinational cycle detected -- " ++
                                show net
        -- For new visits:
        Unmarked -> do
          let net = getNet nl netId
          -- For non leaf nets: mark net as temporarily under visit,
          --                    explore all inputs recursively
          when (not $ isLeaf net) do
            writeArray visited netId Temporary
            let allInputIds = concatMap (map fst . netInputWireIds)
                                        (netInputs net)
            mapM_ (topoSort visited sorted) allInputIds
          -- Mark net as permanent and insert it into the sorted list
          writeArray visited netId Permanent
          insert sorted netId
