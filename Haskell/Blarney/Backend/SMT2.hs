{-# LANGUAGE RecordWildCards    #-}
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
import System.IO
import Data.Maybe
import Control.Monad
import Text.PrettyPrint
import Data.Array.IArray
import Prelude hiding ((<>))
import System.Process (system)

-- Blarney imports
import Blarney.Netlist
import Blarney.Backend.SMT2.Utils
import Blarney.Backend.SMT2.NetlistUtils
import Blarney.Backend.SMT2.BasicDefinitions

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
     hPutStr h (renderStyle rStyle $ showAll nl)
     hClose h
  where fileName = dir ++ "/" ++ nm ++ ".smt2"
        rStyle = Style PageMode 80 1.05

-- internal helpers specific to blarney netlists
--------------------------------------------------------------------------------
showGeneralDefs :: Doc
showGeneralDefs =
      char ';' <> text (replicate 79 '-')
  $+$ text "; Defining Tuple sorts"
  $+$ declareTupleTypes [2, 3]
  $+$ text "; Defining a generic ListX sort"
  $+$ declareListXType
  $+$ text "; Defining an \"and\" reduction function for ListX Bool"
  $+$ defineAndReduce
  $+$ text "; Defining an \"implies\" reduction function for ListX Bool"
  $+$ defineImpliesReduce

showBaseCase :: String -> [(Integer, InputWidth)] -> Int -> Doc
showBaseCase cFun initS depth =
      text "(push)" $+$ decls $+$ assertion
  $+$ text (   "(echo \"Base case for property " ++ cFun
            ++ ", induction depth " ++ show depth ++ "\")")
  $+$ text "(check-sat)" $+$ text "(pop)"
  where inpts = [ "in" ++ show i | i <- [0 .. depth-1] ]
        decls = vcat $ map (\i -> text $ "(declare-const " ++ i ++ " Inputs)")
                           inpts
        assertion = applyOp (text "assert") [ letBind bindArgs matchInvoke ]
        bindArgs = [ (text "inpts", mkListX inpts "Inputs")
                   , (text "initS", createState initS) ]
        createState [] = text "mkState"
        createState xs = parens $   text "mkState"
                                <+> sep (map (\(v, w) -> int2bv w v) xs)
        matchInvoke = matchBind (applyOp (text cFun)
                                         [text "inpts", text "initS"])
                                [( text "(mkTuple2 oks ss)"
                                 , applyOp (text "not")
                                           [applyOp (text "andReduce")
                                                    [text "oks"]] )]

showInductionStep :: String -> Int -> Bool -> Doc
showInductionStep cFun depth restrict =
      text "(push)" $+$ decls $+$ assertion
  $+$ text (   "(echo \"Induction step for property " ++ cFun
            ++ ", induction depth " ++ show depth
            ++ if restrict then " with restricted states\")" else "\")")
  $+$ text "(check-sat)" $+$ text "(pop)"
  where inpts = [ "in" ++ show i | i <- [0 .. depth] ]
        decls = vcat $ (text "(declare-const startS State)") :
                       map (\i -> text $ "(declare-const " ++ i ++ " Inputs)")
                           inpts
        assertion = applyOp (text "assert") [ letBind bindArgs matchInvoke ]
        bindArgs = [ (text "inpts", mkListX inpts "Inputs") ]
        matchInvoke = matchBind (applyOp (text cFun)
                                         [text "inpts", text "startS"])
                                [( text "(mkTuple2 oks ss)"
                                 , applyOp (text "not") [propHolds] )]
        propHolds =
          applyOp (text "impliesReduce")
                  if not restrict then [ text "oks" ]
                  else [ applyOp (text "cons")
                                 [ applyOp (text "allDifferent_ListX_State")
                                           [applyOp (text "init_ListX_State")
                                                    [text "ss"]]
                                 , text "oks" ]]

data Context = Context { ctxtNetlist    :: Netlist
                       , ctxtInputIds   :: [InstId]
                       , ctxtStateIds   :: [InstId]
                       , ctxtStateInits :: [(Integer, InputWidth)]
                       , ctxtRootNet    :: Net
                       , ctxtTFunName   :: String
                       , ctxtCFunName   :: String
                       , ctxtAssertMsg  :: Maybe String }

showSpecificDefs :: Context -> Doc
showSpecificDefs Context{..} =
      char ';' <> text (replicate 79 '-')
  $+$ text ("; code gen for net" ++ show ctxtRootNet)
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text ("(push)")
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Defining the specific Inputs record type"
  $+$ declareNLDatatype ctxtNetlist ctxtInputIds "Inputs"
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Defining the specific State record type"
  $+$ declareNLDatatype ctxtNetlist ctxtStateIds "State"
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Defining the specific transition function"
  $+$ defineNLTransition ctxtNetlist ctxtRootNet ctxtStateIds ctxtTFunName
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Defining the specific chaining of the transition function"
  $+$ defineChainTransition ctxtTFunName ctxtCFunName

showSpecificAssert :: Context -> Int -> Bool -> Doc
showSpecificAssert ctxt@Context{..} depth restrictStates =
      showSpecificDefs ctxt
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Proof by induction, induction depth =" <+> int depth
  $+$ maybe empty (\msg -> text $ "(echo \"" ++ msg ++ "\")") ctxtAssertMsg
  $+$ text ("(echo \"" ++ replicate 80 '-' ++ "\")")
  $+$ char ';' <> text (replicate 79 '-')
  $+$ text "; Base case"
  $+$ showBaseCase ctxtCFunName ctxtStateInits depth
  $+$ char ';' <> text (replicate 79 '-')
  $+$ (if restrictStates then
             text "; Defining helpers for restricted state checking"
         $+$ defineDistinctListX "State"
       else empty)
  $+$ text "; Induction step"
  $+$ showInductionStep ctxtCFunName depth restrictStates
  $+$ text ("(echo \"" ++ replicate 80 '-' ++ "\")")
  $+$ text ("(pop)")

showAll :: Netlist -> Doc
showAll nl =
  -- general definitions
      showGeneralDefs
  -- show the transition function and the induction proof for each netlist root.
  -- XXX TODO sort out showing transitions functions for each "root" and proofs
  -- only for assert nets
  $+$ vcat [ showSpecificAssert (mkCtxt x) depth restrictStates | x <- roots ]
  where depth = 8 -- MUST BE AT LEAST 1
        restrictStates = True
        roots = [ n | Just n@Net{netPrim=p} <- elems nl
                    , case p of Output _ _ -> True
                                Assert _ _ -> True
                                _          -> False ]
        mkCtxt n =
          Context { ctxtNetlist    = nl
                  , ctxtInputIds   = inputIds
                  , ctxtStateIds   = stateIds
                  , ctxtStateInits = stateInits
                  , ctxtRootNet    = n
                  , ctxtTFunName   = tFun
                  , ctxtCFunName   = cFun
                  , ctxtAssertMsg  = msg } where (tFun, cFun, msg) = rootStrs n
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
        stateInit n =
          error $ "SMT2 backend error: non state net " ++ show n ++
                  " encountered where state net was expected"
        rootStrs n = (tFun, cFun, msg) where tFun = "tFun_" ++ nm
                                             cFun = "chain_" ++ tFun
                                             (nm, msg) = primStrs n
        primStrs n = case netPrim n of
          Output _      outnm   -> ("output_"++outnm,  Nothing)
          Assert propnm propmsg -> ("assert_"++propnm, Just propmsg)
          _ -> error $ "SMT2 backend error: expected Output or Assert net " ++
                       "but got " ++ show n
