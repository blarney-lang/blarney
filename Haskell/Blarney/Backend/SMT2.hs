{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE NoRebindableSyntax #-}

{-|
Module      : Blarney.Backend.SMT2
Description : SMT2 generation
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

Convert Blarney Netlist to SMT2 scripts.
-}

module Blarney.Backend.SMT2 (
  genSMT2Script
, VerifyConf (..)
, dfltVerifyConf
, verifyWithSMT2
) where

-- Standard imports
import System.IO
import Data.List
import Data.Maybe
import Control.Monad
import System.Process
import Text.PrettyPrint
import Data.Array.IArray
import Prelude hiding ((<>))

-- Blarney imports
import Blarney.Netlist
import Blarney.Backend.SMT2.Utils
import Blarney.Backend.SMT2.NetlistUtils
import Blarney.Backend.SMT2.BasicDefinitions
import Blarney.Misc.ANSIEscapeSequences

-- Toplevel API
--------------------------------------------------------------------------------

data VerifyConf = VerifyConf { verifyConfDepth          :: (Int, Int) -- cur depth, max depth. max depth 0 means incremental search
                             , verifyConfRestrictStates :: Bool
                             , verifyConfSolverCmd      :: (String, [String])
                             , verifyConfVerbosity      :: Int }
dfltVerifyConf :: VerifyConf
dfltVerifyConf = VerifyConf { verifyConfDepth = (1,1)
                            , verifyConfRestrictStates = False
                            , verifyConfSolverCmd = ("z3", ["-in"])
                            , verifyConfVerbosity = 1 }
legalDepth :: (Int, Int) -> (Int, Int)
legalDepth (curD, maxD) = (max 1 curD, maxD)

-- | Convert given blarney 'Netlist' to an SMT2 script
genSMT2Script :: VerifyConf
              -> Netlist -- ^ blarney netlist
              -> String  -- ^ script name
              -> String  -- ^ output directory
              -> IO ()
genSMT2Script VerifyConf{..} nl nm dir =
  do system ("mkdir -p " ++ dir)
     withFile fileName WriteMode \h -> do
       hPutStr h $ renderStyle rStyle smtCode
  where fileName = dir ++ "/" ++ nm ++ ".smt2"
        rStyle = Style PageMode 80 1.05
        depth = snd . legalDepth $ verifyConfDepth
        smtCode = showAll nl depth verifyConfRestrictStates False

verifyWithSMT2 :: VerifyConf -> Netlist -> IO ()
verifyWithSMT2 VerifyConf{..} nl =
  withCreateProcess smtP \(Just hIn) (Just hOut) _ _ -> do
    -- set stdin handle to unbuffered for communication with SMT solver
    hSetBuffering hIn LineBuffering
    say 0 $ "---- now verifying property so and so ----"
    say 1 $ "restricted states: " ++
            if verifyConfRestrictStates then blue "enabled"
                                        else yellow "disabled"
    -- send general SMT sort definitions
    say 2 $ rStr (showGeneralDefs True)
    hPutStrLn hIn $ rStr (showGeneralDefs True)
    -- send netlist-specific SMT sort definitions
    say 2 $ rStr (showSpecificDefs ctxt True)
    hPutStrLn hIn $ rStr (showSpecificDefs ctxt True)
    when verifyConfRestrictStates do
      say 2 $ rStr (defineDistinctListX "State")
      hPutStrLn hIn $ rStr (defineDistinctListX "State")
    -- iterative depening for induction proof
    deepen ctxt (hIn, hOut) (legalDepth verifyConfDepth)
  where
    say n msg = when (max 0 n <= verifyConfVerbosity) $ putStrLn msg
    smtP = (uncurry proc verifyConfSolverCmd){ std_in  = CreatePipe
                                             , std_out = CreatePipe
                                             , std_err = CreatePipe }
    rStr = renderStyle $ Style OneLineMode 0 0
    root = head [ n | Just n@Net{netPrim=p} <- elems nl
                    , case p of Output _ _ -> True
                                _          -> False ]
    ctxt = mkContext nl root
    deepen c@Context{..} (hI, hO) (curD, maxD) = do
      say 2 $ "=================> depth " ++ show curD
      --
      let base = assertInductionBase ctxtCFunName ctxtStateInits curD
      let step = assertInductionStep ctxtCFunName curD verifyConfRestrictStates
      sndLn "(push)"
      say 2 $ rStr base
      sndLn $ rStr base
      sndLn "(check-sat)"
      ln <- rcvLn
      say 2 $ "check-sat of base: " ++ ln
      if | ln == "unsat" -> do
           say 1 $ "base " ++ show curD ++ "... " ++ blue "valid"
           sndLn "(pop)"
           sndLn "(push)"
           --
           say 2 $ rStr step
           sndLn $ rStr step
           sndLn "(check-sat)"
           ln <- rcvLn
           say 2 $ "check-sat of step: " ++ ln
           if | ln == "unsat" -> do
                say 1 $ "step " ++ show curD ++ "... " ++ blue "valid"
                sndLn "(pop)"
                say 0 $ "property so and so: " ++ green "verified"
              | curD == maxD -> failVerify
              | otherwise -> do
                sndLn "(pop)"
                say 1 $ "step " ++ show curD ++ "... " ++ yellow "falsifiable"
                say 2 "failed to verify induction step ---> deepen"
                deepen c (hI, hO) (curD + 1, maxD)
         | curD == maxD -> failVerify
         | otherwise -> do
           sndLn "(pop)"
           say 1 $ "base " ++ show curD ++ "... " ++ yellow "falsifiable"
           say 2 "failed to verify base case ---> deepen"
           deepen c (hI, hO) (curD + 1, maxD)
      where sndLn = hPutStrLn hI
            rcvLn = hGetLine hO
            rcvAll = hGetContents hO
            failVerify = do say 0 $ "property so and so: " ++
                                    red "couldn't verify"
                            say 0 "Show current counter-example? y/N"
                            ln <- getLine
                            when (ln == "y" || ln == "Y") $ sndLn "(get-model)"
                            sndLn "(exit)"
                            say 0 =<< rcvAll

-- internal helpers specific to blarney netlists
--------------------------------------------------------------------------------
showGeneralDefs :: Bool -> Doc
showGeneralDefs quiet =
      shush (char ';' <> text (replicate 79 '-'))
  $+$ shush (text "; Defining Tuple sorts")
  $+$ declareTupleTypes [2, 3]
  $+$ shush (text "; Defining a generic ListX sort")
  $+$ declareListXType
  $+$ shush (text "; Defining an \"and\" reduction function for ListX Bool")
  $+$ defineAndReduce
  $+$ shush (text "; Defining an \"implies\" reduction function for ListX Bool")
  $+$ defineImpliesReduce
  where shush doc = if quiet then empty else doc

showSpecificDefs :: Context -> Bool -> Doc
showSpecificDefs Context{..} quiet =
      shush (char ';' <> text (replicate 79 '-'))
  $+$ shush (text ("; code gen for net" ++ show ctxtRootNet))
  $+$ shush (char ';' <> text (replicate 79 '-'))
  $+$ text ("(push)")
  $+$ shush (char ';' <> text (replicate 79 '-'))
  $+$ shush (text "; Defining the specific Inputs record type")
  $+$ declareNLDatatype ctxtNetlist ctxtInputIds "Inputs"
  $+$ shush (char ';' <> text (replicate 79 '-'))
  $+$ shush (text "; Defining the specific State record type")
  $+$ declareNLDatatype ctxtNetlist ctxtStateIds "State"
  $+$ shush (char ';' <> text (replicate 79 '-'))
  $+$ shush (text "; Defining the specific transition function")
  $+$ defineNLTransition ctxtNetlist ctxtRootNet ctxtStateIds ctxtTFunName
  $+$ shush (char ';' <> text (replicate 79 '-'))
  $+$ shush (text "; Defining the specific chaining of the transition function")
  $+$ defineChainTransition ctxtTFunName ctxtCFunName
  where shush doc = if quiet then empty else doc

showSpecificAssert :: Context -> Int -> Bool -> Bool -> Doc
showSpecificAssert ctxt@Context{..} depth restrictStates quiet =
      showSpecificDefs ctxt quiet
  $+$ shush (char ';' <> text (replicate 79 '-'))
  $+$ shush (text "; Proof by induction, induction depth =" <+> int depth)
  $+$ shush (maybe empty (\msg -> text $ "(echo \"" ++ msg ++ "\")")
                         ctxtAssertMsg)
  $+$ shush (text ("(echo \"" ++ replicate 80 '-' ++ "\")"))
  $+$ shush (char ';' <> text (replicate 79 '-'))
  $+$ shush (text "; Base case")
  $+$ checkInductionBase ctxtCFunName ctxtStateInits depth
  $+$ shush (char ';' <> text (replicate 79 '-'))
  $+$ (if restrictStates then
             shush (text "; Defining helpers for restricted state checking")
         $+$ defineDistinctListX "State"
       else empty)
  $+$ shush (text "; Induction step")
  $+$ checkInductionStep ctxtCFunName depth restrictStates
  $+$ shush (text ("(echo \"" ++ replicate 80 '-' ++ "\")"))
  $+$ text ("(pop)")
  where shush doc = if quiet then empty else doc

showAll :: Netlist -> Int -> Bool -> Bool -> Doc
showAll nl depth restrictStates quiet =
  -- general definitions
      showGeneralDefs quiet
  -- show the transition function and the induction proof for each netlist root.
  -- XXX TODO sort out showing transitions functions for each "root" and proofs
  -- only for assert nets
  $+$ vcat [ showSpecificAssert (mkContext nl n) depth restrictStates quiet
           | n <- roots ]
  where roots = [ n | Just n@Net{netPrim=p} <- elems nl
                    , case p of Output _ _ -> True
                                Assert _ _ -> True
                                _          -> False ]
---

data Context = Context { ctxtNetlist    :: Netlist
                       , ctxtInputIds   :: [InstId]
                       , ctxtStateIds   :: [InstId]
                       , ctxtStateInits :: [(Integer, InputWidth)]
                       , ctxtRootNet    :: Net
                       , ctxtTFunName   :: String
                       , ctxtCFunName   :: String
                       , ctxtAssertMsg  :: Maybe String }

mkContext :: Netlist -> Net -> Context
mkContext nl n = Context { ctxtNetlist    = nl
                         , ctxtInputIds   = inputIds
                         , ctxtStateIds   = stateIds
                         , ctxtStateInits = stateInits
                         , ctxtRootNet    = n
                         , ctxtTFunName   = tFun
                         , ctxtCFunName   = cFun
                         , ctxtAssertMsg  = msg }
  where inputIds = [ netInstId n | Just n@Net{netPrim = p} <- elems nl
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
        stateInit n = error $ "SMT2 backend error: non state net " ++ show n ++
                              " encountered where state net was expected"
        tFun = "tFun_" ++ nm
        cFun = "chain_" ++ tFun
        (nm, msg) = case netPrim n of
          Output _      outnm   -> ("output_"++outnm,  Nothing)
          Assert propnm propmsg -> ("assert_"++propnm, Just propmsg)
          _ -> error $ "SMT2 backend error: expected Output or Assert net " ++
                       "but got " ++ show n

------

