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
  VerifyDepth (..)
, fixedDepth
, VerifyMode (..)
, UserConf (..)
, dfltUserConf
, VerifyConf (..)
, dfltVerifyConf
, genSMT2Script
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

type UseRestrictedStates = Bool

data VerifyDepth = Range Int Int | IncreaseFrom Int deriving Show
fixedDepth :: Int -> VerifyDepth
fixedDepth n = Range n n
legalizeVerifyDepth :: VerifyDepth -> VerifyDepth
legalizeVerifyDepth x@(Range lo hi)
  | lo > 0 && lo <= hi = x
  | otherwise = error $ "Malformed VerifyDepth" ++ show x ++ ". " ++
                        "First argument " ++ show lo ++ " must be > 0. " ++
                        "Second argument " ++ show hi ++ " mus be >= " ++
                        show lo ++ "."
legalizeVerifyDepth x@(IncreaseFrom start)
  | start > 0 = x
  | otherwise = error $ "Malformed VerifyDepth" ++ show x ++ ". " ++
                        "Argument " ++ show start ++ " must be > 0. "
maxVerifyDepth :: VerifyDepth -> Int
maxVerifyDepth (Range _ x) = x
maxVerifyDepth (IncreaseFrom x) = x
rangeVerifyDepth :: VerifyDepth -> (Int, Int)
rangeVerifyDepth (Range x y) = (x, y)
rangeVerifyDepth (IncreaseFrom x) = (x, 0)

data VerifyMode = Bounded VerifyDepth
                | Induction VerifyDepth UseRestrictedStates

data UserConf = UserConf { userConfInteractive    :: Bool
                         , userConfIncreasePeriod :: Int }

dfltUserConf = UserConf { userConfInteractive    = False
                        , userConfIncreasePeriod = 4 }

data VerifyConf = VerifyConf { verifyConfMode      :: VerifyMode
                               -- the following fields only have meaning for
                               -- the interaction with an SMT solver
                             , verifyConfSolverCmd :: (String, [String])
                             , verifyConfUser      :: UserConf
                             , verifyConfVerbosity :: Int }

dfltVerifyConf = VerifyConf { verifyConfMode      = Bounded (Range 1 1)
                            , verifyConfSolverCmd = ("z3", ["-in"])
                            , verifyConfUser      = dfltUserConf
                            , verifyConfVerbosity = 1 }

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
        smtCode = showAll nl verifyConfMode False

verifyWithSMT2 :: VerifyConf -> Netlist -> IO ()
verifyWithSMT2 VerifyConf{..} nl =
  withCreateProcess smtP \(Just hIn) (Just hOut) _ _ -> do
    -- set stdin handle to unbuffered for communication with SMT solver
    hSetBuffering hIn LineBuffering
    say 0 $ "---- now verifying property so and so ----"
    say 1 $ "restricted states: " ++ if restrictStates then blue "enabled"
                                                       else yellow "disabled"
    -- send general SMT sort definitions
    say 2 $ rStr (showGeneralDefs True)
    hPutStrLn hIn $ rStr (showGeneralDefs True)
    -- send netlist-specific SMT sort definitions
    say 2 $ rStr (showSpecificDefs ctxt True)
    hPutStrLn hIn $ rStr (showSpecificDefs ctxt True)
    when restrictStates do
      say 2 $ rStr (defineDistinctListX $ ctxtStateType ctxt)
      hPutStrLn hIn $ rStr (defineDistinctListX $ ctxtStateType ctxt)
    -- iterative deepening for potentially increasing depth
    deepen ctxt (hIn, hOut) depthRg
  where
    (depthRg, doInduction, restrictStates) = case verifyConfMode of
      Bounded vD -> ( rangeVerifyDepth . legalizeVerifyDepth $ vD
                    , False, False )
      Induction vD rSt -> ( rangeVerifyDepth . legalizeVerifyDepth $ vD
                          , True, rSt )
    interactive = userConfInteractive verifyConfUser
    diveStep = userConfIncreasePeriod verifyConfUser
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
      -- send assertion for the bounded proof of current depth
      let bounded = assertBounded ctxtCFunName
                                  (ctxtInputType, ctxtStateType)
                                  ctxtStateInits
                                  curD
      say 2 $ rStr bounded
      sndLn "(push)"
      sndLn $ rStr bounded
      sndLn "(check-sat)"
      ln <- rcvLn
      sndLn "(pop)"
      say 2 $ "check-sat: " ++ ln
      if | ln == "unsat" -> do
           say 1 $ "bounded at depth " ++ show curD ++ "... " ++ blue "valid"
           if | doInduction -> do
                let step = assertInduction ctxtCFunName
                                           (ctxtInputType, ctxtStateType)
                                           curD
                                           restrictStates
                say 2 $ rStr step
                sndLn "(push)"
                sndLn $ rStr step
                sndLn "(check-sat)"
                ln <- rcvLn
                sndLn "(pop)"
                say 2 $ "check-sat: " ++ ln
                if | ln == "unsat" -> do
                     say 1 $ "induction step at depth " ++ show curD ++ "... " ++ blue "valid"
                     successVerify
                   | curD == maxD -> failVerify
                   | otherwise -> do
                     say 1 $ "induction step at depth " ++ show curD ++ "... " ++ yellow "falsifiable"
                     say 2 "failed to verify induction step ---> deepen"
                     diveMore
              | otherwise -> successVerify
         | curD == maxD -> failVerify
         | otherwise -> do
           say 1 $ "bounded at depth " ++ show curD ++ "... " ++ yellow "falsifiable"
           say 2 "failed to verify bounded property ---> deepen"
           diveMore
      where sndLn = hPutStrLn hI
            rcvLn = hGetLine hO
            rcvAll = hGetContents hO
            diveMore =
              if interactive && curD `mod` diveStep == 0 then do
                 askYN "Keep searching for a proof?" False do
                   deepen c (hI, hO) (curD + 1, maxD)
              else deepen c (hI, hO) (curD + 1, maxD)
            successVerify = say 0 $ "property so and so: " ++ green "verified"
            failVerify = do
              say 0 $ "property so and so: " ++ red "couldn't verify"
              if interactive then do
                askYN "Show current counter-example?" False do
                  sndLn "(get-model)"
                sndLn "(exit)"
                say 0 =<< rcvAll
              else do
                sndLn "(get-model)"
                sndLn "(exit)"
                say 0 =<< rcvAll
            askYN msg dflt act = do
              putStrLn (msg ++ " ...... " ++ yes ++ "/" ++ no)
              answer <- getLine
              if dflt then when (answer /= "n" && answer /= "N") act
              else when (answer == "y" || answer == "Y") act
              where yes = if dflt then "Y" else "y"
                    no = if dflt then "n" else "N"

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
  $+$ declareNLDatatype ctxtNetlist ctxtInputIds ctxtInputType
  $+$ shush (char ';' <> text (replicate 79 '-'))
  $+$ shush (text "; Defining the specific State record type")
  $+$ declareNLDatatype ctxtNetlist ctxtStateIds ctxtStateType
  $+$ shush (char ';' <> text (replicate 79 '-'))
  $+$ shush (text "; Defining the specific transition function")
  $+$ defineNLTransition ctxtNetlist ctxtRootNet
                         (ctxtInputType, ctxtStateType)
                         ctxtStateIds
                         ctxtTFunName
  $+$ shush (char ';' <> text (replicate 79 '-'))
  $+$ shush (text "; Defining the specific chaining of the transition function")
  $+$ defineChainTransition ctxtTFunName
                            (ctxtInputType, ctxtStateType)
                            ctxtCFunName
  where shush doc = if quiet then empty else doc

showSpecificAssert :: Context -> VerifyMode -> Doc
showSpecificAssert ctxt@Context{..} vMode =
  check test0 $+$ if doInduction then check test1 else empty
  where check x = text "(push)" $+$ x $+$ text "(check-sat)" $+$ text "(pop)"
        test0 = cmnt0 $+$ assertBounded ctxtCFunName
                                        (ctxtInputType, ctxtStateType)
                                        ctxtStateInits
                                        depth
        test1 = (if restrictStates then rStDefs else empty) $+$ cmnt1
                $+$ assertInduction ctxtCFunName
                                    (ctxtInputType, ctxtStateType)
                                    depth
                                    restrictStates
        rStDefs =     text "; Defining helpers for restricted state checking"
                  $+$ defineDistinctListX ctxtStateType
        cmnt0 =
              char ';' <> text (replicate 79 '-')
          $+$ (if doInduction then
                  text "; Proof by induction, induction depth =" <+> int depth
               else text "; Bounded proof for depth =" <+> int depth)
          $+$ maybe empty (\msg -> text $ "(echo \"" ++ msg ++ "\")")
                          ctxtAssertMsg
          $+$ text ("(echo \"" ++ replicate 80 '-' ++ "\")")
        cmnt1 = char ';' <> text (replicate 79 '-')
        (depth, doInduction, restrictStates) = case vMode of
          Bounded vD -> ( maxVerifyDepth . legalizeVerifyDepth $ vD
                        , False, False )
          Induction vD rSt -> ( maxVerifyDepth . legalizeVerifyDepth $ vD
                              , True, rSt )

showAll :: Netlist -> VerifyMode -> Bool -> Doc
showAll nl verifyMode quiet =
  -- general definitions
      showGeneralDefs quiet
  -- definitions specific to the netlist
  $+$ vcat [ showSpecificDefs ctxt quiet | ctxt <- rootCtxts ]
  -- show the transition function and the induction proof for each netlist root.
  $+$ vcat [ showSpecificAssert ctxt verifyMode | ctxt <- rootCtxts ]
  where rootCtxts = [ mkContext nl n | Just n@Net{netPrim=p} <- elems nl
                                     , case p of Output _ _ -> True
                                                 Assert _   -> True
                                                 _          -> False ]
---

data Context = Context { ctxtNetlist    :: Netlist
                       , ctxtInputType  :: String
                       , ctxtInputIds   :: [InstId]
                       , ctxtStateType  :: String
                       , ctxtStateIds   :: [InstId]
                       , ctxtStateInits :: [(Integer, InputWidth)]
                       , ctxtRootNet    :: Net
                       , ctxtTFunName   :: String
                       , ctxtCFunName   :: String
                       , ctxtAssertMsg  :: Maybe String }

mkContext :: Netlist -> Net -> Context
mkContext nl n = Context { ctxtNetlist    = nl
                         , ctxtInputType  = inptType
                         , ctxtInputIds   = inputIds
                         , ctxtStateType  = stType
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
        inptType = "Input_" ++ nm
        stType = "State_" ++ nm
        tFun = "tFun_" ++ nm
        cFun = "chain_" ++ tFun
        (nm, msg) = case netPrim n of
          Output _      outnm   -> ("output_"++outnm,  Nothing)
          Assert propmsg -> ("assert_"++propnm, Just propmsg)
            where propnm = wireName nl propWire
                  propWire = head . netInputWireIds $ (netInputs n !! 1)
          _ -> error $ "SMT2 backend error: expected Output or Assert net " ++
                       "but got " ++ show n

------

