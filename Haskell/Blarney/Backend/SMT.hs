{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE NoRebindableSyntax #-}

{-|
Module      : Blarney.Backend.SMT
Description : SMT generation
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

Convert Blarney Netlist to SMT scripts.
-}

module Blarney.Backend.SMT (
  VerifyDepth (..)
, fixedDepth
, VerifyMode (..)
, UserConf (..)
, dfltUserConf
, VerifyConf (..)
, dfltVerifyConf
, genSMTScript
, verifyWithSMT
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
import Blarney.Backend.SMT.Utils
import Blarney.Backend.SMT.NetlistUtils
import Blarney.Backend.SMT.BasicDefinitions
import Blarney.Misc.ANSIEscapeSequences

-- configuration types

-- | 'Bool' synonym for enabling use of restricted states in induction proofs
type UseRestrictedStates = Bool

-- | Type to control depth of verification
data VerifyDepth =
    -- | Span depths between both 'Int' arguments
    Range Int Int
    -- | Span all depths increasingly starting from the 'Int' argument
  | IncreaseFrom Int
  deriving Show

-- | Helper function to construct a 'VerifyDepth' describing a fixed unique
--   depth `n`, i.e. `Range n n`
fixedDepth :: Int -> VerifyDepth
fixedDepth n = Range n n

-- | Return the argument 'VerifyDepth' if it is legal, that is the covered
--   depths are strictly greater than 0, and the higher bound of a 'Range' is
--   greater than its lower bound. Calls 'error' otherwise.
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

-- | Return a unique depth rather than a span of values for the provided
--   'VerifyDepth'.
--   Return the higher bound of a 'Range' and the starting depth for an
--   'IncreaseFrom'.
collapseVerifyDepth :: VerifyDepth -> Int
collapseVerifyDepth (Range _ x) = x
collapseVerifyDepth (IncreaseFrom x) = x

-- | Return a pair of depths from a provided 'VerifyDepth'.
--   If the pair's first element is smaller than its second element, the pair
--   describe a range of depths to cover. Otherwise, the pair describes an
--   unbounded set of depths increasing from its first element.
rangeVerifyDepth :: VerifyDepth -> (Int, Int)
rangeVerifyDepth (Range x y) = (x, y)
rangeVerifyDepth (IncreaseFrom x) = (x, 0)

-- | Type to specify the kind of verification desired
data VerifyMode =
    -- | property holds up to a certain depth
    Bounded VerifyDepth
    -- | property holds by induction
  | Induction VerifyDepth UseRestrictedStates

-- | Configuration type for interaction with a human user
data UserConf = UserConf {
  -- | Ask the user for confirmation or use default answer
  userConfInteractive :: Bool
  -- | For iterative deepening, period at which to ask the user for continuation
, userConfIncreasePeriod :: Int }

-- | Default 'UserConf':
-- @
--    dfltUserConf = UserConf { userConfInteractive    = False
--                            , userConfIncreasePeriod = 4 }
-- @
dfltUserConf = UserConf { userConfInteractive    = False
                        , userConfIncreasePeriod = 4 }

-- | Configuration type for a verification session
data VerifyConf = VerifyConf {
  -- | verification type to use
  verifyConfMode :: VerifyMode
  -- the following fields only have meaning for interaction with an SMT solver
  -- | SMT solver command
, verifyConfSolverCmd :: (String, [String])
  -- | user configuration to use
, verifyConfUser      :: UserConf
  -- | verbosity level for the interactive session
, verifyConfVerbosity :: Int }

-- | Default 'VerifyConf':
-- @
--    dfltVerifyConf = VerifyConf { verifyConfMode      = Bounded (Range 1 1)
--                                , verifyConfSolverCmd = ("z3", ["-in"])
--                                , verifyConfUser      = dfltUserConf
--                                , verifyConfVerbosity = 1 }
-- @
dfltVerifyConf = VerifyConf { verifyConfMode      = Bounded (Range 1 1)
                            , verifyConfSolverCmd = ("z3", ["-in"])
                            , verifyConfUser      = dfltUserConf
                            , verifyConfVerbosity = 1 }

-- | A context netlist and target root net together with extra list of nets of
--   interest to identify inputs, registers, etc...
data Context = Context { ctxtNetlist    :: Netlist
                       , ctxtSortedIds  :: [InstId]
                       , ctxtInputType  :: String
                       , ctxtInputIds   :: [InstId]
                       , ctxtStateType  :: String
                       , ctxtStateIds   :: [InstId]
                       , ctxtStateInits :: [(Integer, InputWidth)]
                       , ctxtRootNet    :: Net
                       , ctxtTFunName   :: String
                       , ctxtCFunName   :: String
                       , ctxtPropName   :: String
                       , ctxtAssertMsg  :: Maybe String }

-- | Make a 'Context' from a 'Netlist' and a 'Net' in that 'Netlist'
mkContext :: Netlist -> Net -> Context
mkContext nl n = Context { ctxtNetlist    = nl
                         , ctxtSortedIds  = topoSortIds
                         , ctxtInputType  = inptType
                         , ctxtInputIds   = inputIds
                         , ctxtStateType  = stType
                         , ctxtStateIds   = stateIds
                         , ctxtStateInits = stateInits
                         , ctxtRootNet    = n
                         , ctxtTFunName   = tFun
                         , ctxtCFunName   = cFun
                         , ctxtPropName   = nm
                         , ctxtAssertMsg  = msg }
  where topoSortIds = partialTopologicalSort nl $ netInstId n
        inputIds = [ i | Just x@Net{netInstId = i, netPrim = p} <- elems nl
                       , case p of Input _ _ -> True
                                   _         -> False
                       , elem i topoSortIds ]
        stateElems =
          [ (i, stateInit x) | Just x@Net{netInstId=i, netPrim=p} <- elems nl
                             , case p of RegisterEn _ _ -> True
                                         Register   _ _ -> True
                                         _              -> False
                             , elem i topoSortIds ]
        stateInit Net{netPrim=RegisterEn init w} = (init, w)
        stateInit Net{netPrim=Register   init w} = (init, w)
        stateInit x = error $ "Blarney.Backend.SMT: non state net " ++ show x ++
                              " encountered where state net was expected"
        (stateIds, stateInits) = unzip stateElems
        inptType = "Input_" ++ nm
        stType = "State_" ++ nm
        tFun = "tFun_" ++ nm
        cFun = "chain_" ++ tFun
        (nm, msg) = case netPrim n of
          Output _      outnm   -> ("output_"++outnm,  Nothing)
          Assert propmsg -> ("assert_"++propnm, Just propmsg)
            where propnm = wireName nl propWire
                  propWire = head . netInputWireIds $ (netInputs n !! 1)
          _ -> error $ "Blarney.Backend.SMT: expected Output or Assert net " ++
                       "but got " ++ show n

-- Internal helpers

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
  $+$ shush (text ("; code gen for " ++ show ctxtPropName))
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
                         ctxtSortedIds
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
          $+$ maybe empty (\msg -> say msg) ctxtAssertMsg
          $+$ say (replicate 80 '-')
          $+$ (if doInduction
                  then     say ("Using induction of depth " ++ show depth)
                       $+$ (if restrictStates
                               then say "Using restricted states"
                               else empty)
                       $+$ say "Base case refutation:"
                  else     say ("Using bounded verification of depth " ++
                                show depth)
                       $+$ say "Bounded property refutation:")
        cmnt1 =     char ';' <> text (replicate 79 '-')
                $+$ say "Inductive case refutation:"
        (depth, doInduction, restrictStates) = case vMode of
          Bounded vD -> ( collapseVerifyDepth . legalizeVerifyDepth $ vD
                        , False, False )
          Induction vD rSt -> ( collapseVerifyDepth . legalizeVerifyDepth $ vD
                              , True, rSt )
        say = parens . (text "echo" <+>) . doubleQuotes . text

-- | Convert given blarney 'Netlist' to an SMT script
genSMTScript :: VerifyConf -- ^ configuration for the verification
              -> Netlist    -- ^ blarney netlist
              -> String     -- ^ script name
              -> String     -- ^ output directory
              -> IO ()
genSMTScript VerifyConf{..} nl nm dir = do
  system ("mkdir -p " ++ dir)
  withFile fileName WriteMode \h -> do
    hPutStr h $ renderStyle rStyle smtCode
  where
    fileName = dir ++ "/" ++ nm ++ ".smt2"
    rStyle = Style PageMode 80 1.05
    rootCtxts = [ mkContext nl n | Just n@Net{netPrim=p} <- elems nl
                                 , case p of Assert _   -> True
                                             _          -> False ]
    smtCode = showGeneralDefs False $+$ vcat defs
    defs = intercalate [text "(echo \"\")"]
                       [ [ showSpecificDefs ctxt False
                         , showSpecificAssert ctxt verifyConfMode ]
                       | ctxt <- rootCtxts ]

-- | Verify given blarney 'Netlist' predicate using an SMT solver
verifyWithSMT :: VerifyConf -- ^ configuration for the verification
               -> Netlist    -- ^ blarney property
               -> IO ()
verifyWithSMT VerifyConf{..} nl =
  withCreateProcess smtP \(Just hIn) (Just hOut) _ _ -> do
    -- set stdin handle to unbuffered for communication with SMT solver
    hSetBuffering hIn LineBuffering
    -- send general SMT sort definitions
    say 2 $ rStr (showGeneralDefs True)
    hPutStrLn hIn $ rStr (showGeneralDefs True)
    -- for each assertion...
    forM_ rootCtxts \ctxt -> do
      say 0 $ "------ verifying property " ++ ctxtPropName ctxt ++ " ------"
      maybe (pure ()) (say 0) (ctxtAssertMsg ctxt)
      say 1 $ "restricted states: " ++ if restrictStates then blue "enabled"
                                                         else yellow "disabled"
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
    rootCtxts = [ mkContext nl n | Just n@Net{netPrim=p} <- elems nl
                                 , case p of Assert _   -> True
                                             _          -> False ]
    smtP = (uncurry proc verifyConfSolverCmd){ std_in  = CreatePipe
                                             , std_out = CreatePipe
                                             , std_err = CreatePipe }
    rStr = renderStyle $ Style OneLineMode 0 0
    say n msg = when (max 0 n <= verifyConfVerbosity) $ putStrLn msg
    justifyLeft n c str = str ++ replicate (n - length str) c
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
           say 1 $ justifyLeft 30 '.'
                     ("(depth " ++ show curD ++ ") bounded ")
                   ++ blue " valid"
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
                     say 1 $ justifyLeft 30 '.'
                               ("(depth " ++ show curD ++ ") induction step ")
                             ++ blue " valid"
                     successVerify
                   | curD == maxD -> failVerify
                   | otherwise -> do
                     say 1 $ justifyLeft 30 '.'
                               ("(depth " ++ show curD ++ ") induction step ")
                             ++ yellow " falsifiable"
                     say 2 "failed to verify induction step ---> deepen"
                     diveMore
              | otherwise -> successVerify
         | curD == maxD -> failVerify
         | otherwise -> do
           say 1 $ justifyLeft 30 '.'
                     ("(depth " ++ show curD ++ ") bounded ")
                   ++ yellow " falsifiable"
           say 2 "failed to verify bounded property ---> deepen"
           diveMore
      where sndLn = hPutStrLn hI
            rcvLn = hGetLine hO
            rcvAll = hGetContents hO
            diveMore =
              if interactive && curD `mod` diveStep == 0 then do
                 askYN "Keep searching for a proof?" False
                       (deepen c (hI, hO) (curD + 1, maxD))
                       failVerify
              else deepen c (hI, hO) (curD + 1, maxD)
            successVerify = say 0 $ " >>> property " ++ ctxtPropName ++ ": "
                                    ++ green "verified"
            failVerify = do
              say 0 $ " >>> property " ++ ctxtPropName ++ ": "
                      ++ red "couldn't verify"
              if interactive then do
                askYN "Show current counter-example?" False
                      (sndLn "(get-model)") (pure ())
                sndLn "(exit)"
                say 0 =<< rcvAll
              else do
                sndLn "(get-model)"
                sndLn "(exit)"
                say 0 =<< rcvAll
                -- XXX TODO deal with parsing the model and exit at the top
                -- XXX currently cannot recover for next assertiong...
            askYN msg dflt actY actN = do
              putStrLn (msg ++ " ...... " ++ yes ++ "/" ++ no)
              answer <- getLine
              if dflt then if (answer /= "n" && answer /= "N") then actY
                                                               else actN
              else if (answer == "y" || answer == "Y") then actY else actN
              where yes = if dflt then "Y" else "y"
                    no = if dflt then "n" else "N"
