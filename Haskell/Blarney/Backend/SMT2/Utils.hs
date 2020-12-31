{-|
Module      : Blarney.Backend.SMT2.Utils
Description : Basic utility functions for SMT2 pretty printing
Copyright   : (c) Alexandre Joannou, 2020
License     : MIT
Stability   : experimental

This module provides basic pretty printing utilities for SMT2 code generation
-}

module Blarney.Backend.SMT2.Utils (
-- * general numerical helpers
  showAtBase
, showHex
, showBin
-- * formatting helpers
, psep
, plist
, applyOp
-- SMT2 BitVec helpers
, bvHexLit
, bvBinLit
, bvSlice
, bvSortStr
, bvSort
, int2bv
, bvIsFalse
, bvIsTrue
, bv2Bool
, bool2bv
-- * SMT2 binders and qualifiers
, qualify
, parBind
, forallBind
, existsBind
, matchBind
, letBind
, nestLetBind
-- * SMT2 datatype declarations
, declareDataTypes
, declareDataType
-- * SMT2 function definitions
, defineFun
, defineFunRec
, defineFunsRec
) where

-- Standard imports
import Text.PrettyPrint
import Prelude hiding ((<>))
import Data.Char (intToDigit)
import Numeric (showIntAtBase)

-- | pretty print an 'Integral' in a given base
showAtBase :: (Integral a, Show a) => a -> a -> Doc
showAtBase b n = text $ showIntAtBase b intToDigit n ""

-- | pretty print an 'Integral' in hexadecimal
showHex :: (Integral a, Show a) => a -> Doc
showHex n = showAtBase 16 n

-- | pretty print an 'Integral' in binary
showBin :: (Integral a, Show a) => a -> Doc
showBin n = showAtBase 2 n

-- | pretty print an 'Integer' as an SMT2 BitVec hexadecimal literal
bvHexLit :: Integer -> Doc
bvHexLit n = text "#x" <> showHex n

-- | pretty print an 'Integer' as an SMT2 BitVec binary literal
bvBinLit :: Integer -> Doc
bvBinLit n = text "#b" <> showBin n

-- | extract the bitslice from 'lo' to 'hi' from the 'bv' SMT2 BitVec
bvSlice :: Int-> Int-> Doc -> Doc
bvSlice hi lo bv =
  parens $ (parens $ char '_' <+> text "extract" <+> int hi <+> int lo) <+> bv

-- | return a string representing the BitVec SMT2 sort of the given width 'n'
bvSortStr :: Int -> String
bvSortStr n = "(_ BitVec " ++ show n ++ ")"

-- | pretty print the BitVec SMT2 sort of the given width 'n'
bvSort :: Int -> Doc
bvSort = text . bvSortStr

-- | cast the Int sorted SMT2 value 'n' to a (BitVec 'w') sorted SMT2 value
int2bv :: Int -> Integer -> Doc
int2bv w n =
  parens $ parens (char '_' <+> text "int2bv" <+> int w) <+> int (fromInteger n)

-- | test a (BitVec 1) sorted SMT2 value for "false"
bvIsFalse :: Doc -> Doc
bvIsFalse doc = parens $ text "=" <+> doc <+> text "#b0"

-- | test a (BitVec 1) sorted SMT2 value for "true"
bvIsTrue :: Doc -> Doc
bvIsTrue doc = parens $ text "=" <+> doc <+> text "#b1"

-- | cast a (BitVec 1) sorted SMT2 value to a Bool sorted SMT2 value
bv2Bool :: Doc -> Doc
bv2Bool = bvIsTrue

-- | cast a Bool sorted SMT2 value to a (BitVec 1) sorted SMT2 value
bool2bv :: Doc -> Doc
bool2bv doc = parens $ text "ite" <+> doc <+> text "#b1" <+> text "#b0"

-- | pretty print a whitespace separated list of 'Doc' in parentheses
psep :: [Doc] -> Doc
psep = parens . sep

-- | pretty print a whitespace separated list of parenthesised pairs of 'Doc's
--   in parentheses
plist :: [(Doc, Doc)] -> Doc
plist = psep . map (\(v, s) -> psep [v, s])

-- | pretty print the application of an operator to a list of arguments
applyOp :: Doc -> [Doc] -> Doc
applyOp opName opArgs = psep $ opName : opArgs

-- | wrap the SMT2 expression 'expr' in the "as" qualifier with the given 'sort'
qualify :: Doc -> Doc -> Doc
qualify expr sort = applyOp (text "as") [ expr, sort ]

-- | wrap the SMT2 parametric expression 'expr' in the "par" sort parameter
--   binder with the 'pars' list of sort parameters
parBind :: [String] -> Doc -> Doc
parBind [] expr = expr
parBind pars expr = applyOp (text "par") [parens (sep $ text <$> pars), expr]

-- | internal helper to define binders for same level bindings
flatBind :: Doc -> [(Doc, Doc)] -> Doc -> Doc
flatBind _ [] doc = doc
--flatBind binder xs doc = applyOp binder [ plist xs, doc ]
flatBind binder xs doc = parens $ binder <+> sep [ plist xs, doc ]

-- | wrap an SMT2 expression in the "forall" universal quantifier with the
--   provided list of pairs of (variable name, sort)
forallBind :: [(Doc, Doc)] -> Doc -> Doc
forallBind = flatBind $ text "forall"

-- | wrap an SMT2 expression in the "exists" existential quantifier with the
--   provided list of pairs of (variable name, sort)
existsBind :: [(Doc, Doc)] -> Doc -> Doc
existsBind = flatBind $ text "exists"

-- | wrap an SMT2 expression in the "match" binder, pattern matching the SMT2
--   expression against each of the patterns provided in the argument list of
--   pairs of (pattern, expression)
matchBind :: Doc -> [(Doc, Doc)] -> Doc
matchBind doc [] = doc
matchBind doc matches = applyOp (text "match") [ doc, plist matches ]

-- | wrap an SMT2 expression in the "let" binder with the provided list of pairs
--   of (variable name, sort)
letBind :: [(Doc, Doc)] -> Doc -> Doc
letBind = flatBind $ text "let"

-- | wrap an SMT2 expression in a sequence of nested "let" binders, each
--   introducing one of the variables provided by the argument list of pairs of
--   (variable name, sort), in the provided order
nestLetBind :: [[(Doc, Doc)]] -> Doc -> Doc
nestLetBind [] doc = doc
--nestLetBind (xs:xss) doc = letBind xs (nestLetBind xss doc)
-- the 5 is the length of "let " + 1
nestLetBind (xs:xss) doc = letBind xs (nest (-5) (nestLetBind xss doc))

-- | pretty print the declaration of the datatypes provided in the argument list
--   of triples each representing a single datatype:
--   its name, a potentially empty list of parameters, and a non empty list of
--   constructors consisting of a name and a potentially empty list of fields
declareDataTypes :: [(String, [String], [(String, [String])])] -> Doc
declareDataTypes dts =
  applyOp (text "declare-datatypes") [ plist dtNames, psep dtConss ]
  where (dtNames, dtConss) = unzip $ fmt <$> dts
        fmt (nm, ps, conss) = ( (text nm, int $ length ps)
                              , (parBind ps) . psep $ fmtCons <$> conss)
        fmtCons (cnm, flds) = psep $ (text cnm) : ((parens . text) <$> flds)

-- | pretty print the declaration of a single datatype given its name, a
--   potentially empty list of parameters, and a non empty list of constructors
--   consisting of a name and a potentially empty list of fields
declareDataType :: String -> [String] -> [(String, [(String, String)])] -> Doc
declareDataType dtName dtParams dtConsAndFields =
  applyOp (text "declare-datatype") [ text dtName, dtDef ]
  where dtDef = parBind dtParams dtCons
        dtCons = psep . (map dtOneCons) $ dtConsAndFields
        dtOneCons (cN, cFs) = psep $ text cN : (map dtField cFs)
        dtField (v, s) = parens $ text v <+> text s

-- | pretty print the definition of a function given its name, a potentially
--   empty list of arguments, a return type and a function body
defineFun :: Doc -> [(Doc, Doc)] -> Doc -> Doc -> Doc
defineFun fName fArgs fRet fBody =
  applyOp (text "define-fun") [ fName, plist fArgs, fRet, fBody ]

-- | pretty print the definition of a recursive function given its name, a
--   potentially empty list of arguments, a return type and a function body
defineFunRec :: Doc -> [(Doc, Doc)] -> Doc -> Doc -> Doc
defineFunRec fName fArgs fRet fBody =
  applyOp (text "define-fun-rec") [ fName, plist fArgs, fRet, fBody ]

-- | pretty print the definition of the potentially mutually recursive functions
--   provided in the argument list of triples each representing a single
--   potentially recursive function:
--   its name, a potentially empty list of arguments, a return type and a
--   function body
defineFunsRec :: [(String, [(String, String)], String, Doc)] -> Doc
defineFunsRec fs = applyOp (text "define-funs-rec") [ psep fDecls, psep fDefs ]
  where (fDecls, fDefs) = unzip $ fmt <$> fs
        fmt (nm, as, r, body) = ( psep [ text nm
                                       , plist [(text a, text b) | (a,b) <- as]
                                       , text r ]
                                , body )
