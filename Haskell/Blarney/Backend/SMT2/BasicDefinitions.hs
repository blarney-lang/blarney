{-|
Module      : Blarney.Backend.SMT2.BasicDefinitions
Description : Basic SMT2 helper types and functions definitions
Copyright   : (c) Alexandre Joannou, 2020-2021
License     : MIT
Stability   : experimental

This module provides basic SMT2 types and functions to assist in blarney Netlist
code generation
-}

module Blarney.Backend.SMT2.BasicDefinitions (
-- * datatypes declaration
  declareTupleTypes
, declareListXType
-- * list creation
, mkListX
-- * list reduction
, defineAndReduce
, defineImpliesReduce
, defineDistinctListX
-- * miscellaneous
, defineChain
) where

-- Standard imports
import Data.List
import Text.PrettyPrint
import Prelude hiding ((<>))

-- Blarney imports
import Blarney.Backend.SMT2.Utils

-- | Declare an SMT2 tuple sort per entry in the argument list. Each sort is
--   defined as a function of the 'Int' "N", as the parametric sort "TupleN"
--   parameterised on "X", and a constructor "mkTupleN" with N fields named
--   "tplN_i" for i ranging from 1 to N
declareTupleTypes :: [Int] -> Doc
declareTupleTypes ns = declareDataTypes $ decl <$> nub ns
  where decl n = ( "Tuple" ++ show n, (("X" ++) . show) <$> [1 .. n]
                 , [( "mkTuple" ++ show n
                    , fld n <$> [1 .. n] )] )
        fld n i = "tpl" ++ show n ++ "_" ++ show i ++ " X" ++ show i

-- | Declare a "ListX" parametric Sort parameterised on "X" with 2 constructors:
--   * a "nil" constructor
--   * a "cons" constructor with 2 fields:
--     + "head" of sort "X"
--     + "tail" of sort "(ListX X)"
declareListXType :: Doc
declareListXType =
  declareDataType "ListX" ["X"] [ ("nil", [])
                                , ("cons", [ ("head", "X")
                                           , ("tail", "(ListX X)") ]) ]

-- | Construct a ListX from a '[Sting]' by chaining the appropriate sequence of
--   "cons" and a "nil" adequately qualified
mkListX :: [String] -> String -> Doc
mkListX [] lstSort = qualify (text "nil") (text $ "(ListX "++lstSort++")")
mkListX (e:es) lstSort = applyOp (text "cons") [ text e, mkListX es lstSort ]

-- | Define the "andReduce" SMT2 function operating on a "(ListX Bool)",
--   reducing it using "and"
defineAndReduce :: Doc
defineAndReduce = defineFunRec (text "andReduce")
                               [ (text "lst", text "(ListX Bool)") ]
                               (text "Bool")
                               fBody
  where fBody = matchBind (text "lst")
                          [ (text "nil", text "true")
                          , ( text "(cons h t)"
                            , applyOp (text "and")
                                      [ text "h"
                                      , applyOp (text "andReduce")
                                                [text "t"] ] ) ]

-- | Define the "impliesReduce" SMT2 function operating on a "(ListX Bool)",
--   reducing it using "=>" (implication)
defineImpliesReduce :: Doc
defineImpliesReduce = defineFunRec (text "impliesReduce")
                                   [ (text "lst", text "(ListX Bool)") ]
                                   (text "Bool")
                                   fBody
  where fBody = matchBind (text "lst")
                          [ (text "nil", text "true")
                          , ( text "(cons h t)"
                            , matchBind (text "t") recCases ) ]
        recCases = [ (text "nil", text "h")
                   , (text "(cons hh tt)", applyOp (text "=>") [ text "h"
                                                               , recCall ]) ]
        recCall = applyOp (text "impliesReduce") [text "t"]

-- | Define the "init", "allDifferent" and "distinctFrom" family of functions on
--   ListX parameterised on the provided sort. For a given sort S, the defined
--   functions are:
--     * "init_ListX_S" - return all but the last element of the list or nil for
--                        an empty list
--     * "allDifferent_ListX_S" - return true if all elements of the list are
--                                distinct
--     * "distinctFrom_ListX_S" - return true if the first argument is distinct
--                                from all the elements in the second argument
defineDistinctListX :: String -> Doc
defineDistinctListX elemSort = defineFunsRec
  [ ( "init" ++ sfx, [ ("lst", lstSort) ], lstSort
    , matchBind (text "lst")
                [ ( text "nil", qualify (text "nil") (text lstSort))
                , ( text "(cons h t)"
                  , matchBind (text "t")
                              [ ( text "nil"
                                , qualify (text "nil") (text lstSort))
                              , ( text "(cons hh tt)"
                                , applyOp (text "cons")
                                          [ text "h"
                                          , applyOp (text $ "init" ++ sfx)
                                                    [ text "t" ]])])])
  , ( "allDifferent" ++ sfx, [ ("lst", lstSort) ], "Bool"
    , matchBind (text "lst")
                [ ( text "nil", text "true" )
                , ( text "(cons h t)"
                  , applyOp (text "and")
                            [ applyOp (text $ "distinctFrom" ++ sfx)
                                      [text "h", text "t"]
                            , applyOp (text $ "allDifferent" ++ sfx)
                                      [ text "t" ]])])
  , ( "distinctFrom" ++ sfx, [ ("s", elemSort), ("ss", lstSort) ], "Bool"
    , matchBind (text "ss")
                [ ( text "nil", text "true" )
                , ( text "(cons h t)"
                  , applyOp (text "and")
                            [ applyOp (text "distinct") [text "s", text "h"]
                            , applyOp (text $ "distinctFrom" ++ sfx)
                                      [ text "s", text "t" ]])])]
  where lstSort = "(ListX " ++ elemSort ++ ")"
        sfx = "_ListX_" ++ elemSort

-- | Define the repeated application of a function f of 2 arguments returning a
--   pair where
--   * the first argument of f is taken from an overall argument list
--   * the second argument of f is an explicit argument of first invocation, and
--     is the second member of the pair returned by the previous invocation on
--     subsequent calls
--   * the call depth is defined by the length of the explicit argument list
--   * the overall return is a pair of reversed lists of individual returned
--     values
defineChain :: String -> (String, (String, String), String) -> Doc
defineChain name (f, (a0Sort, a1Sort), retSort) =
  defineFunRec (text name)
               [ (text "xs", text a0Lst)
               , (text "prev", text a1Sort) ]
               (text $ "(Tuple2 " ++ retLst ++ " " ++ a1Lst ++ ")") fBody
  where fBody = matchBind (text "xs")
                          [ (text "nil", lastRet)
                          , (text "(cons h t)", matchInvoke) ]
        lastRet = applyOp (text "mkTuple2")
                          [ qualify (text "nil") (text retLst)
                          , applyOp (text "cons")
                                    [ text "prev"
                                    , qualify (text "nil")
                                              (text a1Lst) ] ]
        matchInvoke = matchBind (applyOp (text f) [text "h", text "prev"])
                                [(text "(mkTuple2 ret next)", matchRecCall)]
        matchRecCall = matchBind (applyOp (text name) [text "t", text "next"])
                                 [( text "(mkTuple2 rets ys)", recRet)]
        recRet = applyOp (text "mkTuple2")
                         [ applyOp (text "cons") [text "ret", text "rets"]
                         , applyOp (text "cons") [text "next", text "ys"] ]
        retLst = "(ListX " ++ retSort ++ ")"
        a0Lst = "(ListX " ++ a0Sort ++ ")"
        a1Lst = "(ListX " ++ a1Sort ++ ")"
