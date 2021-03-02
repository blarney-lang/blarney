{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds   #-}

{-|
Module      : Blarney.BitScan
Description : Bit-string pattern matching, similar to scanf
Copyright   : (c) Matthew Naylor, 2019
License     : MIT
Maintainer  : mattfn@gmail.com
Stability   : experimental

Dynamically typed pattern matching on bit-strings, similar to scanf.
Here's an instruction decoder for a tiny subset of RISC-V, written
using the 'match' and '==>' combinators provided by the module:

@
-- Semantics of add instruction
add :: Bit 5 -> Bit 5 -> Bit 5 -> Action ()
add rs2 rs1 rd = display "add " rd ", " rs1 ", " rs2

-- Semantics of addi instruction
addi :: Bit 12 -> Bit 5 -> Bit 5 -> Action ()
addi imm rs1 rd = display "addi " rd ", " rs1 ", " imm

-- Semantics of store instruciton
sw :: Bit 12 -> Bit 5 -> Bit 5 -> Action ()
sw imm rs2 rs1 = display "sw " rs2 ", " rs1 "[" imm "]"

top :: Action ()
top = do
  let instr :: Bit 32 = 0b1000000_00001_00010_010_00001_0100011

  match instr
    [
      "0000000   rs2[4:0]  rs1[4:0] 000 rd[4:0]  0110011" ==> add,
      "          imm[11:0] rs1[4:0] 000 rd[4:0]  0010011" ==> addi,
      "imm[11:5] rs2[4:0]  rs1[4:0] 010 imm[4:0] 0100011" ==> sw
    ]

  finish
@
-}
module Blarney.BitScan
  ( (==>)
  , Alt
  , match
  , matchDefault
  , matchOpts
  , MatchOpts(..)
  , matchMap
  , TagMap
  , Tag
  , packTagMap
  , FieldMap
  , matchSel
  , SelMap
  , getField
  , getFieldStrict
  , getFieldSel
  , makeFieldSelector
  ) where

import Blarney hiding (Range)
import Blarney.Option

import Data.Char
import Data.List
import Data.Map (Map, fromList, fromListWith,
                   toList, lookup, mapWithKey, elems)

type BitList = [Bit 1]

data Token =
    Lit String
  | Var String
  | Range String Int Int
  deriving (Show)

tokenise :: String -> [Token]
tokenise = init []
  where
    isBit c = c == '0' || c == '1'

    init acc [] = acc
    init acc (c:cs)
      | isSpace c = init acc cs
      | isBit c = lit acc (c:cs)
      | c == '<' = len "" acc cs
      | otherwise = var "" acc (c:cs)

    var str acc [] = init (Var (reverse str) : acc) []
    var str acc (c:cs)
      | c == '[' = high (reverse str) acc cs
      | c == '<' = len (reverse str) acc cs
      | c == ' ' = init (Var (reverse str) : acc) cs
      | otherwise = var (c:str) acc cs

    len id acc cs =
      case takeWhile isDigit cs of
        [] -> error "BitScan: expected width"
        ds -> lenClose (Range id (n-1) 0 : acc) (dropWhile isDigit cs)
          where n = read ds :: Int

    lenClose acc ('>':cs) = init acc cs
    lenClose acc other = error "BitScan: expected '>'"

    high id acc cs =
      case takeWhile isDigit cs of
        [] -> error "BitScan: expected high number"
        ds -> colon id n acc (dropWhile isDigit cs)
          where n = read ds :: Int

    colon id high acc (':':cs) = low id high acc cs
    colon id high acc (']':cs) = init (Range id high high : acc) cs
    colon id high acc other = error "BitScan: expected ':'"

    low id high acc cs =
      case takeWhile isDigit cs of
        [] -> error "BitScan: expected low number"
        ds -> if   n > high
              then error "BitScan: range error"
              else close (Range id high n : acc) (dropWhile isDigit cs)
          where n = read ds :: Int

    close acc (']':cs) = init acc cs
    close acc other = error "BitScan: expected ']'"

    lit acc cs = init (Lit s : acc) (dropWhile isBit cs)
      where s = reverse (takeWhile isBit cs)

-- A token tagged with the bit offset of it's LSB within the pattern
data TaggedToken = Tag Int Token
  deriving Show

tag :: [Token] -> [TaggedToken]
tag = tagger 0
  where
    tagger n [] = []
    tagger n (t:ts) =
      case t of
        Lit bs -> Tag n t : tagger (n + length bs) ts
        Var v -> error "BitScan: unranged vars not supported"
        Range "" hi lo -> tagger (n + (hi-lo) + 1) ts
        Range v hi lo -> Tag n t : tagger (n + (hi-lo) + 1) ts

-- Position of a bit in the subject being matched
type BitPos = Maybe Int

-- A list of bit positions
type BitPosList = [BitPos]

-- Mapping from var bit-index to subject bit-index
type Mapping = [(Int, BitPos)]

mapping :: String -> [TaggedToken] -> Mapping
mapping v toks =
  concat [ zip [lo..hi] (fmap Just [n..])
         | Tag n (Range w hi lo) <- toks, v == w ]

-- Join a scattered bit-string, complain if gaps or overlapping
unscatter :: String -> [(Int, BitPos)] -> BitPosList
unscatter field = join 0
  where
   join i [] = []
   join i m =
     case [pos | (j, pos) <- m, i == j] of
       [] -> Nothing : join (i+1) m
       [pos] -> pos : join (i+1) [p | p <- m, fst p /= i]
       other -> error ("BitScan: overlapping variable assignment " ++
                       "in field: " ++ field)

-- Extra fields from subject using pattern
fields :: [TaggedToken] -> [(String, BitPosList)]
fields toks = get (reverse toks)
  where
    notVar v (Tag i (Range w hi lo)) = v /= w
    notVar v other = False

    get [] = []
    get ts@(Tag i (Range v hi lo) : rest) =
      (v, unscatter v (mapping v ts)) :
        get (filter (notVar v) rest)
    get (t:ts) = get ts

-- Determine argument values to right-hand-side
args :: BitList -> [TaggedToken] -> [BitList]
args subj toks = [fmap bit ps | (_, ps) <- fields toks]
  where
    bit Nothing = error "BitScan: non-contiguous variable assignment"
    bit (Just i) = subj !! i

-- Determine width of a token
tokenWidth :: Token -> Int
tokenWidth (Var v) = error "BitScan: tokenWidth not defined for unranged vars"
tokenWidth (Range v hi lo) = (hi-lo)+1
tokenWidth (Lit bs) = length bs

-- |A bit pattern consists of a list of pattern bits
type Pattern = [PatternBit]

-- |Pattern bit: zero, one, or don't care
data PatternBit = Zero | One | X
  deriving Eq

-- |Convert a token stream to a bit pattern
toPattern :: [Token] -> Pattern
toPattern [] = []
toPattern (t:ts) =
  case t of
    Var v -> error "BitScan: unranged vars not supported"
    Lit bs -> [if b == '0' then Zero else One | b <- bs] ++ toPattern ts
    Range id hi lo -> replicate ((hi-lo)+1) X ++ toPattern ts

-- |Match alternative
data Alt =
  Alt {
    altPattern :: Pattern
  , altBody    :: BitList -> Action ()
  }

-- |Class capturing the right-hand-side of a match alternative
class RHS f where
  apply :: f -> [BitList] -> Action ()

instance RHS (Action ()) where
  apply f [] = f
  apply f other = error "BitScan: too many pattern vars"

instance (RHS f, KnownNat n) => RHS (Bit n -> f) where
  apply f [] = error "BitScan: too few pattern vars"
  apply f (arg:args) = apply (f (fromBitList arg)) args

-- |Infix operator to construct a match alternative
infix 1 ==>
(==>) :: RHS rhs => String -> rhs -> Alt
fmt ==> rhs =
  Alt {
    altPattern = toPattern toks
  , altBody = \subj -> apply rhs (args subj (tag toks))
  }
  where toks = tokenise fmt

-- |Match a subject against a pattern
isMatch :: BitList -> Pattern -> Bit 1
isMatch subj pat
  | length pat /= length subj = error "BitScan: width mismatch"
  | otherwise = andList (concat (zipWith matchBit pat subj))
  where
    matchBit X x = []
    matchBit One x      = [x]
    matchBit Zero x     = [inv x]

-- |Match a subject against many patterns (simple algorithm)
isMatchManySimple :: BitList -> [Pattern] -> [Bit 1]
isMatchManySimple subj pats = fmap (isMatch subj) pats

-- |Match a subject against many patterns (exploit sharing).
-- Overview: Is any bit position non-X in every pattern?
--   Yes: select bit position with most 1's or most 0's
--     * split patterns based on this bit, into those with 1's and
--       that with 0's at this position
--     * remove the bit from subject and each pattern
--     * for patterns where bit is 1: AND the bit with accum and recurse
--     * for patterns where bit is 0: INV the bit and AND with accum and recurse
--   No: select bit position with most X's
--     * split patterns bsaed on this bit, into X's and non-X's
--     * for patterns where bit is X: remove bit from subject
--       and each pattern, and recurse
--     * for patterns where bit is non-X: recurse
isMatchManyShare :: BitList -> [Pattern] -> [Bit 1]
isMatchManyShare subj pats
  | not isValid = error ("BitScan: width mismatch in pattern set. " ++
                         "Widths: " ++ show (map length pats))
  | otherwise =
        fmap snd
      $ sortBy cmpFst
      $ match 1 [0..numPats-1] subj pats
  where
    numPats = length pats
    numBits = length subj
    isValid = and [len == numBits | len <- fmap length pats]
    cmpFst x y = fst x `compare` fst y

    match cond ids subj pats
      | all null pats = [(id, cond) | id <- ids]
      | null someX =
             match (cond .&. inv splitBit)
                   ids0
                   (remove splitBitPos subj)
                   [pat | (id, pat) <- zip ids pats', id `elem` ids0]
          ++ match (cond .&. splitBit)
                   ids1
                   (remove splitBitPos subj)
                   [pat | (id, pat) <- zip ids pats', id `elem` ids1]
      where
        patsT = transpose pats

        -- Which bit position is X in every alternative?
        (noneX, someX) = partition (all (/= X)) patsT

        -- Count max number of 0's or 1's at each bit position
        counts = [ length [() | One <- bs] `max`
                     length [() | Zero <- bs]
                 | bs <- noneX ]

        -- Which bit position has the most sharing?
        splitBitPos = snd (maximum (zip counts [0..]))
        splitBit = subj !! splitBitPos
        
        -- Split the alternatives
        ids0 = [id | (id, Zero) <- zip ids (patsT !! splitBitPos)]
        ids1 = [id | (id, One) <- zip ids (patsT !! splitBitPos)]

        -- Remove the split bit
        pats' = transpose (remove splitBitPos patsT)
    match cond ids subj pats =
           match cond
                 idsX
                 (remove remBitPos subj)
                 [pat | (id, pat) <- zip ids pats', id `elem` idsX]
        ++ match cond
                 idsNotX
                 subj
                 [pat | (id, pat) <- zip ids pats, id `elem` idsNotX]
      where
        patsT = transpose pats

        -- Count max number of X's at each bit position
        counts = [ length [() | X <- bs] | bs <- patsT ]

        -- Which bit position has the most X's?
        remBitPos = snd (maximum (zip counts [0..]))

        -- Split the alternatives
        idsX = [id | (id, X) <- zip ids (patsT !! remBitPos)]
        idsNotX = ids \\ idsX

        -- Remove the split bit
        pats' = transpose (remove remBitPos patsT)

    -- Remove element at index i of list xs
    remove i xs = take i xs ++ drop (i+1) xs

-- |Match options
data MatchOpts =
  MatchOpts {
    -- Default case (optional)
    matchOptDefault :: Maybe (Action ())
    -- Use simple matching algorithm?
  , matchOptUseSimpleAlgorithm :: Bool
  }

-- |General parameterised match statement
matchOpts :: KnownNat n => MatchOpts -> Bit n -> [Alt] -> Action ()
matchOpts opts subj alts = do
  let subj' = toBitList subj
  let conds = if matchOptUseSimpleAlgorithm opts
                then isMatchManySimple subj' (fmap altPattern alts)
                else isMatchManyShare subj' (fmap altPattern alts)
  let bodies = [altBody alt subj' | alt <- alts]
  sequence_ [when cond body | (cond, body) <- zip conds bodies]
  case matchOptDefault opts of
    Nothing  -> return ()
    Just act -> when (inv (orList conds)) act

-- |Match statement, with a subject and a list of alternatives
match :: KnownNat n => Bit n -> [Alt] -> Action ()
match subj alts = matchOpts opts subj alts
  where
    opts = MatchOpts {
             matchOptDefault = Nothing
           , matchOptUseSimpleAlgorithm = False
           }

-- |Match statement, with a default case
matchDefault :: KnownNat n => Bit n -> [Alt] -> Action () -> Action ()
matchDefault subj alts def = matchOpts opts subj alts
  where
    opts = MatchOpts {
             matchOptDefault = Just def
           , matchOptUseSimpleAlgorithm = False
           }

-- |Mapping from field names to optional bit lists
type FieldMap = Map String (Option BitList)

-- |Mapping from tag names to hot bits
type TagMap tag = Map tag (Bit 1)

-- |Common constraint of tags in a TagMap
type Tag a = (Ord a, Enum a, Bounded a)

-- |Options for the match
data MatchMapOpts =
  MatchMapOpts {
    matchMapOptStrict :: Bool
  }

-- |Compute tag and field maps given list of pattern/tag pairs.
-- This is a relaxed version which fills field gaps with zero, and
-- sign-extends each field to the length of the longest instance of
-- that field.
matchMap :: (KnownNat n, Ord tag) =>
  Bool -> [(String, tag)] -> Bit n -> (TagMap tag, FieldMap)
matchMap strict alts subj = (tagMap, mapWithKey combine fieldMap)
  where
    tokLists = [(tokenise fmt) | (fmt, _) <- alts]
    pats = [tag toks | toks <- tokLists]
    subj' = toBitList subj
    conds = isMatchManyShare subj' (fmap toPattern tokLists)
    tagMap = fromListWith (.|.) (zip (fmap snd alts) conds)
    fieldMap = fromListWith (++) [ (name, [(cond, bits)])
                                 | (cond, pat) <- zip conds pats
                                 , (name, bits) <- fields pat ]
    combine field choices
      | null choices = error "BitScan.matchMap: combine"
      | allSame = Option valid (interpret (snd (head choices)))
      | otherwise = Option valid value
      where
        allSame = length (nub (fmap snd choices)) == 1
        valid = orList (fmap fst choices)
        lens = [length bits | (_, bits) <- choices]
        maxLen = maximum lens
        value = fmap orList $ transpose
                  [ fmap (cond .&.) (interpret bits)
                  | (cond, bits) <- choices ]

        interpret bits =
          extend [ case b of
                     Nothing -> 0
                     Just i  -> subj' !! i
                 | b <- bits ]

        extend [] = error "BitScan.matchMap: extend"
        extend xs
          | strict && maxLen /= length xs =
              error ("BitScan.matchMap: different widths for field " ++ field)
          | otherwise = take maxLen (xs ++ repeat (last xs))

-- |Pack the tag map into a bit vector
packTagMap :: (KnownNat n, Tag tag) => TagMap tag -> Bit n
packTagMap tagMap
  | w >= numBits = vec
  | otherwise = error
      "BitScan.packTagMap: bit vector not big enough capture all tags"
  where
    w = widthOf vec
    bits = [ case Data.Map.lookup tag tagMap of
               Nothing -> 0
               Just v -> v
           | tag <- [minBound..maxBound] ]
    numBits = length bits
    vec = fromBitList $ take w (bits ++ repeat 0)

-- |Get field value from a map, and cast to required size using
-- truncation or sign-extension.
getField :: KnownNat n => FieldMap -> String -> Option (Bit n)
getField m key = result
  where
    result =
      case Data.Map.lookup key m of
        Nothing -> error ("BitScan.getField: unknown key " ++ key)
        Just opt -> Option (opt.valid) (fromBitList (resize (opt.val)))

    resize [] = error "BitScan.getField: resize"
    resize list = take (widthOf (result.val)) (list ++ repeat (last list))

-- |Get field value from a map, and raise an error the size is incorrect
getFieldStrict :: KnownNat n => FieldMap -> String -> Option (Bit n)
getFieldStrict m key = result
  where
    result =
      case Data.Map.lookup key m of
        Nothing -> error ("BitScan.getFieldStrict: unknown key " ++ key)
        Just opt -> Option (opt.valid) (fromBitList (resize (opt.val)))

    resize list
      | length list == widthOf (result.val) = list
      | otherwise = error ("BitScan.getFieldStrict: width mismatch "  ++
                             "for field " ++ key)

-- |Mapping from field names to field selectors
type SelMap n = Map String (Bit n -> BitList)

-- |Compute a field selector function for each field that occupies
-- precisely the same bits in every match alternative in which it occurs
matchSel :: KnownNat n => [(String, tag)] -> SelMap n
matchSel alts = mapWithKey combine fieldMap
  where
    tokLists = [(tokenise fmt) | (fmt, _) <- alts]
    pats = [tag toks | toks <- tokLists]
    fieldMap = fromListWith (++) [ (name, [bits])
                                 | pat <- pats
                                 , (name, bits) <- fields pat ]
    combine field choices
      | null choices = error "BitScan.matchSel: combine"
      | allSame = interpret (head choices)
      | otherwise =
          error ("BitScan.matchSel does not currently compute" ++
                 "field selector functions for non-uniform fields " ++
                 "(field=" ++ field ++ ")")
      where
        allSame = length (nub choices) == 1
        interpret bits = \subj ->
          let subj' = toBitList subj in
            [ case b of
                Nothing -> 0
                Just i  -> subj' !! i
            | b <- bits ]

-- |Get field selector function from a selector map.
getFieldSel :: (KnownNat n, KnownNat m) =>
  SelMap n -> String -> Bit n -> Bit m
getFieldSel m key subj = result
  where
    result =
      case Data.Map.lookup key m of
        Nothing -> error ("BitScan.getFieldSel: undefined field " ++ key)
        Just f -> fromBitList (resize (f subj))

    resize list
      | length list == widthOf result = list
      | otherwise = error ("BitScan.getFieldSel: width mismatch "  ++
                             "for result of field selector " ++ key)

-- |Make field selector function from a list of alternatives.
makeFieldSelector :: (KnownNat n, KnownNat m) =>
  [(String, tag)] -> String -> Bit n -> Bit m
makeFieldSelector alts field =
  let selMap = matchSel alts in
    \subj -> getFieldSel selMap field subj
