{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE FlexibleInstances #-}

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
  , match
  , matchDefault
  , Alt
  , MatchOpts(..)
  ) where

import Blarney
import Data.Char
import Data.List

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
        [] -> error "Format error: expected width"
        ds -> lenClose (Range id (n-1) 0 : acc) (dropWhile isDigit cs)
          where n = read ds :: Int

    lenClose acc ('>':cs) = init acc cs
    lenClose acc other = error "Format error: expected '>'"

    high id acc cs =
      case takeWhile isDigit cs of
        [] -> error "Format error: expected high number"
        ds -> colon id n acc (dropWhile isDigit cs)
          where n = read ds :: Int

    colon id high acc (':':cs) = low id high acc cs
    colon id high acc (']':cs) = init (Range id high high : acc) cs
    colon id high acc other = error "Format error: expected ':'"

    low id high acc cs =
      case takeWhile isDigit cs of
        [] -> error "Format error: expected low number"
        ds -> if   n > high
              then error "Format error: range error"
              else close (Range id high n : acc) (dropWhile isDigit cs)
          where n = read ds :: Int

    close acc (']':cs) = init acc cs
    close acc other = error "Format error: expected ']'"

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
        Var v -> error "tag: unranged vars not supported"
        Range "" hi lo -> tagger (n + (hi-lo) + 1) ts
        Range v hi lo -> Tag n t : tagger (n + (hi-lo) + 1) ts

-- Mapping from var bit-index to subject bit-index
type Mapping = [(Int, Int)]

mapping :: String -> [TaggedToken] -> Mapping
mapping v toks =
  concat [ zip [lo..hi] [n..]
         | Tag n (Range w hi lo) <- toks, v == w ]

-- Perform a substitution on a subject
subst :: Mapping -> BitList -> BitList
subst m bs = unscatter [(bi, bs !! si) | (bi, si) <- m]

-- Join a scattered bit-string, complain if gaps or overlapping
unscatter :: [(Int, Bit 1)] -> BitList
unscatter = join 0
  where
   join i [] = []
   join i m =
     case [x | (j, x) <- m, i == j] of
       [] -> error "Format error: non-contiguous variable assignment"
       [x] -> x : join (i+1) [p | p <- m, fst p /= i]
       other -> error "Format error: overlapping variable assignment"

-- Determine argument values to right-hand-side
args :: BitList -> [TaggedToken] -> [BitList]
args subj = get `o` reverse
  where
    notVar v (Tag i (Range w hi lo)) = v /= w
    notVar v other = False

    get [] = []
    get ts@(Tag i (Range v hi lo) : rest) =
      subst (mapping v ts) subj :
        get (filter (notVar v) rest)
    get (t:ts) = get ts

-- Determine width of a token
tokenWidth :: Token -> Int
tokenWidth (Var v) = error "Error: tokenWidth not defined for unranged vars"
tokenWidth (Range v hi lo) = (hi-lo)+1
tokenWidth (Lit bs) = length bs

-- |A bit pattern consists of a list of pattern bits
type Pattern = [PatternBit]

-- |Pattern bit
data PatternBit = Zero | One | X
  deriving Eq

-- |Convert a token stream to a bit pattern
toPattern :: [Token] -> Pattern
toPattern [] = []
toPattern (t:ts) =
  case t of
    Var v -> error "Format error: unranged vars not supported"
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
  apply f other = error "Format error: too many pattern vars"

instance (RHS f, KnownNat n) => RHS (Bit n -> f) where
  apply f [] = error "Format error: too few pattern vars"
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
  | length pat /= length subj = error "BitScan format error: width mismatch"
  | otherwise = andList (concat (zipWith matchBit pat subj))
  where
    matchBit X x = []
    matchBit One x      = [x]
    matchBit Zero x     = [inv x]

-- |Match a subject against many patterns (simple algorithm)
isMatchManySimple :: BitList -> [Pattern] -> [Bit 1]
isMatchManySimple subj pats = map (isMatch subj) pats

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
  | not isValid = error "BitScan format error: width mismatch"
  | otherwise =
        map snd
      $ sortBy cmpFst
      $ match 1 [0..numPats-1] subj pats
  where
    numPats = length pats
    numBits = length subj
    isValid = and [len == numBits | len <- map length pats]
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
matchGeneral :: KnownNat n => Bit n -> [Alt] -> MatchOpts -> Action ()
matchGeneral subj alts opts = do
  let subj' = toBitList subj
  let conds = if matchOptUseSimpleAlgorithm opts
                then isMatchManySimple subj' (map altPattern alts)
                else isMatchManyShare subj' (map altPattern alts)
  let bodies = [altBody alt subj' | alt <- alts]
  sequence_ [when cond body | (cond, body) <- zip conds bodies]
  case matchOptDefault opts of
    Nothing  -> return ()
    Just act -> when (inv (orList conds)) act

-- |Match statement, with a subject and a list of alternatives
match :: KnownNat n => Bit n -> [Alt] -> Action ()
match subj alts = matchGeneral subj alts opts
  where
    opts = MatchOpts {
             matchOptDefault = Nothing
           , matchOptUseSimpleAlgorithm = False
           }

-- |Match statement, with a default case
matchDefault :: KnownNat n => Bit n -> [Alt] -> Action () -> Action ()
matchDefault subj alts def = matchGeneral subj alts opts
  where
    opts = MatchOpts {
             matchOptDefault = Just def
           , matchOptUseSimpleAlgorithm = False
           }
