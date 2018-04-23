{-# LANGUAGE FlexibleInstances #-}

module Blarney.BitScan where

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
      | otherwise = var "" acc (c:cs)

    var str acc [] = init (Var (reverse str) : acc) []
    var str acc (c:cs)
      | c == '[' = high (reverse str) acc cs 
      | c == ' ' = init (Var (reverse str) : acc) cs
      | otherwise = var (c:str) acc cs

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

-- Match literals in pattern against subject
matches :: BitList -> [Token] -> Bit 1
matches subj toks
  | width /= length subj = error "Format error: width mismatch"
  | otherwise = check 0 toks
  where
    width = sum (map tokenWidth toks)

    check n [] = 1
    check n (t : rest) =
      case t of
        Var v -> error "Format error: unranged vars not supported"
        Range id hi lo -> check (n + (hi-lo) + 1) rest
        Lit bs ->
              andList [ if c == '0' then inv b else b
                      | (c, b) <- zip bs (drop n subj) ]
          .&. check (n + length bs) rest

class RHS f where
  apply :: f -> [BitList] -> RTL ()

instance RHS (RTL ()) where
  apply f [] = f
  apply f other = error "Format error: too many pattern vars"

instance (RHS f, KnownNat n) => RHS (Bit n -> f) where
  apply f [] = error "Format error: too few pattern vars"
  apply f (arg:args) = apply (f (fromList arg)) args

toBitList :: KnownNat n => Bit n -> BitList
toBitList x = [getBit i x | i <- [0 .. widthOf x - 1]]

infix 7 ==>
(==>) :: KnownNat n => RHS rhs => String -> rhs -> Bit n -> RTL ()
fmt ==> rhs = \subj -> do
  let subj' = toBitList subj
  when (matches subj' toks) $
    apply rhs (args subj' (tag toks))
  where toks = tokenise fmt

match :: KnownNat n => Bit n -> [Bit n -> RTL ()] -> RTL ()
match subj alts = sequence_ [alt subj | alt <- alts]
