import System.Directory

import Blarney
import qualified Blarney.Vector as V

-- | Split a list into two halves
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

-- | Sklansky parallel prefix network (assumes power-of-2 list size)
sklansky :: (a -> a -> a) -> [a] -> [a]
sklansky op [x] = [x]
sklansky op xs = ys' ++ map (last ys' `op`) zs'
  where
    (ys, zs) = halve xs
    ys' = sklansky op ys
    zs' = sklansky op zs

-- | Broken Sklansky implementation
doom_sklansky :: (a -> a -> a) -> [a] -> [a]
doom_sklansky op [x] = [x]
doom_sklansky op xs = ys' ++ map (head ys' `op`) zs'
  where
    (ys, zs) = halve xs
    ys' = sklansky op ys
    zs' = sklansky op zs

-- | Does parallel scan equal sequential scan for given operator?
prop_scan :: forall n m.
             ((Bit n -> Bit n -> Bit n) -> [Bit n] -> [Bit n])
          -> (Bit n -> Bit n -> Bit n)
          -> V.Vec m (Bit n)
          -> Bit 1
prop_scan sklansky_imp op ins = equal
  where
    res0 = sklansky_imp op (V.toList ins)
    res1 = scanl1 op (V.toList ins)
    equal = andList (zipWith (.==.) res0 res1)

main :: IO ()
main = do
  -- path to script output directory
  cwd <- getCurrentDirectory
  let smtDir = cwd ++ "/Sklansky-SMT/"
  -- generate smt2 scripts
  let verifyConf = dfltVerifyConf
  writeSMTScript verifyConf (prop_scan @4 @4 sklansky (+))
                 "goodSklansky4_4" smtDir
  writeSMTScript verifyConf (prop_scan @4 @4 doom_sklansky (+))
                 "brokenSklansky4_4" smtDir
  -- helper usage message
  putStrLn $ "SMT2 scripts generated under " ++ smtDir
  putStrLn $ "Run an SMT solver such as z3 with an SMT2 script as input:"
  putStrLn $ "    $ z3 " ++ smtDir ++ "goodSklansky4_4.smt2"
  putStrLn $ "This should return \"unsat\". Running:"
  putStrLn $ "    $ z3 " ++ smtDir ++ "brokenSklansky4_4.smt2"
  putStrLn $ "should return \"sat\". Run:"
  putStrLn $ "    $ echo \"(get-model)\" >> "
             ++ smtDir ++ "brokenSklansky4_4.smt2"
  putStrLn $ "    $ z3 " ++ smtDir ++ "brokenSklansky4_4.smt2"
  putStrLn $ "to get a set of input assignments for which the tested property"
             ++ " does not hold."
