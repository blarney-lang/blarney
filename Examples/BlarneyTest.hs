module BlarneyTest (
  blarneyTestMain
) where

import Prelude
import Data.IORef
import System.Environment
import System.Console.GetOpt
import qualified Control.Monad as M

import Blarney

-- Blarney options
data TestOpts = TestOpts { testOptsVerilogGen :: Bool
                         , testOptsSimulation :: Bool }

defaultTestOpts :: TestOpts
defaultTestOpts = TestOpts { testOptsVerilogGen = False
                           , testOptsSimulation = False }

options :: [OptDescr (TestOpts -> TestOpts)]
options = [ Option [] ["test-verilog-gen"]
              (NoArg \opts -> opts { testOptsVerilogGen = True })
              "Generate verilog for the test module"
          , Option [] ["test-simulation"]
              (NoArg \opts -> opts { testOptsSimulation = True })
              "Simulate the test module" ]

parseTestOpts :: [String] -> (TestOpts, [String])
parseTestOpts args = case getOpt' Permute options args of
  (opts, unused0, unused1, []) ->
    (foldl (flip id) defaultTestOpts opts, unused1 ++ unused0)
  (_, _, _, errs) ->
    error (concat errs ++ usageInfo "BlarneyTest options: " options)

getTestOpts :: IO (TestOpts, [String])
getTestOpts = parseTestOpts <$> getArgs

--blarneyTestMain :: Modular a => String -> a -> IO ()
blarneyTestMain :: String -> Module () -> IO ()
blarneyTestMain name mod = do
  -- get test options from the command line
  (testOpts, _) <- getTestOpts
  -- did anything run?
  nothingRan <- newIORef True
  -- test verilog generation
  M.when (testOptsVerilogGen testOpts) do verilogTest
                                          writeIORef nothingRan False
  -- test simulation
  M.when (testOptsSimulation testOpts) do simulationTest
                                          writeIORef nothingRan False
  -- always run default test if nothing else was explicitly specified
  doDefltTest <- readIORef nothingRan
  M.when doDefltTest dfltTest
  --
  return ()
  where verilogTest = writeVerilogTop mod name (name ++ "-Verilog/")
        simulationTest = simulate mod
        dfltTest = simulationTest
