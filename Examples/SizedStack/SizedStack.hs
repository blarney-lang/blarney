import Blarney
import Blarney.Stmt
import Blarney.Stack
import System.Environment

-- Test bench
-- ==========

topLevel :: Module ()
topLevel = do
  -- Create 256-element stack
  stk :: Stack (Bit 8) <- makeSizedStack 4

  -- Sample test sequence
  runStmt do
    action do
      stk.push 1
    action do
      stk.push 2
      display stk.top
    action do
      stk.push 3
      display stk.top
    action do
      stk.push 4
      display stk.top
    action do
      stk.pop
      display stk.top
    action do
      stk.pop
      display stk.top
    action do
      stk.push 5
      stk.pop
      display stk.top
    action do
      stk.pop
      display stk.top
    action do
      display stk.top
    action do
      display stk.top
      finish

-- Code generation
-- ===============

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate topLevel
     | otherwise ->
         writeVerilogTop topLevel "SizedStack" "SizedStack-Verilog/"
