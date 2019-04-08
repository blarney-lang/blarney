-- Heat diffusion on a 2D mesh

import Blarney
import Mesh
import Data.List

-- Temperature type
type Temp = Bit 32

-- Update cell using values of neighbours
step :: Reg Temp -> [Temp] -> Action ()
step me neighbours =
  me <== sumList neighbours .>>. (2 :: Bit 2)

-- Top-level
top :: Integer -> Int -> Int -> Module ()
top t w h = do
  -- North and east borders (initialised hot)
  north <- replicateM w (makeReg 0xff0000)
  east  <- replicateM (h-2) (makeReg 0xff0000)
  -- South and west borders (initialised cold)
  south <- replicateM w (makeReg 0x2a0000)
  west  <- replicateM (h-2) (makeReg 0x2a0000)
  -- Remaining cells
  cells <- replicateM (h-2) (replicateM (w-2) (makeReg 0))
  -- Count time steps
  timer :: Reg (Bit 32) <- makeReg 0
  -- Overall grid
  let grid = [north]
          ++ transpose ([east] ++ transpose cells ++ [west])
          ++ [south]
  always do
    -- Mesh
    mesh step grid
    -- Increment time
    timer <== timer.val + 1
    -- Termination
    when (timer.val .==. fromInteger t) $ do
      forM_ (zip [0..] grid) $ \(i, row) -> 
        forM_ (zip [0..] row) $ \(j, cell) ->
          display (show i) "," (show j) ":0x%08x" (cell.val .>>. (16 :: Bit 5))
      finish

-- Main function
main :: IO ()
main = writeVerilogTop (top 5000 16 16) "top" "Heat-Verilog/"
