-- Heat diffusion on a 2D mesh

import Blarney
import Mesh
import Data.List

-- Temperature type
type Temp = Bit 32

-- Update cell using values of neighbours
update :: Reg Temp -> [Temp] -> RTL ()
update me neighbours =
  me <== sumList neighbours .>>. 2

-- Top-level
top :: Integer -> Int -> Int -> RTL ()
top t w h = do
  -- North and east borders (initialised hot)
  north <- replicateM w (makeReg 0xff0000)
  east  <- replicateM (h-2) (makeReg 0xff0000)
  -- South and west borders (initialised cold)
  south <- replicateM w (makeReg 0x2a0000)
  west  <- replicateM (h-2) (makeReg 0x2a0000)
  -- Remaining cells
  cells <- replicateM (h-2) (replicateM (w-2) (makeReg 0))
  -- Overall grid
  let grid = [north]
          ++ transpose ([east] ++ transpose cells ++ [west])
          ++ [south]
  -- Mesh
  mesh update grid
  -- Count time steps
  timer :: Reg (Bit 32) <- makeReg 0
  timer <== val timer + 1
  -- Termination
  when (val timer .==. fromInteger t) $ do
    forM_ (zip [0..] grid) $ \(i, row) -> 
      forM_ (zip [0..] row) $ \(j, cell) ->
        display (show i) "," (show j) ":" (val cell .>>. 16)
    finish

-- Main function
main :: IO ()
main = netlist (top 100000 64 64) >>= writeCXXWith params
  where
    params = (defaultCXXGenParams "/tmp/heat") { numThreads = 2 }
