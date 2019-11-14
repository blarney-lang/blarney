import Blarney
import Blarney.SourceSink
import Blarney.Vector as V

makeElement :: KnownNat n => Integer -> Module (Source (Bit n))
makeElement i = do
  reg :: Reg (Bit n) <- withNewName ("element"++show i) (makeReg $ fromInteger i)
  let src :: Source (Bit n) = Source { canPeek = true
                                     , peek    = reg.val
                                     , consume = reg <== (reg.val) + 1
                                     }
  return $ debugSource src (fshow $ "src"++show i)

testVecModule :: Module (Vec 4 (Source (Bit 3)))
testVecModule = genWithM makeElement

testVecReg :: Module (Vec 4 (Reg (Bit 3)))
testVecReg = genWithM (makeReg `o` fromInteger)

top :: Module ()
top = do
  -- check zipAny type errors
  let a :: Vec 4 () = newVec
  let b :: Vec 5 () = newVec
  let c :: Vec 6 () = newVec
  let x :: Vec 4 _ = zipWithAny3 (\x y z -> ()) a b c
  -- check instance of Vector of Sources
  cycleCount :: Reg (Bit 4) <- makeReg 0
  srcs <- testVecModule
  always do
    -- increment cycle count
    cycleCount <== cycleCount.val + 1
    -- consume from each source on each cycle
    forM_ [0..3] \i -> do
      (srcs V.! i).consume
    -- terminate simulation when count reaches 10
    when (cycleCount.val .==. 10) do
      display "Finished"
      finish

main :: IO ()
main = do
  writeVerilogModule testVecModule "testVecModule" "Vectors-Verilog/"
  writeVerilogModule testVecReg "testVecReg" "Vectors-Verilog/"
  writeVerilogTop top "top" "Vectors-Verilog/"
