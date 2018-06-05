import Blarney

fact :: RTL ()
fact = do
  -- State
  n   :: Reg (Bit 32) <- makeRegInit 0
  acc :: Reg (Bit 32) <- makeRegInit 0

  -- Compute factorial of 10
  let recipe =
        Seq [
          RTL $ do
            n <== 10
        , While (n.val .>. 0) $ RTL $ do
            n <== n.val - 1
            acc <== acc.val + n.val
        , RTL $ do
            display "fact(10) = " (acc.val)
            finish
        ]
       
  runOnce recipe

main :: IO ()
main = generateCXX fact "/tmp/fact"
