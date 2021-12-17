import Blarney
import Blarney.Either
import Blarney.Option
import System.Environment

type Foo = Option (Bit 8) :|: Bit 4

top :: Module ()
top = do
  always do
    let foo :: Foo = makeLeft (some 100)
    display "[foo] isLeft: " foo.isLeft
            " isRight: " foo.isRight
            " left: " foo.left
            " size: " (sizeOf foo)
    let bar :: Foo = makeRight 15
    display "[bar] isLeft: " bar.isLeft
            " isRight: " bar.isRight
            " right: " bar.right
    finish

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "Either" "Either-Verilog/"
