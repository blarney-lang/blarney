{-# LANGUAGE UndecidableInstances #-}

import Blarney
import Blarney.Option
import Blarney.TaggedUnion
import System.Environment

type MyEither a b =
  TaggedUnion [
    "left"  ::: a
  , "right" ::: b
  ]

newtype NewEither a b =
  NewEither (
    TaggedUnion [
      "left"  ::: a
    , "right" ::: b
    ]
  )
  deriving newtype (IsTaggedUnion, Bits, FShow)

--type Foo = MyEither (Option (Bit 8)) (Bit 4)
type Foo = NewEither (Option (Bit 8)) (Bit 4)

top :: Module ()
top = do
  always do
    let foo :: Foo = tag #left (some 100)
    display "[foo] isLeft: " (foo `is` #left)
            " isRight: " (foo `is` #right)
            " left: " (untag #left foo)
            " size: " (sizeOf foo)
    let bar :: Foo = tag #right 15
    display "[bar] isLeft: " (bar `is` #left)
            " isRight: " (bar `is` #right)
            " right: " (untag #right bar)
    display "foo = " foo
    display "bar = " bar
    finish

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "Either" "Either-Verilog/"
