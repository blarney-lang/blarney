# Blarney

Blarney is a Haskell library for hardware description that builds a
range of HDL features on top of a small set of core circuit
primitives.  It can be viewed as a modern variant of
[Lava](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.110.5587&rep=rep1&type=pdf)
that supports a variety of hardware description styles.  Blarney
requires GHC 8.4.1 or later.

## Example 1: Two-sort

Sorting makes for a good introduction to Blarney.  Let's start with
perhaps the simplest kind of sorter possible: one that sorts just two
inputs.  Given a pair of 8-bit values, the function `twoSort` returns
the sorted pair.

```hs
import Blarney

twoSort :: (Bit 8, Bit 8) -> (Bit 8, Bit 8)
twoSort (a, b) = a .<. b ? ((a, b), (b, a))
```

This definition makes use of two [Blarney
primitives](a-blarney-primitives).  The first is the unsigned
comparison operator

```hs
(.<.) :: Bit n -> Bit n -> Bit 1
```

which is polymorphic in the width `n` of the bit-vectors being compared.
The second is the ternary conditional operator

```hs
(?) :: Bits a => Bit 1 -> (a, a) -> a
```

which is polymorphic in the return type `a`.  The constraint `Bits a`
requires that the type `a` can be converted to a bit-vector and back
again -- akin to a type class for binary serialisation.

To check that our circuit works, let's create a simple *test bench*
that applies some sample inputs to `twoSort` and displays the outputs.

```hs
top :: RTL ()
top = do
  display "twoSort (0x1,0x2) = " (twoSort (0x1,0x2))
  display "twoSort (0x2,0x1) = " (twoSort (0x2,0x1))
  finish
```

This test bench is written using Blarney's RTL (register-transfer
level) monad.  All statements in this monad are executed *in parallel*
and *on every clock cycle*.   To generate a cycle-accurate C++
simulator for this test bench, we write

```hs
main :: IO ()
main = generateCXX top "/tmp/twoSort/"
```

Assuming the above code is in a file named `Sorter.hs`, it can be
compiled at the command-line using

```sh
> blc Sorter.hs
```

where `blc` stands for *Blarney compiler*.  This is just a script that
invokes GHC with the appropriate compiler flags.  For it to work,
the `BLARNEY_ROOT` environment variable needs to be set to the root of
the repository, and `BLARNEY_ROOT/Scripts` must be in the `PATH`.
Running the resulting executable will generate a C++ simulator in
`/tmp/twoSort`, which can be built and run as follows.

```sh
> cd /tmp/twoSort
> make -j 4
> ./main
twoSort (0x1,0x2) = (0x1,0x2)
twoSort (0x2,0x1) = (0x1,0x2)
```

It looks like `twoSort` is working!

## Example 2: Bubble sort

We can build a general *N*-element sorter by connecting together
multiple two-sorters.  One of the simplest ways to do this is the
*bubble sort* network.  The key component of this network is a
function that takes a list of inputs and returns a new list in which
the smallest element comes first (the smallest element "bubbles" to
the front).

```hs
bubble :: [Bit 8] -> [Bit 8]
bubble [] = []
bubble [x] = [x]
bubble (x:y:rest) = bubble (small:rest) ++ [large]
  where (small, large) = twoSort (x, y)
```

If we repeatedly call `bubble` then we end up with a sorted list.

```hs
sort :: [Bit 8] -> [Bit 8]
sort [] = []
sort xs = smallest : sort rest
  where smallest:rest = bubble xs
```

Running the test bench

```hs
top :: RTL ()
top = do
  let inputs = [0x3, 0x4, 0x1, 0x0, 0x2]
  display "sort " inputs " = " (sort inputs)
  finish
```

in simulation yields:

```
sort [0x3,0x4,0x1,0x0,0x2] = [0x0,0x1,0x2,0x3,0x4]
```

To see that the `sort` function really is describing a circuit, let's
draw the circuit digram for a 5-element bubble sorter.

```
        -->.
           |
        -->+---.
           |   |
Inputs  -->+---+---.
           |   |   |
        -->+---+---+---.
           |   |   |   |
        -->+---+---+---+---.
           |   |   |   |   |

                Outputs
```

The input list is supplied on the left, and the sorted output list is
produced at the bottom.  Each `+` denotes a two-sorter that takes
inputs from the top and the left, and produces the smaller value to
the bottom and the larger value to the right.

Check out [The design and verification of a
sorter
core](https://pdfs.semanticscholar.org/de30/22efc5aec833d7b52bd4770a382fea729bba.pdf)
-- one of the classic Lava papers -- for a more in-depth exploration
of sorting circuits in Haskell.

## Example 3: Basic RTL

So far, we have only seen the `display` and `finish` actions of the
RTL monad.  The RTL monad also supports creation and assignment of
registers.  To illustrate, here is a piece of RTL that creates a
4-bit `cycleCount` register, increments it on each cycle, stopping
when it reaches 10.

```hs
top :: RTL ()
top = do
  -- Create a register
  cycleCount :: Reg (Bit 4) <- makeRegInit 0

  -- Increment on every cycle
  cycleCount <== cycleCount.val + 1

  -- Display value on every cycle
  display "cycleCount = " (cycleCount.val)

  -- Terminate simulation when count reaches 10
  when (cycleCount.val .==. 10) $ do
    display "Finished"
    finish
```

This example introduces a number of new library functions:
`makeRegInit` creates a register, initialised to the given value;
`val` returns the value of a register;
the `.` operator is defined by Blarney as *reverse function
application* rather than the usual *function composition*;
and `when` allows conditional RTL blocks to be introduced.  One
can also use `if`/`then`/`else` in an RTL context, thanks to Haskell's
rebindable syntax feature.

```hs
  -- Terminate simulation when count reaches 10
  if cycleCount.val .==. 10
    then do
      display "Finished"
      finish
    else
      display "Not finished"
```

Running `top` in simulation gives

```
cycleCount = 0x0
cycleCount = 0x1
cycleCount = 0x2
cycleCount = 0x3
cycleCount = 0x4
cycleCount = 0x5
cycleCount = 0x6
cycleCount = 0x7
cycleCount = 0x8
cycleCount = 0x9
cycleCount = 0xa
Finished
```
