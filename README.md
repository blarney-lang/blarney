# Blarney

Blarney is a Haskell library for hardware description that builds a
range of HDL features on top of a small set of core circuit
primitives.  It can be viewed as a modern variant of
[Lava](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.110.5587&rep=rep1&type=pdf)
that supports a variety of hardware description styles.  Blarney
requires GHC 8.4.1 or later.

## Example 1: Two-sort

Sorting makes for a good introduction to the library.  Let's start
with perhaps the simplest kind of sorter possible: one that sorts just
two inputs.  Given a pair of 8-bit values, the function `twoSort`
returns the sorted pair.

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

As well as generating C++ for simulation, Blarney suppoerts generation
of synthesisable Verilog.  Instead of calling `generateCXX` we simply
call `generateVerilog`.

```hs
main :: IO ()
main = generateVerilog top "/tmp/twoSort.v"
```

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

## Example 3: Bit-width polymorphism

For simplicity, we've made our sorter specific to lists of 8-bit
values.  We can make it generic to any bit-width by redefining
`twoSort` as

```hs
twoSort :: KnownNat n => (Bit n, Bit n) -> (Bit n, Bit n)
twoSort (a, b) = a .<. b ? ((a, b), (b, a))
```

The `KnownNat n` constraint is Haskell's way of saying that it must be
possible to convert the type-level number `n` to a value-level number.
This constraint appears almost any time a bit-vector with polymorphic
width occurs.  And since it is satisfiable for any type-level number,
it doesn't really carry any useful information.  Therefore, we
recommend the use of *partial type signatures* in GHC to avoid having
to write `KnownNat` constraints:

```hs
twoSort :: _ => (Bit n, Bit n) -> (Bit n, Bit n)
twoSort (a, b) = a .<. b ? ((a, b), (b, a))
```

## Example 4: Basic RTL

So far, we've only seen the `display` and `finish` actions of the
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

## Example 5: Queues

Queues (also known as FIFOs) are very commonly used abstraction in
hardware design.  Blarney provides a wide range of different queue
implementations, all of which implement the following interface.

```hs
-- Queue interface
data Queue a =
  Queue {
    notEmpty :: Bit 1        -- Is the queue non-empty?
  , notFull  :: Bit 1        -- Is there any space in the queue?
  , enq      :: a -> RTL ()  -- Insert an element
  , deq      :: RTL ()       -- Remove the first element
  , canDeq   :: Bit 1        -- Guard on the deq and first methods
  , first    :: a            -- View the first element
  }
```

The type `Queue a` represents a queue holding elements of type `a`,
and provides a range of standard functions on queues.  The `enq`
method should only be called when `notFull` is true and the `deq`
method should only be called when `canDeq` is true.  Similarly, the
`first` element of the queue is only valid when `canDeq` is true.
Below, we demonstrate the simplest possible implementation of a
one-element queue.

```hs
-- Simple one-element queue implementation
makeSimpleQueue :: Bits a => RTL (Queue a)
makeSimpleQueue = do
  -- Register holding the one element
  reg :: Reg a <- makeReg

  -- Register defining whether or not queue is full
  full :: Reg (Bit 1) <- makeRegInit 0

  -- Methods
  let notFull  = full.val .==. 0
  let notEmpty = full.val .==. 1
  let enq a    = do reg <== a
                    full <== 1
  let deq      = full <== 0
  let canDeq   = full.val .==. 1
  let first    = reg.val

  -- Return interface
  return (Queue notEmpty notFull enq deq canDeq first)
```

The following simple test bench illustrates how to use a queue.

```hs
-- Small test bench for queues
top :: RTL ()
top = do
  -- Instantiate a queue if 8-bit values
  queue :: Queue (Bit 8) <- makeSimpleQueue

  -- Create an 8-bit count register
  count :: Reg (Bit 8) <- makeRegInit 0
  count <== count.val + 1

  -- Writer side
  when (queue.notFull) $ do
    enq queue (count.val)
    display "Enqueued " (count.val)

  -- Reader side
  when (queue.canDeq) $ do
    deq queue
    display "Dequeued " (queue.first)

  -- Terminate after 100 cycles
  when (count.val .==. 100) finish
```

## Example 6: Wires

*Wires* are a feature of the RTL monad that offer a way for separate
RTL blocks to communicate *within the same clock cycle*.  Whereas
assignment to a register becomes visible on the clock cycle after the
assigment occurs, assignment to a wire is visible on the same cycle as
the assignment.  If no assignment is made to a wire on a particular
cycle, then the wire emits its *default value* on that cycle.  When
multiple assignments to the same wire occur on the same cycle, the
wire emits the bitwise disjunction of all the assigned values.

To illustrate, let's implement an *n*-bit counter module that supports
increment and decerement operations.

```hs
-- Interface for a n-bit counter
data Counter n =
  Counter {
    inc    :: RTL ()
  , dec    :: RTL ()
  , output :: Bit n
  }
```

We'd like the counter to support *parallel calls* to `inc` and `dec`.
That is, if `inc` and `dec` are called on the same cycle then the
counter's `output` is unchanged.  We'll achieve this using wires.

```hs
makeCounter :: _ => RTL (Counter n)
makeCounter = do
  -- State
  count :: Reg (Bit n) <- makeRegInit 0

  -- Wires
  incWire :: Wire (Bit 1) <- makeWireDefault 0
  decWire :: Wire (Bit 1) <- makeWireDefault 0

  -- Increment
  when (incWire.val .&. decWire.val.inv) $ do
    count <== count.val + 1

  -- Decrement
  when (incWire.val.inv .&. decWire.val) $ do
    count <== count.val - 1

  -- Interface
  let inc    = incWire <== 1
  let dec    = decWire <== 1
  let output = count.val

  return (Counter inc dec output)
```

## Example 7: Recipes

State machines are a very common way of defining the control-path of a
hardware core.  They are often written by case-switching on the
current state and manually setting the next state.  Quite often
however, they can be expressed more neatly as a `Recipe`.

```hs
data Recipe = 
    Skip                   -- Do nothing (in zero cycles)
  | Tick                   -- Do nothing (in one cycle)
  | Block (RTL ())         -- Perform RTL block (in one cycle)
  | Seq [Recipe]           -- Execute recipes in sequence
  | Par [Recipe]           -- Fork-join parallelism
  | If (Bit 1) Recipe      -- Conditional recipe
  | While (Bit 1) Recipe   -- Loop
```

To illustrate, here is a small state machine that computes the
factorial of 10.

```hs
fact :: RTL ()
fact = do
  -- State
  n   :: Reg (Bit 32) <- makeRegInit 0
  acc :: Reg (Bit 32) <- makeRegInit 0

  -- Compute factorial of 10
  let recipe =
        Seq [
          Block $ do
            n <== 10
        , While (n.val .>. 0) $ Block $ do
            n <== n.val - 1
            acc <== acc.val + n.val
        , Block $ do
            display "fact(10) = " (acc.val)
            finish
        ]
       
  runOnce recipe 
```

Blarney provides a very lightweight compiler for the `Recipe`
language (under 100 lines of code), which we invoke above through the
call to `runOnce`.

A very commoon use of recipes is to define test sequences.  For
example, here is a sample test sequence for the `Counter` module
defined earlier.

```hs
-- Test-bench for a counter
top :: RTL ()
top = do
  -- Instantiate an 4-bit counter
  counter :: Counter 4 <- mkCounter

  -- Sample test sequence
  test =
    Seq [
      Block $ do
        counter.inc
    , Block $ do
        counter.inc
    , Block $ do
        counter.inc
        counter.dec
    , Block $ do
        display "counter = " (counter.output)
        finish
    ]

  runOnce test
```

Here, we increment `counter` on the first cycle, and then again on the
second cycle.  On the third cycle, we both increment and decrement it
in parallel.  On the fourth cycle, we display the value and terminate
the simulator.

## More to come!
