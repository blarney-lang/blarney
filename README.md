# Blarney

Blarney is a Haskell library for hardware description that builds a
range of HDL abstractions on top of a small set of core circuit
primitives.  It can be viewed as a modern variant of
[Lava](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.110.5587&rep=rep1&type=pdf)
that supports a variety of hardware description styles.  Blarney
requires GHC 8.4.1 or later.

## Contents

Examples:

* [Example 1: Two-sort](#example-1-two-sort)
* [Example 2: Bubble sort](#example-2-bubble-sort)
* [Example 3: Bit-width polymorphism](#example-3-bit-width-polymorphism)
* [Example 4: Basic RTL](#example-4-basic-rtl)
* [Example 5: Queues](#example-5-queues)
* [Example 6: Wires](#example-6-wires)
* [Example 7: Recipes](#example-7-recipes)
* [Example 8: Block RAMs](#example-8-block-rams)
* [Example 9: Streams](#example-9-streams)
* [Example 10: Bit-string pattern matching](#example-10-bit-string-pattern-matching)
* [Example 11: Tiny 8-bit CPU](#example-11-tiny-8-bit-cpu)

APIs:

* [API 1: Bit vectors](#api-1-bit-vectors)
* [API 2: Bit selection](#api-2-bit-selection)
* [API 3: Bits class](#api-3-bits-class)
* [API 4: FShow class](#api-4-fshow-class)
* [API 5: Prelude](#api-5-prelude)
* [API 6: RTL](#api-6-rtl)
* [API 7: Queue library](#api-7-queue-library)

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

This definition makes use of two Blarney functions.  The first is the
unsigned comparison operator

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
again -- akin to a type class for binary serialisation.  (See
[here](#api-3-bits-class) for details about the `Bits` class.)

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

As well as generating C++ for simulation, Blarney supports generation
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

Check out [The design and verification of a sorter
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
This constraint often appears when a bit-vector with polymorphic
width is used.  It is satisfiable for any type-level number, so
doesn't really carry any useful information.  Therefore, we
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
hardware design.  Blarney provides [a range of different queue
implementations](), all of which implement the following interface.

```hs
-- Queue interface
data Queue a =
  Queue {
    notEmpty :: Bit 1        -- Is the queue non-empty?
  , notFull  :: Bit 1        -- Is there any space in the queue?
  , enq      :: a -> RTL ()  -- Insert an element (assuming notFull)
  , deq      :: RTL ()       -- Remove the first element (assuming canDeq)
  , canDeq   :: Bit 1        -- Guard on the deq and first methods
  , first    :: a            -- View the first element (assuming canDeq)
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
  -- Instantiate a queue of 8-bit values
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

State machines are a common way of defining the control-path of a
circuit.  They are typically expressed by doing case-analysis of the
current-state and manually setting the next-state.  Quite often
however, they can be expressed more neatly as a `Recipe` -- a simple
imperative language with various control-flow statements.

```hs
data Recipe = 
    Skip                   -- Do nothing (in zero cycles)
  | Tick                   -- Do nothing (in one cycle)
  | RTL (RTL ())           -- Perform RTL block (in one cycle)
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
```

Blarney provides a lightweight compiler for the `Recipe` language
(under 100 lines of code), which we invoke above through the call to
`runOnce`.

A very common use of recipes is to define test sequences.  For
example, here is a simple test sequence for the `Counter` module
defined earlier.

```hs
-- Test-bench for a counter
top :: RTL ()
top = do
  -- Instantiate an 4-bit counter
  counter :: Counter 4 <- makeCounter

  -- Sample test sequence
  let test =
        Seq [
          RTL $ do
            counter.inc
        , RTL $ do
            counter.inc
        , RTL $ do
            counter.inc
            counter.dec
        , RTL $ do
            display "counter = " (counter.output)
            finish
        ]

  runOnce test
```

Here, we increment `counter` on the first cycle, and then again on the
second.  On the third cycle, we both increment and decrement it in
parallel.  On the fourth cycle, we display the value and terminate the
simulator.

## Example 8: Block RAMs

Blarney provides [a variety of block RAM modules]() commonly supported
on FPGAs.  They are all based around the following interface.

```hs
-- Block RAM interface
data RAM a d =
  RAM {
    load    :: a -> RTL ()
  , store   :: a -> d -> RTL ()
  , out     :: d
  }
```

When a `load` is issued for a given address, the value at that address
appears on `out` on the next clock cycle.  When a `store` is issued,
the value is written to the RAM on the current cycle, and the written
value appears on `out` on the next cycle.  A `load` and `store` to the
same `RAM` interface should not be issued on the same cycle.  To
illustrate, here is a test bench that creates a block RAM and performs
a `store` followed by a `load`.

```hs
top :: RTL ()
top = do
  -- Instantiate a 256 element RAM of 5-bit values
  ram :: RAM (Bit 8) (Bit 5) <- makeRAM

  -- Write 10 to ram[0] and read it back again
  let test =
        Seq [
          RTL $ do
            store ram 0 10
        , RTL $ do
            load ram 0
        , RTL $ do
            display "Got " (ram.out)
            finish
        ]

  runOnce test
```

Somewhat-related to block RAMs are *register files*.  The difference
is that a register file allows the value at an address to be
determined *within* a clock cycle.  It also allows any number of reads
and writes to be performed within the same cycle.  Register files have
the following interface.

```hs
data RegFile a d =
  RegFile {
    (!)    :: a -> d              -- Read
  , update :: a -> d -> RTL ()    -- Write
  }
```

Unlike block RAMs, register files (especially large ones) do not
always map efficiently onto hardware, so use with care!

## Example 9: Streams

Streams are another commonly-used abstraction in hardware description.
They are often used to implement hardware modules that consume data at
a *variable rate*, depending on internal details of the module that
the implementer does not wish to (or is unable to) expose.  In
Blarney, streams are captured by the following interface.

```hs
type Stream a = Get a

data Get a =
  Get {
    get    :: RTL ()
  , canGet :: Bit 1
  , value  :: a
  }
```

Streams are closely related to queues.  Indeed, any queue can be
converted to a stream:

```hs
-- Convert a queue to a stream
toStream :: Queue a -> Stream a
toStream q =
  Get {
    get    = q.deq
  , canGet = q.canDeq
  , value  = q.first
  }
```

As an example, here's a higher-order stream combinator that combines
two streams into one using a given binary function.

```hs
zipWithS :: (Bits a, Bits b, Bits c)
         => (a -> b -> c)
         -> Stream a -> Stream b -> RTL (Stream c)
ripWithS f xs ys = do
  -- Output buffer
  buffer :: Queue c <- makeQueue

  when (xs.canGet .&. ys.canGet .&. buffer.notFull) $ do
    xs.get
    ys.get
    enq buffer (f (xs.value) (ys.value))

  return (buffer.toStream)
```

## Example 10: Bit-string pattern matching

Recent work on specifying and implementing ISAs led us to develop two
libraries for doing bit-string pattern matching.  The first, `BitPat`,
is based on the paper [Type-safe pattern
combinators](https://core.ac.uk/download/pdf/50525461.pdf) and let's
us define an instruction decoder for a tiny subset of RISC-V as
follows.

```hs
import Blarney.BitPat

-- Semantics of add instruction
add :: Bit 5 -> Bit 5 -> Bit 5 -> RTL ()
add rs2 rs1 rd =
  display "add r" (rd.val) ", r" (rs1.val) ", r" (rs1.val)

-- Semantics of addi instruction
addi :: Bit 12 -> Bit 5 -> Bit 5 -> RTL ()
addi imm rs1 rd =
  display "add r" (rd.val) ", r" (rs1.val) ", " (imm.val)

-- Instruction dispatch
top :: RTL ()
top = do
  -- Sample RISC-V add instruction
  let instr :: Bit 32 = 0b00000000000100010000000110110011

  -- Dispatch
  match instr
    [
      Lit(7,0) <#> Var(5) <#> Var(5) <#> Lit(3,0) <#> Var(5) <#> Lit(7,0b0110011) ==> add,
      Var(12)             <#> Var(5) <#> Lit(3,0) <#> Var(5) <#> Lit(7,0b0010011) ==> addi
    ]
```

Here, the `match` function represents a case statement with a case
subject and a list of case alternatives.  Each case alternative
contains a pattern on the left-hand-side and a function on the
right-hand-side.  In a pattern, the macro `Lit(n,x)` matches an
`n`-bit string containing the value `x`, whereas `Var(n)` matches
*any* `n`-bit string and passes it as an argument to the
right-hand-side function.  The `<#>` combinator composes patterns
sequentially.  To use the `Lit` and `Var` macros, one needs to
`#include <BitPat.h>`.

The `BitPat` library provides strong static typing.  For example, if
the type signatures of `add` and `addi` are omitted, then they will be
inferred by the type checker.

Our second library, `BitScan`, is more powerfull but at the cost of
being dynamically typed rather than statically typed.  This means we
loose type-inference of the right-hand-side function, and any width
error will be displayed at circuit-generation time rather than
compile-time.  In addition, the error message may not be as helpful as
the corresponding one provided by `BitPat`.  To illustrate `BitScan`,
let's add the RISC-V `sw` (store-word) instruction to our decoder:

```hs
import Blarney.BitScan

-- Semantics of store-word instruciton
sw :: Bit 12 -> Bit 5 -> Bit 5 -> RTL ()
sw imm rs2 rs1 = display "sw " rs2 ", " rs1 "[" imm "]"

top :: RTL ()
top = do
  -- Sample RISC-V store-word instruction
  let instr :: Bit 32 = 0b10000000000100010010000010100011

  -- Dispatch
  match instr
    [
      "0000000   rs2[4:0]  rs1[4:0] 000 rd[4:0]  0110011" ==> add,
      "          imm[11:0] rs1[4:0] 000 rd[4:0]  0010011" ==> addi,
      "imm[11:5] rs2[4:0]  rs1[4:0] 010 imm[4:0] 0100011" ==> sw
    ]
```

The nice thing about this decoder is that the *scattered immediate*
field `imm` in the `sw` instruction is automatically assembled by the
library.  That is, the `imm[11:5]` part of the immediate is combined
with the `imm[4:0]` part to give the final 12-bit immediate value
passed to the right-hand-side function.  Scattered immediates appear
*a lot* in the RISC-V specification.  Thanks to Jon Woodruff for
suggesting this feature!

## Example 11: Tiny 8-bit CPU

As a way of briging together a number of the ideas introduced above,
let's define a very simple, 8-bit CPU with the following ISA.

  Opcode     | Meaning
  ---------- | ---------
  `ZZNNNN00` | Write value `0000NNNN` to register `ZZ`
  `ZZXXYY01` | Add register `XX` to register `YY` and store in register `ZZ`
  `NNNNYY10` | Branch back by `NNNN` instructions if register `YY` is non-zero
  `NNNNNN11` | Halt

To begin, let's consider a non-pipelined implementation of this ISA,
which has a CPI (cycles-per-instruction) of two: one cycle is used to
fetch the next instruction, and one cycle is used to execute it.  The
CPU will execute the program defined in the file `instrs.hex`.

```hs
-- Instructions
type Instr = Bit 8

-- Register identifiers
type RegId = Bit 2

-- Tiny 8-bit CPU
makeCPU :: RTL ()
makeCPU = do
  -- Instruction memory (containing 256 instructions)
  instrMem :: RAM (Bit 8) Instr <- makeRAMInit "instrs.hex"

  -- Register file (containing 4 registers)
  regFile :: RegFile RegId (Bit 8) <- makeRegFile

  -- Program counter
  pc :: Reg (Bit 8) <- makeRegInit 0

  -- Are we fetching (1) or executing (0)?
  fetch :: Reg (Bit 1) <- makeRegInit 1

  -- Load immediate instruction
  let li rd imm = do
        update regFile rd (zeroExtend imm)
        pc <== pc.val + 1
        display "rf[" rd "] := " imm

  -- Add instruction
  let add rd rs0 rs1 = do
        let sum = regFile!rs0 + regFile!rs1
        update regFile rd sum
        pc <== pc.val + 1
        display "rf[" rd "] := " sum

  -- Branch instruction
  let bnz offset rs = do
        if regFile!rs .==. 0
          then pc <== pc.val + 1
          else pc <== pc.val - zeroExtend offset

  -- Halt instruction
  let halt imm = finish

  -- Fetch
  when (fetch.val) $ do
    load instrMem (pc.val)
    fetch <== 0

  -- Execute
  when (fetch.val.inv) $ do
    match (instrMem.out)
      [
        Var(2) <#> Var(4)            <#> Lit(2,0b00) ==> li,
        Var(2) <#> Var(2) <#> Var(2) <#> Lit(2,0b01) ==> add,
        Var(4) <#>            Var(2) <#> Lit(2,0b10) ==> bnz,
        Var(6) <#>                       Lit(2,0b11) ==> halt
      ]
    fetch <== 1
```

Let's now look at a 3-stage pipeline implemention of the same ISA that
has a CPI much closer to 1.  Unfortunately, it's a bit too long to
in-line the code listing here, so we provide a
[link](https://github.com/POETSII/blarney/blob/master/Examples/CPU/CPU.hs)
instead.  Although the ISA is very simple, it does contain a few
challenges for a pipelined implementation, namely *control hazards*
(due to the branch instruction) and *data hazards* (due to the add
instruction).  We resolve data hazards using *register forwarding* and
control hazards using a *pipeline flush*.

## API 1: Bit vectors

```hs
-- Bit-vector containing n bits
newtype Bit (n :: Nat)

-- Determine width of bit vector from type
widthOf :: KnownNat n => Bit n -> Int

-- Bit replication
replicateBit :: KnownNat n => Bit 1 -> Bit n   -- Replication
low          :: KnownNat n => Bit n            -- All 0's
high         :: KnownNat n => Bit n            -- All 1's

-- Bitwise operators
inv   :: Bit n -> Bit n            -- Bitwise not
(.&.) :: Bit n -> Bit n -> Bit n   -- Bitwise and
(.|.) :: Bit n -> Bit n -> Bit n   -- Bitwise or
(.^.) :: Bit n -> Bit n -> Bit n   -- Bitwise xor

-- Comparison operators
eq     :: Bit n -> Bit n -> Bit 1   -- Equality
neq    :: Bit n -> Bit n -> Bit 1   -- Disequality
(.<.)  :: Bit n -> Bit n -> Bit 1   -- Less than
(.<=.) :: Bit n -> Bit n -> Bit 1   -- Less than or equal
(.>.)  :: Bit n -> Bit n -> Bit 1   -- Greated than
(.>=.) :: Bit n -> Bit n -> Bit 1   -- Greater than or equal

-- Arithmetic operators
(.+.) :: Bit n -> Bit n -> Bit n    -- Addition
(.-.) :: Bit n -> Bit n -> Bit n    -- Subtraction
(.*.) :: Bit n -> Bit n -> Bit n    -- Multiplication

-- Num instance
instance KnownNat n => Num (Bit n) where
  (+)         :: Bit n -> Bit n -> Bit n  -- Addition
  (-)         :: Bit n -> Bit n -> Bit n  -- Subtraction
  (*)         :: Bit n -> Bit n -> Bit n  -- Multiplication
  negate      :: Bit n -> Bit n           -- Two's complement
  abs         :: Bit n -> Bit n           -- Identity function
  signum      :: Bit n -> Bit n           -- If > 0 then 1 else 0
  fromInteger :: Integer -> Bit n         -- Convert from integer

-- Fractional instance
instance KnownNat n => Fractional (Bit n) where
  (/) :: Bit n -> Bit n -> Bit n   -- Division

-- Modulus division
(%) :: Bit n -> Bit n -> Bit n

-- Muxing and shifting
mux       :: Bit 1 -> Bit n -> Bit n -> Bit n   -- Multiplexer
(.<<.)    :: Bit n -> Bit n -> Bit n            -- Shift left
(.>>.)    :: Bit n -> Bit n -> Bit n            -- Shift right

-- Population count
countOnes :: Bit n -> Bit (Log2 n + 1)

-- Bit-vector resizing
(#)        :: Bit n -> Bit m -> Bit (n+m)                -- Concatenation
zeroExtend :: (KnownNat m, n <= m) => Bit n -> Bit m     -- Zero extension
signExtend :: (KnownNat m, n <= m) => Bit n -> Bit m     -- Sign extension
upper      :: (KnownNat m, m <= n) => Bit n -> Bit m     -- Extract MSBs
lower      :: (KnownNat m, m <= n) => Bit n -> Bit m     -- Extract LSBs
split      :: KnownNat n => Bit (n+m) -> (Bit n, Bit m)  -- Split bit vector

-- I/O
input :: KnownNat n => String -> Bit n  -- External input

-- Registers
reg   :: Bit n -> Bit n -> Bit n            -- Plain register
regEn :: Bit n -> Bit 1 -> Bit n -> Bit n   -- Register with write-enable
```

## API 2: Bit selection

Blarney provides the following untyped bit-selection functions, i.e.
where the selection indices are values rather than types, meaning the
width mismatches will not be caught by the type checker, but by a
(probably unhelpful) error-message at circuit-generation time.

```hs
-- Untyped bit selection
getBit :: Int -> Bit n -> Bit 1

-- Untyped sub-range selection
getBits :: KnownNat m => (Int, Int) -> Bit n -> Bit m
```

We recommended the following type-safe bit-selection *macros* instead:

  * Macro `bit(i)`, where `i` is a type-level number, expands to a function
that maps a given bit-vector to the bit at index `i` of that vector.

  * Macro `bits(hi,lo)`, where `hi` and `lo` are type-level numbers, expands
to  a function that maps a given bit-vector to bits `hi` down to `lo`
of that vector.

To illustrate, here's an example of using the `bits(hi,lo)` macro to
select the upper four bits of a byte.

```hs
-- Extract upper 4 bits of a byte
upperNibble :: Bit 8 -> Bit 4
upperNibble x = x.bits(7,4)
```

We use macros here for purely syntacic reasons: passing types to
functions in Haskell is a bit clunky.  

## API 3: Bits class

Any type in the `Bits` class can be represented in hardware, e.g.
stored in a wire, a register, or a RAM.

```hs
class Bits a where
  type SizeOf a :: Nat
  replicateBit  :: Bit 1 -> a
  sizeOf        :: a -> Int
  pack          :: a -> Bit (SizeOf a)
  unpack        :: Bit (SizeOf a) -> a
```

The `Bits` class supports *generic deriving*.  For example, suppose
we have a simple data type for memory requests:

```hs
data MemReq =
  MemReq {
    memOp   :: Bit 1    -- Is it a load or a store request?
  , memAddr :: Bit 32   -- 32-bit address
  , memData :: Bit 32   -- 32-bit data for stores
  }
  deriving (Generic, Bits)
```

To make this type a member of the `Bits` class, we have suffixed it
with `derving (Generic, Bits)`.  The generic deriving mechanism for
`Bits` does not support *sum types* (there is no way to convert a
bit-vector to a sum type using the circuit primitives provided
Blarney).

## API 4: FShow class

Any type in the `FShow` class can be passed as an argument to the
`display` function.

```hs
class FShow a where
  fshow     :: a -> Format
  fshowList :: [a] -> Format     -- Has default definition

-- Abstract data type for things that can be displayed
newtype Format

-- Format constructors
mempty :: Format                         -- Empty (from Monoid class)
(<>)   :: Format -> Format -> Format     -- Append (from Monoid class)

-- Primitive instances
instance FShow Char
instance FShow (Bit n)
```

As an example, here is how the `FShow` instance for pairs is defined.

```hs
-- Example instance: displaying pairs
instance (FShow a, FShow b) => FShow (a, b) where
  fshow (a, b) = fshow "(" <> fshow a <> fshow "," <> fshow b <> fshow ")"
```

Like the `Bits` class, the `FShow` class supports *generic deriving*:
just include `FShow` in the `deriving` clause for the data type.

## API 5: Prelude

```hs
-- Equality
(.==.) :: Bits a => a -> a -> Bit 1

-- Disequality
(.!=.) :: Bits a => a -> a -> Bit 1

-- Ternary conditional operator
(?) :: Bits a => Bit 1 -> (a, a) -> a

-- One-hot select
select :: Bits a => [(Bit 1, a)] -> a

-- Index a list
index :: (KnownNat n, Bits a) => Bit n -> [a] -> a

-- All 0's
zero :: Bits a => a

-- All 1's
ones :: Bits a => a

-- Register
register   :: Bits a => a -> a -> a
registerEn :: Bits a => a -> Bit 1 -> a -> a

-- RAM primitives
-- (Read new data on write)
ram     :: (Bits a, Bits d) => (a, d, Bit 1) -> d
ramInit :: (Bits a, Bits d) => String -> (a, d, Bit 1) -> d

-- True dual-port RAM primitives
-- (Reads new data on write)
-- (When read-address == write-address on different ports, read old data)
ramTrueDual     :: (Bits a, Bits d) =>
                   (a, d, Bit 1)
                -> (a, d, Bit 1)
                -> (d, d)
ramTrueDualInit :: (Bits a, Bits d) =>
                   String 
                -> (a, d, Bit 1)
                -> (a, d, Bit 1)
                -> (d, d)

-- Adder tree
sumList :: KnownNat n => [Bit n] -> Bit n

-- Tree of bitwise-and
andList :: Bits a => [a] -> a

-- Tree of bitwise-or
orList :: Bits a => [a] -> a
```

## API 6: RTL

```hs
-- The RTL monad
newtype RTL a

-- Register variables
data Reg a

-- Wire variables
data Wire a

-- Mutable variables
-- (Reg and Wire are both instances of this)
class Var v where
  val :: Bits a => v a -> a
  (<==) :: Bits a => v a -> a -> RTL ()

-- Create register
makeReg     :: Bits a => RTL (Reg a)
makeRegInit :: Bits a => a -> RTL (Reg a)

-- Create wire
makeWire        :: Bits a => RTL (Wire a)
makeWireDefault :: Bits a => a -> RTL (Wire a)

-- A DReg holds the assigned value only for one cycle.
-- At all other times, it has the given default value.
makeDReg :: Bits a => a -> RTL (Reg a)

-- Extra wire functions
val'    :: Wire a -> a       -- Registered output of wire
active  :: Wire a -> Bit 1   -- Is wire being assigned on this cycle?
active' :: Wire a -> Bit 1   -- Registed value of active

-- RTL conditionals
when    :: Bit 1 -> RTL () -> RTL ()
whenNot :: Bit 1 -> RTL () -> RTL ()

-- For rebindable syntax
-- (Haskell's if-then-else maps to this)
ifThenElseRTL :: Bit 1 -> RTL () -> RTL () -> RTL ()

-- RTL switch statement
switch :: Bits a => a -> [(a, RTL ())] -> RTL ()

-- For neat construction of case alternatives
(-->)  :: a -> RTL () -> (a, RTL ())

-- RTL finish statements
finish :: RTL ()

-- RTL display statements
-- (Variadic function: args in FShow, result of type RTL ())
display :: Displayable a => a

-- Declare external output
output :: String -> Bit n -> RTL ()

-- Convert RTL monad to a netlist
netlist :: RTL () -> IO [Net]
```

## API 7: Queue library

The `Blarney.Queue` module defines several queue
implementations, all implementing the interface:

```hs
data Queue a =
  Queue {
    notEmpty :: Bit 1        -- Is the queue non-empty?
  , notFull  :: Bit 1        -- Is there any space in the queue?
  , enq      :: a -> RTL ()  -- Insert an element (assuming notFull)
  , deq      :: RTL ()       -- Remove the first element (assuming canDeq)
  , canDeq   :: Bit 1        -- Guard on the deq and first methods
  , first    :: a            -- View the first element (assuming canDeq)
  }
```

**Two-element queue**:

  * A full-throughput queue implemented using 2 registers.
  * No combinatorial paths between sides.
  * There's a mux on the enqueue path.

```hs
makeQueue :: Bits a => RTL (Queue a)
```

**Sized queue**:

  * A full-throughput N-element queue implemented using 2 registers and a RAM.
  * No combinatorial paths between sides.
  * There's a mux on the  enqueue path.

```hs
makeSizedQueue :: Bits a => Int -> RTL (Queue a)
```

**Shift queue**:

  * N-element queue implemented using a shift register.
  * No muxes.
  * Input element goes straight to a register and output element
    comes straight from a register.
  * N-cycle latency between enqueuing an element and being able to dequeue
    it, where N is the queue capacity.

```hs
-- Full throughput but there's a combinatorial path between notFull and deq.
makePipelineQueue :: Bits a => Int -> RTL (Queue a)

-- No combinatorial paths between sides but max throughput = N/(N+1),
-- where N is the queue capacity
makeShiftQueue :: Bits a => Int -> RTL (Queue a)
```
