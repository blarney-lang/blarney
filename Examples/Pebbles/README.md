# Pebbles

Pebbles is a 5-stage 32-bit RISC-V core implemented in Blarney, aiming
for a high-level definition of the `RV32I` instruction set and
moderate performance.

The 5 pipeline stages are:

  1. Intruction Fetch
  2. Operand Fetch
  3. Operand Latch (also: pre-execute)
  4. Execute
  5. Writeback (also: post-execute)

The main feature of the Pebbles description is that it largely
separates the definition of the instruction-set architecture (ISA)
from the definition of the pipeline microarchitecture:

  * Instruction set: [Pebbles.hs](Pebbles.hs).
  * Pipeline: [Pipeline.hs](Pipeline.hs).

This is achieved by parameterising the pipeline by:

  * Three functions to extract the two source registers and the 
    destination register from a three-operand instruction.
  * Three sets of (pattern, rule) pairs that are applied to the
    instruction in the pre-execute, execute, and post-execute stages.

Each rule is passed the pipeline state, which is used by the rule to
communicate with the pipeline.  This state contains the following
fields:

  * `opA` and `opB`, available in all stages.  These contain the
    values of the two source registers.
  * `result`, available in execute and pre-execute.  Writing to this
    modifies the destination register.
  * `pc`, available in all stages.  This may be read, to obtain the
    program counter of the currently executing instruciton.  It may be
    written, to modify the program counter.
    If unwritten, the pipeline implicity updates the program counter
    to point to the next instruction in memory.
  * `late`, available in the pre-execute stage only.  Pulsing this
    wire tells the pipeline that the result of the instruction will be
    determined *late*, in the post-execute stage rather than the execute
    stage.

The pipeline automatically takes care of all data and control hazards,
with register forwarding and branch prediction.

The pipeline is intended to be fairly ISA agnostic.  For example, it
might be possible to define another 3-operand register machine, such
as MIPS, using more-or-less the same pipeline.

The performance of the core is not bad, considering the high-level
description: on a Stratix 5 FPGA, it uses under 800 ALMs and clocks at
over 230MHz.

The core includes a built-in UART interface, accessible via CSRs.  For
an example program, take a look at the [boot loader](Boot/main.c),
i.e.  the program that resides in instruction memory at startup --
this particular boot loader doesn't do much boot loading yet.
