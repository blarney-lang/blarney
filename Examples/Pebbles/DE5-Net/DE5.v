// Copyright (c) 2017 Matthew Naylor
// Copyright (c) 2016-2017 Alex Forencich
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

module DE5(
  input wire [3:0] BUTTON,

  output wire CLOCK_SCL,
  inout wire CLOCK_SDA,

  input wire CPU_RESET_n,

  input wire OSC_50_B3B,
  input wire OSC_50_B3D,
  input wire OSC_50_B4A,
  input wire OSC_50_B4D,
  input wire OSC_50_B7A,
  input wire OSC_50_B7D,
  input wire OSC_50_B8A,
  input wire OSC_50_B8D,

  output wire [6:0] HEX0_D,
  output wire HEX0_DP,

  output wire [6:0] HEX1_D,
  output wire HEX1_DP,

  output wire [3:0] LED,
  input wire [3:0] SW
);

  SoC soc (
    .clk_clk       (OSC_50_B7A),
    .reset_reset_n (CPU_RESET_n)
  );

endmodule 
