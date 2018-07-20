// For simulation

module FPDiv32#(parameter tokenWidth = 1) (
  input wire clock,                    // Clock
  input wire [tokenWidth-1:0] in_0,    // "go" trigger
  input wire [31:0] in_1,              // Operand A
  input wire [31:0] in_2,              // Operand B
  output wire [tokenWidth-1:0] out_0,  // "done" trigger
  output wire [31:0] out_1             // Result
  );

localparam latency = 14;

integer i;
reg [tokenWidth-1:0] trigger [latency-1:0];
reg [31:0] result [latency-1:0];
assign out_0 = trigger[0];
assign out_1 = result[0];
always @(posedge clock) begin
  trigger[latency-1] <= in_0;
  result[latency-1] <= $c("FPDiv32(", in_1, ",", in_2, ")");
  for (i=1; i < latency; i=i+1) begin
    trigger[i-1] <= trigger[i];
    result[i-1] <= result[i];
  end
end

endmodule
