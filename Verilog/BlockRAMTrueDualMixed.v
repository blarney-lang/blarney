// Mixed width true dual-port block RAM
// ====================================

module BlockRAMTrueDualMixed
  #(parameter int
    DATA_WIDTH_A = 1,
    ADDR_WIDTH_A = 1,
    ADDR_WIDTH_B = 1)
(
  input [ADDR_WIDTH_A-1:0] ADDR_A,
  input [ADDR_WIDTH_B-1:0] ADDR_B,
  input [DATA_WIDTH_A-1:0] DI_A, 
  input [DATA_WIDTH_A*(1<<(ADDR_WIDTH_A-ADDR_WIDTH_B))-1:0] DI_B, 
  input WE_A, WE_B, CLK,
  output reg [DATA_WIDTH_A-1:0] DO_A,
  output reg [DATA_WIDTH_A*(1<<(ADDR_WIDTH_A-ADDR_WIDTH_B))-1:0] DO_B);
    
  localparam RATIO = 1 << (ADDR_WIDTH_A - ADDR_WIDTH_B);
  localparam DATA_WIDTH_B = DATA_WIDTH_A * RATIO;
  localparam RAM_DEPTH = 1 << ADDR_WIDTH_B;

  reg [RATIO-1:0] [DATA_WIDTH_A-1:0] ram[0:RAM_DEPTH-1];
  reg [DATA_WIDTH_A-1:0] dataRegA;
  reg [DATA_WIDTH_B-1:0] dataRegB;

  // Port A
  always@(posedge CLK)
  begin
    if (WE_A) begin 
      ram[ADDR_A / RATIO][ADDR_A % RATIO] <= DI_A;
      dataRegA <= DI_A;
    end else begin 
      dataRegA <= ram[ADDR_A / RATIO][ADDR_A % RATIO];
    end
  end
  assign DO_A = dataRegA;

  // Port B
  always@(posedge CLK)
  begin
    if (WE_B) begin
      ram[ADDR_B] <= DI_B;
      dataRegB <= DI_B;
    end else begin
      dataRegB <= ram[ADDR_B];
    end
  end
  assign DO_B = dataRegB;
endmodule
