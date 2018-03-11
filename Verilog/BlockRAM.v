// Single-port block RAM
// =====================

module BlockRAM (
  CLK,     // Clock
  DI,      // Data in
  ADDR,    // Read address
  WE,      // Write enable
  DO       // Data out
  );

  parameter ADDR_WIDTH = 1;
  parameter DATA_WIDTH = 1;
  parameter INIT_FILE  = "UNUSED";

  input  CLK;
  input  [DATA_WIDTH-1:0] DI;
  input  [ADDR_WIDTH-1:0] ADDR;
  input  WE;
  output reg [DATA_WIDTH-1:0] DO;
  reg [DATA_WIDTH-1:0] RAM[2**ADDR_WIDTH-1:0];

  generate
    if (INIT_FILE != "UNUSED") begin
      initial $readmemh(INIT_FILE, RAM);
    end
  endgenerate

  always @(posedge CLK)
  begin
    if (WE) begin
      RAM[ADDR] <= DI;
      DO <= DI;
    end
    else begin
      DO <= RAM[ADDR];
    end 
  end 

endmodule

// True dual-port block RAM
// ========================

module BlockRAMTrueDual (
  CLK,       // Clock
  DI_A,      // Data in
  ADDR_A,    // Read address
  WE_A,      // Write enable
  DO_A,      // Data out
  DI_B,      // Data in
  ADDR_B,    // Read address
  WE_B,      // Write enable
  DO_B,      // Data out
  );

  parameter ADDR_WIDTH = 1;
  parameter DATA_WIDTH = 1;
  parameter INIT_FILE  = "UNUSED";

  input [(DATA_WIDTH-1):0] DI_A, DI_B;
  input [(ADDR_WIDTH-1):0] ADDR_A, ADDR_B;
  input WE_A, WE_B, CLK;
  output reg [(DATA_WIDTH-1):0] DO_A, DO_B;
  reg [DATA_WIDTH-1:0] RAM[2**ADDR_WIDTH-1:0];

  generate
    if (INIT_FILE != "UNUSED") begin
      initial $readmemh(INIT_FILE, RAM);
    end
  endgenerate

  // Port A 
  always @(posedge CLK)
  begin
    if (WE_A) begin
      RAM[ADDR_A] <= DI_A;
      DO_A <= DI_A;
    end else begin
      DO_A <= RAM[ADDR_A];
    end 
  end 

  // Port B 
  always @(posedge CLK)
  begin
    if (WE_B) begin
      RAM[ADDR_B] <= DI_B;
      DO_B <= DI_B;
    end else begin
      DO_B <= RAM[ADDR_B];
    end 
  end

endmodule
