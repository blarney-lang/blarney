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
      DO <= {DATA_WIDTH{1'hx}};
    end
    else begin
      DO <= RAM[ADDR];
    end 
  end 

endmodule
