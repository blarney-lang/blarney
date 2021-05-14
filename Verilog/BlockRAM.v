// Single-port block RAM
// =====================

module BlockRAM (
  CLK,     // Clock
  DI,      // Data in
  ADDR,    // Read address
  WE,      // Write enable
  RE,      // Read enable
  DO       // Data out
  );

  parameter ADDR_WIDTH = 1;
  parameter DATA_WIDTH = 1;
  parameter INIT_FILE  = "UNUSED";

  input  CLK;
  input  [DATA_WIDTH-1:0] DI;
  input  [ADDR_WIDTH-1:0] ADDR;
  input  WE, RE;
  output reg [DATA_WIDTH-1:0] DO;
  reg [DATA_WIDTH-1:0] RAM[2**ADDR_WIDTH-1:0];

  generate
    if (INIT_FILE != "UNUSED") begin
      initial $readmemh(INIT_FILE, RAM);
    end else begin
      integer i;
      initial
        for (i = 0; i < 2**ADDR_WIDTH; i=i+1)
          RAM[i] = 0;
    end
  endgenerate

  always @(posedge CLK)
  begin
    if (WE) begin
      RAM[ADDR] <= DI;
    end
    if (RE) begin
      if (WE) begin
        DO <= {DATA_WIDTH{1'hx}};
      end else begin
        DO <= RAM[ADDR];
      end 
    end
  end 

endmodule
