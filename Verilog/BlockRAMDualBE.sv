// Dual-port block RAM with byte enables
// =====================================

module BlockRAMDualBE (
  CLK,     // Clock
  DI,      // Data in
  RD_ADDR, // Read address
  WR_ADDR, // Write address
  WE,      // Write enable
  RE,      // Read enable
  BE,      // Byte enable
  DO       // Data out
  );

  parameter ADDR_WIDTH = 1;
  parameter DATA_WIDTH = 1;
  parameter INIT_FILE  = "UNUSED";
  parameter BE_WIDTH   = DATA_WIDTH/8;

  input  CLK;
  input  [DATA_WIDTH-1:0] DI;
  input  [ADDR_WIDTH-1:0] RD_ADDR, WR_ADDR;
  input  WE, RE;
  input  [BE_WIDTH-1:0] BE;
  output reg [DATA_WIDTH-1:0] DO;
  logic [BE_WIDTH-1:0][7:0] RAM[2**ADDR_WIDTH-1:0];

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

  // Write port
  generate
    for (genvar b = 0; b < BE_WIDTH; b++) begin
      always_ff@(posedge CLK) begin
        if (WE) begin
          if(BE[b]) RAM[WR_ADDR][b] <= DI[((b+1)*8-1):b*8];
        end
      end
    end
  endgenerate

  // Read port
  always_ff@(posedge CLK) begin
    if (RE) begin
      DO <= (WE && RD_ADDR == WR_ADDR) ? {DATA_WIDTH{1'hx}} : RAM[RD_ADDR];
    end
  end

endmodule
