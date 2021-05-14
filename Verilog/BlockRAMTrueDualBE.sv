// True dual-port block RAM with byte enables
// ==========================================

module BlockRAMTrueDualBE (
  CLK,       // Clock
  DI_A,      // Data in
  ADDR_A,    // Read address
  WE_A,      // Write enable
  RE_A,      // Read enable
  BE_A,      // Byte enable
  DO_A,      // Data out
  DI_B,      // Data in
  ADDR_B,    // Read address
  WE_B,      // Write enable
  RE_B,      // Read enable
  BE_B,      // Byte enable
  DO_B       // Data out
  );

  parameter ADDR_WIDTH = 1;
  parameter DATA_WIDTH = 1;
  parameter INIT_FILE  = "UNUSED";
  parameter BE_WIDTH   = DATA_WIDTH/8;

  input  CLK;
  input  [DATA_WIDTH-1:0] DI_A, DI_B;
  input  [ADDR_WIDTH-1:0] ADDR_A, ADDR_B;
  input  WE_A, WE_B, RE_A, RE_B;
  input  [BE_WIDTH-1:0] BE_A, BE_B;
  output reg [DATA_WIDTH-1:0] DO_A, DO_B;
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

  // Port A
  generate
    for (genvar b = 0; b < BE_WIDTH; b++) begin
      always_ff@(posedge CLK) begin
        if (WE_A) begin
          if(BE_A[b]) RAM[ADDR_A][b] <= DI_A[((b+1)*8-1):b*8];
        end
      end
    end
  endgenerate

  always_ff@(posedge CLK) begin
    if (RE_A) begin
      if (WE_A) begin
        DO_A <= {DATA_WIDTH{1'hx}};
      end else begin
        DO_A <= (WE_B && ADDR_A == ADDR_B) ? {DATA_WIDTH{1'hx}} : RAM[ADDR_A];
      end
    end
  end

  // Port B
  generate
    for (genvar b = 0; b < BE_WIDTH; b++) begin
      always_ff@(posedge CLK) begin
        if (WE_B) begin
          if(BE_B[b]) RAM[ADDR_B][b] <= DI_B[((b+1)*8-1):b*8];
        end
      end
    end
  endgenerate

  always_ff@(posedge CLK) begin
    if (RE_B) begin
      if (WE_B) begin
        DO_B <= {DATA_WIDTH{1'hx}};
      end else begin
        DO_B <= (WE_A && ADDR_A == ADDR_B) ? {DATA_WIDTH{1'hx}} : RAM[ADDR_B];
      end
    end
  end

endmodule
