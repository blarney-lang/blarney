// This module provides a streaming interface to
// Altera's memory-mapped JTAG UART component.

// State of the core
`define JTAG_IDLE 0          // Do nothing
`define JTAG_READ_DATA 1     // Consume char from UART, if available
`define JTAG_READ_WSPACE 2   // Read UART's CSR to determine write space
`define JTAG_WRITE_DATA 3    // Write char to UART's data register

module JTAGUART
  ( input wire clock
  , input wire reset

    // Avalon memory-mapped interface
  , output wire [2:0]  address
  , output wire [31:0] writedata
  , output wire        write
  , output wire        read
  , input  wire        waitrequest
  , input  wire [31:0] readdata

    // Input stream
  , input  wire        in_canGet
  , input  wire [7:0]  in_getData
  , output wire        in_get

    // Output stream
  , output wire        out_canGet
  , output wire [7:0]  out_getData
  , input  wire        out_get
  );

  // Input queue
  reg inQueueFull = 0;
  reg [7:0] inQueueData;

  assign in_get = in_canGet && !inQueueFull;

  // Output queue
  reg outQueueFull = 0;
  reg [7:0] outQueueData;

  assign out_canGet = outQueueFull;
  assign out_getData = outQueueData;

  // This register is used to toggle between reading and writing
  reg toggle = 0;

  // Current state of state machine
  reg [1:0] state = `JTAG_IDLE;

  //  Avalon memory-mapped outputs
  assign address =
    (state == `JTAG_READ_DATA || state == `JTAG_WRITE_DATA) ? 0 : 4;
  assign writedata = {24'h0, inQueueData};
  assign write = state == `JTAG_WRITE_DATA;
  assign read = state == `JTAG_READ_DATA || state == `JTAG_READ_WSPACE;

  // State machine
  always @(posedge clock) begin
    if (reset) begin
      state <= `JTAG_IDLE;
      outQueueFull <= 0;
      inQueueFull <= 0;
    end else begin
      if (in_canGet && !inQueueFull) begin
        inQueueData <= in_getData;
        inQueueFull <= 1;
      end

      if (out_get && outQueueFull) outQueueFull <= 0;

      case (state)
        `JTAG_IDLE:
          begin
            toggle <= !toggle;
            if (inQueueFull && toggle)
              state <= `JTAG_READ_WSPACE;
            else if (!outQueueFull)
              state <= `JTAG_READ_DATA;
          end
        `JTAG_READ_DATA:
          if (!waitrequest) begin
            if (readdata[15] == 1) begin
              outQueueFull <= 1;
              outQueueData <= readdata[7:0];
            end
            state <= `JTAG_IDLE;
          end
        `JTAG_READ_WSPACE:
          if (!waitrequest)
            state <= readdata[31:16] > 0 ? `JTAG_WRITE_DATA : `JTAG_IDLE;
        `JTAG_WRITE_DATA:
          if (!waitrequest) begin
            inQueueFull <= 0;
            state <= `JTAG_IDLE;
          end
      endcase
    end
  end

endmodule
