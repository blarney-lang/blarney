module PebblesTop
  ( input wire clock
  , input wire reset

    // Avalon memory-mapped JTAG UART interface
  , output wire [2:0]  address
  , output wire [31:0] writedata
  , output wire        write
  , output wire        read
  , input  wire        waitrequest
  , input  wire [31:0] readdata
  );

  // Input stream
  wire        in_canGet;
  wire [7:0]  in_getData;
  wire        in_get;

  // Output stream
  wire        out_canGet;
  wire [7:0]  out_getData;
  wire        out_get;

  JTAGUART uart
  ( .clock(clock)
  , .reset(reset)

    // Avalon memory-mapped interface
  , .address(address)
  , .writedata(writedata)
  , .write(write)
  , .read(read)
  , .waitrequest(waitrequest)
  , .readdata(readdata)

    // Input stream
  , .in_canGet(in_canGet)
  , .in_getData(in_getData)
  , .in_get(in_get)

    // Output stream
  , .out_canGet(out_canGet)
  , .out_getData(out_getData)
  , .out_get(out_get)
  );

  Pebbles cpu
  ( .clock(clock)
  , .reset(reset)
  , .in_canPeek(out_canGet)
  , .in_peek(out_getData)
  , .in_consume_en(out_get)
  , .out_consume_en(in_get)
  , .out_peek(in_getData)
  , .out_canPeek(in_canGet)
  );

endmodule
