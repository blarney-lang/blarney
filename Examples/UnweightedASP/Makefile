.PHONY: all
all:
	blc ASPEngine.hs -cpp
	blc ASPDirect.hs -cpp
	blc ASPStream.hs -cpp

clean:
	rm -f *.o *.hi ASPEngine ASPDirect ASPStream
	rm -rf ASPEngine-Verilog ASPDirect-Verilog ASPStream-Verilog
