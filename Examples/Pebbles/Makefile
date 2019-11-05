.PHONY: all
all:
	blc Main.hs -o Pebbles
	./Pebbles
	make -C Pebbles-Verilog
	make -C Boot

de5: all
	make -C DE5-Net

de5-clean:
	make -C DE5-Net clean

clean:
	make -C Boot clean
	make -C Tests clean
	make -C DE5-Net clean-mif
	rm -rf *.o *.hi Pebbles Pebbles-Verilog
