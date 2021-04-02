GENS = $(basename $(SRCS))

all: $(GENS)

%-test-verilog: %-Verilog
	make -C $*-Verilog/
	$*-Verilog/$*

%-test-simulation: %
	./$< --simulate

%: %.hs
	blc $(BLC_FLAGS) $<

.SECONDARY: $(addsuffix -Verilog, $(GEN))
%-Verilog: %
	./$< --verilog

.PHONY: clean
clean:
	rm -rf *.o *.hi $(GENS) *Verilog
