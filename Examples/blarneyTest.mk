GENS = $(basename $(SRCS))

all: $(GENS)

%-test-verilog: %-Verilog
	make -C $*-Verilog/
	$*-Verilog/$*

%-test-simulation: %
	./$< --test-simulation

%: %.hs
	blc -i$(BLARNEY_ROOT)/Examples $(BLC_FLAGS) $<

.SECONDARY: $(addsuffix -Verilog, $(GEN))
%-Verilog: %
	./$< --test-verilog-gen

.PHONY: clean
clean:
	rm -rf *.o *.hi $(GENS) *Verilog
