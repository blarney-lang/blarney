ifndef BLARNEY_ROOT
$(error "the BLARNEY_ROOT environment variable must be set")
endif

include $(BLARNEY_ROOT)/blarneyDirs.mk

GENS = $(basename $(SRCS))

all: $(GENS)

%-test-verilog: %-Verilog
	make -C $*-Verilog/
	$*-Verilog/$*

%-test-simulation: %
	./$< --simulate

%: %.hs
	blc --make -j -hidir $(HI_DIR) -odir $(O_DIR) $(BLC_FLAGS) $<

.SECONDARY: $(addsuffix -Verilog, $(GEN))
%-Verilog: %
	./$< --verilog

.PHONY: clean mrproper
clean:
	rm -rf $(GENS) *Verilog

mrproper: clean
	rm -rf $(BUILD_DIR)
