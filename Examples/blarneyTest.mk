ifndef BLARNEY_ROOT
$(error "the BLARNEY_ROOT environment variable must be set")
endif

BUILD_DIR = $(BLARNEY_ROOT)/build
O_DIR = $(BUILD_DIR)/odir
HI_DIR = $(BUILD_DIR)/hidir

GENS = $(basename $(SRCS))

all: $(GENS)

%-test-verilog: %-Verilog
	make -C $*-Verilog/
	$*-Verilog/$*

%-test-simulation: %
	./$< --simulate

%: %.hs
	blc -hidir $(HI_DIR) -odir $(O_DIR) $(BLC_FLAGS) $<

.SECONDARY: $(addsuffix -Verilog, $(GEN))
%-Verilog: %
	./$< --verilog

.PHONY: clean mrproper
clean:
	rm -rf $(GENS) *Verilog

mrproper: clean
	rm -rf $(BUILD_DIR)
