ifndef BLARNEY_ROOT
$(error "the BLARNEY_ROOT environment variable must be set")
endif

include $(BLARNEY_ROOT)/blarneyDirs.mk

BLC = $(BLARNEY_ROOT)/Scripts/blc
GENS = $(basename $(SRCS))

all: $(GENS)

%-test-verilog: %-Verilog
	make -C $*-Verilog/
	$*-Verilog/$*

%-test-simulation: %
	./$< --simulate

%: %.hs
	BLARNEY_ROOT=$(BLARNEY_ROOT) $(BLC) -O --make -hidir $(HI_DIR) -odir $(O_DIR) $(BLC_FLAGS) $<

.SECONDARY: $(addsuffix -Verilog, $(GEN))
%-Verilog: %
	./$< --verilog

.PHONY: clean mrproper
clean:
	rm -rf $(GENS) *-Verilog *-SMT

mrproper: clean
	rm -rf $(BUILD_DIR)
