ifndef BLARNEY_ROOT
$(error "the BLARNEY_ROOT environment variable must be set")
endif

include $(BLARNEY_ROOT)/blarneyDirs.mk

BLARNEY_TOP_MODULES_SRC = $(SRC_DIR)/Blarney.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/BitPat.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/BitScan.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Connectable.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Interconnect.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Option.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/PulseWire.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Queue.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Recipe.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/SourceSink.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Stmt.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Stream.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Vector.hs

all: ghc-build blc-build

ghc-build:
	ghc --make -j \
        -hidir $(HI_DIR) -odir $(O_DIR) -i$(SRC_DIR) \
        -Wno-partial-type-signatures \
        $(BLARNEY_TOP_MODULES_SRC)

blc-build:
	blc --make -j \
        -hidir $(HI_DIR) -odir $(O_DIR) -i$(SRC_DIR) \
        $(BLARNEY_TOP_MODULES_SRC)

.PHONY: clean
clean:
	#rm -f $(shell find Haskell -regex ".*\.\(hi\|o\)$$")
	rm -rf $(BUILD_DIR)
	rm -rf Haskell/BlarneyPlugins/Namer/dist
	make -C Examples clean
