BLARNEY_ROOT ?= $(realpath .)

include $(BLARNEY_ROOT)/blarneyDirs.mk

BLARNEY_TOP_MODULES_SRC = $(SRC_DIR)/Blarney.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/BitPat.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/BitScan.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/ClientServer.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Connectable.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Interconnect.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Option.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/PulseWire.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/PulseReg.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Queue.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Recipe.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/SourceSink.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Stmt.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Stack.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Stream.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/TaggedUnion.hs
BLARNEY_TOP_MODULES_SRC += $(SRC_DIR)/Blarney/Vector.hs

all: ghc-build blc-build

ghc-build:
	ghc --make \
        -hidir $(HI_DIR) -odir $(O_DIR) -i$(SRC_DIR) \
        -Wno-partial-type-signatures \
        $(BLARNEY_TOP_MODULES_SRC)

blc-build:
	BLARNEY_ROOT=$(BLARNEY_ROOT) blc --make \
        -hidir $(HI_DIR) -odir $(O_DIR) -i$(SRC_DIR) \
        $(BLARNEY_TOP_MODULES_SRC)

.PHONY: clean
clean:
	#rm -f $(shell find Haskell -regex ".*\.\(hi\|o\)$$")
	rm -rf $(BUILD_DIR)
	rm -rf Haskell/BlarneyPlugins/Namer/dist
	make -C Examples clean
	cabal clean
