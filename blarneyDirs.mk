ifndef BLARNEY_ROOT
$(error "the BLARNEY_ROOT environment variable must be set")
endif

SRC_DIR = $(BLARNEY_ROOT)/Haskell
BUILD_DIR = $(BLARNEY_ROOT)/build
O_DIR = $(BUILD_DIR)/odir
HI_DIR = $(BUILD_DIR)/hidir
