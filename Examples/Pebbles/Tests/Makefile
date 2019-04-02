# Binary utilities
RV_ARCH    = rv32i
RV_CC      = riscv64-unknown-elf-gcc
RV_LD      = riscv64-unknown-elf-ld
RV_OBJCOPY = riscv64-unknown-elf-objcopy

# Compiler and linker flags
CFLAGS  = -mabi=ilp32 -march=$(RV_ARCH) -O2 -I.
LDFLAGS = -melf32lriscv -G 0 

# Sizes of instruction memory in bytes
INSTR_BYTES = 65536

# Sizes of data memory in words
DATA_BYTES = 65536

.PHONY: all
all: $(patsubst %.S,%.code.hex,$(wildcard *.S)) \
     $(patsubst %.S,%.data.0.hex,$(wildcard *.S)) \
     $(patsubst %.S,%.data.1.hex,$(wildcard *.S)) \
     $(patsubst %.S,%.data.2.hex,$(wildcard *.S)) \
     $(patsubst %.S,%.data.3.hex,$(wildcard *.S))

%.code.hex: %.code.ihex
	./ihex-to-img.py $< hex 0 4 $(INSTR_BYTES) 1 > $@

%.data.0.hex: %.data.ihex
	./ihex-to-img.py $< hex $(INSTR_BYTES) 1 $(DATA_BYTES) 4 > $@

%.data.1.hex: %.data.ihex
	./ihex-to-img.py $< hex $$(($(INSTR_BYTES)+1)) 1 $(DATA_BYTES) 4 > $@

%.data.2.hex: %.data.ihex
	./ihex-to-img.py $< hex $$(($(INSTR_BYTES)+2)) 1 $(DATA_BYTES) 4 > $@

%.data.3.hex: %.data.ihex
	./ihex-to-img.py $< hex $$(($(INSTR_BYTES)+3)) 1 $(DATA_BYTES) 4 > $@

%.code.ihex: %.elf
	$(RV_OBJCOPY) -O ihex --only-section=.text $< $@

%.data.ihex: %.elf
	$(RV_OBJCOPY) -O ihex --remove-section=.text \
                --set-section-flags .bss=alloc,load,contents $< $@

%.elf: %.o link.ld
	$(RV_LD) $(LDFLAGS) -T link.ld -o $@ $<

%.o: %.S test_macros.h riscv_test.h encoding.h
	$(RV_CC) $(CFLAGS) -Wall -c -o $@ $<

.PHONY: clean
clean:
	rm -f *.o *.elf *.out *.ihex *.hex
