entry:

# Set stack pointer
la sp, __stackBase

# Jump to main
jal main

# Terminate simulator
csrw 0x801, zero

# Loop forever
j .
