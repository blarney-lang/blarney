entry:

# Set stack pointer to __stackEnd - 4
la sp, __stackEnd
addi sp, sp, -4

# Jump to main
jal main

# Loop forever
j .
