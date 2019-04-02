#!/usr/bin/python
#
# Copyright (c) Matthew Naylor
#
# Script to convert an Intel Hex file to:
#   * a raw hex image suitable for Verilator;
#   * a .mif file suitable for Quartus.

import sys

def usage():
  print "Usage: ihex-to-img.py <input.hex> <fmt> <base> <width> <depth> <inc>"
  print ""
  print "  <format> is either hex or mif"
  print "  Units of <width> and <depth> are bytes"

if len(sys.argv) != 7:
  usage()
  sys.exit()

fmt = sys.argv[2]
if fmt != "hex" and fmt != "mif":
  usage()
  sys.exit()

try:
  base  = int(sys.argv[3])
  width = int(sys.argv[4])
  depth = int(sys.argv[5])
  inc = int(sys.argv[6])
except:
  print "Invalid parameters"
  usage()
  sys.exit()

lineCount = 0
mem = {}

try:
  file = open(sys.argv[1], "rt")
  upperAddr = 0
  segOffset = 0
  for line in file:
    # Parse leading ":"
    if line[0] != ":": raise
    # Parse data field byte-count
    dataBytes = int(line[1:3], 16)
    # Parse address offset
    addrOffset = int(line[3:7], 16)
    # Parse record type
    recType = line[7:9]
    if recType == "00":
      addr = upperAddr*65536 + addrOffset + segOffset
      for i in range(0, dataBytes):
        mem[addr+i] = line[9+i*2:9+i*2+2]
    elif recType == "01":
      break
    elif recType == "02":
      segOffset = int(line[9:13], 16) * 16
    elif recType == "05":
      pass
    elif recType == "04":
      upperAddr = int(line[9:13], 16)
    else:
      print "Record type", recType, "not supported"
      sys.exit()
    lineCount = lineCount+1
except:
  print "Syntax error on line", lineCount
  sys.exit()

# Print out memory contents
if fmt == "mif":
  # Altera mif format
  print "DEPTH =", (depth/width)/inc, ";"
  print "WIDTH =", 8*width, ";"
  print "ADDRESS_RADIX = DEC ;"
  print "DATA_RADIX = HEX ;"
  print "CONTENT"
  print "BEGIN"
  byteList = []
  addr = base
  count = 0
  for i in range(base, base+depth, inc):
    byteList.insert(0, mem.get(i, "00"))
    if len(byteList) == width:
      print count, ": ",
      print "".join(byteList), ";"
      byteList = []
      addr = addr + 1
      count = count + 1
  print "END"
else:
  # Bluesim hex format
  byteList = []
  for i in range(base, base+depth, inc):
    byteList.insert(0, mem.get(i, "00"))
    if len(byteList) == width:
      print "".join(byteList)
      byteList = []
