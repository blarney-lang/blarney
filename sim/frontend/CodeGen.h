#ifndef _CODEGEN_H_
#define _CODEGEN_H_

#include <stdio.h>
#include <stdarg.h>
#include "Netlist.h"

struct CodeGen {
  // Code is written to this file (stdout by default)
  FILE* outFile;

  // Constructor
  CodeGen();

  // Destructor
  ~CodeGen();

  // Exit with a code gen error
  void genError(const char* fmt, ...);

  // Emit code
  inline void emit(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(outFile, fmt, args);
    va_end(args);
  }

  // Change destination file
  void setOutputFile(char* filename);

  // Is wire width perfectly representable in C?
  inline bool isStdInt(unsigned width) {
    return width == 8 || width == 16 || width == 32 || width == 64;
  }

  // Generate code to declare wire
  void declareWire(NetWire wire, unsigned width);

  // Generate code to initialise wire
  void initWire(NetWire wire, unsigned width, char* init);

  // Generate code for unnary operator
  void unaryOp(NetId r, char* op, NetWire a, unsigned width);

  // Generate code for binary operator
  void binOp(NetId r, NetWire a, char* op, NetWire b, unsigned width);

  // Generate code for comparison operator
  void cmpOp(NetId r, NetWire a, char* op, NetWire b, unsigned width);

  // Generate code for replication operator
  void replicate(NetId r, NetWire a, unsigned width);

  // Generate code for register
  void reg(NetId r, NetWire data, unsigned width);

  // Generate code for register with enable
  void regEn(NetId r, NetWire cond, NetWire data, unsigned width);

  // Generate code for zero extend operator
  void zeroExtend(NetId r, NetWire a, unsigned rw, unsigned aw);

  // Generate code for display statement
  void display(Seq<NetWire>* vals, Seq<NetParam>* params);

  // Generate declarations
  void genDecls(Seq<Net*>* netlist);

  // Generate code for netlist
  void gen(Netlist* netlist);
};

#endif
