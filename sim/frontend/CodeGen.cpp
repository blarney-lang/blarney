#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <gmp.h>
#include "CodeGen.h"
#include "Netlist.h"

// Constructor
CodeGen::CodeGen() {
  outFile = stdout;
}

// Destructor
CodeGen::~CodeGen() {
  if (outFile != NULL && outFile != stdout) fclose(stdout);
}


// Exit with an error message
void CodeGen::genError(const char* fmt, ...)
{
  va_list args;
  va_start(args, fmt);
  fprintf(stderr, "Code gen error:\n");
  vfprintf(stderr, fmt, args);
  fprintf(stderr, "\n");
  va_end(args);
  exit(EXIT_FAILURE);
}

// Change destination file
void CodeGen::setOutputFile(char* filename) {
  outFile = fopen(filename, "wt");
  if (outFile == NULL)
    genError("Error opening file '%s'\n", filename);
}

// Generate code to declare wire
void CodeGen::declareWire(NetWire wire, unsigned width)
{
  if (isStdInt(width))
    emit("uint%d_t w%d_%d;\n", width, wire.id, wire.pin);
  else if (width <= 64)
    emit("uint64_t w%d_%d;\n", wire.id, wire.pin);
  else {
    unsigned numChunks = (width+31)/32;
    emit("uint32_t w%d_%d[%d];\n", wire.id, wire.pin, numChunks);
  }
}

// Generate code to initialise wire
void CodeGen::initWire(NetWire wire, unsigned width, char* init)
{
  if (width <= 64) {
    emit("w%d_%d = %s;\n", wire.id, wire.pin, init);
  }
  else {
    // Use a GNU big num to interpret the initial value
    mpz_t num;
    mpz_init_set_str(num, init, 10);
    // Initialise each 32-bit chunk
    unsigned numChunks = (width+31)/32;
    for (unsigned i = 0; i < numChunks; i++) {
      unsigned long bottom = mpz_get_ui(num) & 0xffffffff; // Bottom 32 bits
      emit("w%d_%d[%d] = %lu;\n", wire.id, wire.pin, i, bottom);
      mpz_fdiv_q_2exp(num, num, 32); // Shift right by 32
    }
    mpz_clear(num);
  }
}

// Generate code for binary operator
void CodeGen::binOp(NetId r, NetWire a, char* op, NetWire b, unsigned width)
{
  if (isStdInt(width))
    emit("w%d_0 = w%d_%d %s w%d_%d;\n",
      r, a.id, a.pin, op, b.id, b.pin);
  else if (width <= 64)
    emit("w%d_0 = (w%d_%d %s w%d_%d) & ((1ul << %d)-1);\n",
      r, a.id, a.pin, op, b.id, b.pin, width);
  else {
    const char* prim;
    if (strcmp(op, "+") == 0)
      prim = "addBU";
    else if (strcmp(op, "-") == 0)
      prim = "subBU";
    else if (strcmp(op, "*") == 0)
      prim = "mulBU";
    else if (strcmp(op, "%") == 0)
      prim = "modBU";
    else if (strcmp(op, "/") == 0)
      prim = "divBU";
    else if (strcmp(op, "&") == 0)
      prim = "andBU";
    else if (strcmp(op, "|") == 0)
      prim = "orBU";
    else if (strcmp(op, "^") == 0)
      prim = "xorBU";
    else if (strcmp(op, "<<") == 0)
      prim = "leftBU";
    else if (strcmp(op, ">>") == 0)
      prim = "rightBU";
    else
      genError("Unknown primitive '%s'\n", op);

    emit("%s(w%d_%d, w%d_%d, w%d_0, %d);\n",
        prim, a.id, a.pin, b.id, b.pin, r, width);
  }
}

// Generate code for comparison operator
void CodeGen::cmpOp(NetId r, NetWire a, char* op, NetWire b, unsigned width)
{
   if (width <= 64)
    emit("w%d = w%d %s w%d;\n", r, a, op, b);
  else {
    const char* prim;
    if (strcmp(op, "<") == 0)
      prim = "ltBU";
    else if (strcmp(op, "<=") == 0)
      prim = "leBU";
    else if (strcmp(op, ">") == 0)
      prim = "gtBU";
    else if (strcmp(op, ">=") == 0)
      prim = "geBU";
    else if (strcmp(op, "==") == 0)
      prim = "eqBU";
    else if (strcmp(op, "!=") == 0)
      prim = "neqBU";
    else
      genError("Unknown primitive '%s'\n", op);

    emit("w%d_0 = %s(w%d_%d, w%d_%d, %d);\n",
      r, prim, a.id, a.pin, b.id, b.pin, width);
  }
}

// Generate code for replication operator
void CodeGen::replicate(NetId r, NetWire a, unsigned width)
{
  if (width <= 64) {
    uint64_t ones = (1ul << width)-1;
    emit("w%d_0 = w%d_%d ? 0x%lxul : %luul;\n", r, a.id, a.pin, ones, 0);
  }
  else
    emit("replicateBU(w%d_%d, w%d_0, %d);\n", a.id, a.pin, r, width);
}

// Generate code for display statement
void CodeGen::display(Seq<NetWire>* wires, Seq<NetParam>* params) {
  NetWire cond = wires->elems[0];
  emit("if (w%d_%d) {\n", cond.id, cond.pin);
  unsigned i = 0;
  unsigned pi = 0;
  unsigned wi = 1;
  while (pi < params->numElems || wi < wires->numElems) {
    if (pi < params->numElems) {
      NetParam p = params->elems[pi];
      if (i == atoi(p.key)) {
        emit("  printf(\"(\%s\", \"%s\");\n", p.val);
        pi++;
        i++;
        continue;
      }
    }
    if (wi < wires->numElems) {
      NetWire w = wires->elems[wi];
      emit("  printf(\"\%d\", w%d_%d);\n", w.id, w.pin);
      wi++;
      i++;
    }
  }
  emit("  printf(\"\\n\");\n}\n");
}

// Generate declarations
void CodeGen::genDecls(Seq<Net*>* nets)
{
  for (unsigned i = 0; i < nets->numElems; i++) {
    Net* net = nets->elems[i];
    if (net == NULL) continue;
    if (strcmp(net->prim, "display") == 0 ||
        strcmp(net->prim, "finish") == 0) continue;
    NetWire wire;
    wire.id = net->id;
    wire.pin = 0;
    unsigned width = net->width;
    if (strcmp(net->prim, "<")  == 0 ||
        strcmp(net->prim, "<=") == 0 ||
        strcmp(net->prim, ">")  == 0 ||
        strcmp(net->prim, ">=") == 0 ||
        strcmp(net->prim, "==") == 0 ||
        strcmp(net->prim, "!=") == 0)
      width = 1;
    declareWire(wire, net->width);
  }
}

// Generate code
void CodeGen::gen(Netlist* netlist)
{
  Seq<Net*> sorted;
  netlist->dfs(&sorted);
  genDecls(&sorted);
  // TODO: complete me
}
