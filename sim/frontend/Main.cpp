#include <stdio.h>
#include "Netlist.h"
#include "Parser.h"
#include "CodeGen.h"

int main()
{
  Parser parser;
  parser.readFile("/tmp/heat.net");

  Netlist netlist;
  parser.demandNetlist(&netlist);

  CodeGen codeGen;
  codeGen.gen(&netlist);

  return 0;
}
