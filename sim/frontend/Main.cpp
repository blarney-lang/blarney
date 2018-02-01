#include <stdio.h>
#include "Netlist.h"
#include "Parser.h"
#include "CodeGen.h"

int main()
{
  Parser parser;
  parser.readFile("/tmp/heat.net");

  Seq<Net> nets;
  parser.demandNets(&nets);

  Netlist netlist(&nets);

  CodeGen codeGen;
  codeGen.gen(&netlist);

  return 0;
}
