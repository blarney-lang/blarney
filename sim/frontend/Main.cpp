#include <stdio.h>
#include <stdlib.h>
#include "Netlist.h"
#include "Parser.h"
#include "CodeGen.h"

int main(int argc, char* argv[])
{
  if (argc != 2) {
    fprintf(stderr, "Filename expected\n");
    exit(EXIT_FAILURE);
  }

  Parser parser;
  parser.readFile(argv[1]);

  Seq<Net*> nets;
  parser.demandNets(&nets);

  Netlist netlist(&nets);

  CodeGen codeGen;
  codeGen.gen(&netlist);

  return 0;
}
