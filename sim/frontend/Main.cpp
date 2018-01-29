#include <stdio.h>
#include "Netlist.h"
#include "Parser.h"

int main()
{
  Parser parser;
  parser.readFile("/tmp/heat.net");

  Netlist netlist;
  parser.demandNetlist(&netlist);

  printf("Parsed %i characters and %i lines\n",
    parser.next, parser.lineNumber);
  return 0;
}
