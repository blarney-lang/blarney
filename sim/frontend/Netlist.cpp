#include <stdio.h>
#include <stdlib.h>
#include "Netlist.h"

// Add a net
// The given net will be freed by the destructor
void Netlist::addNet(Net* net)
{
  NetId id = net->id;
  unsigned oldNumElems = nets.numElems;
  if (id >= nets.maxElems) nets.setCapacity(id*2);
  if (id >= nets.numElems) nets.numElems = id+1;
  for (unsigned i = oldNumElems; i < nets.numElems; i++) nets.elems[i] = NULL;
  if (nets.elems[id] != NULL) {
    fprintf(stderr, "Duplicate net id in netlist: %d\n", id);
    exit(EXIT_FAILURE);
  }
  nets.elems[id] = net;
}

// Determine roots of the netlist
// That is, components with no outputs or external outputs
void Netlist::roots(Seq<Net*>* result)
{
  result->clear();

  for (unsigned i = 0; i < nets.numElems; i++) {
    Net* net = nets.elems[i];
    if (strcmp(net->prim, "display") == 0 ||
        strcmp(net->prim, "finish") == 0) {
      result->append(net);
    }
  }
}

// Recursive depth-first search
void dfsHelp(Seq<Net*>* nets, NetId root, bool* visited, Seq<Net*>* result)
{
  if (visited[root]) return;
  visited[root] = true;
  Net* net = nets->elems[root];
  for (unsigned i = 0; i < net->inputs.numElems; i++) {
    NetWire wire = net->inputs.elems[i];
    dfsHelp(nets, wire.id, visited, result);
  }
  result->append(net);
}

// Depth-first search
void Netlist::dfs(Seq<Net*>* result)
{
  result->clear();

  // Maintain a visited set
  bool* visited = new bool [nets.numElems];
  for (unsigned i = 0; i < nets.numElems; i++) visited[i] = false;

  // Determine roots
  Seq<Net*> rootNets;
  roots(&rootNets);

  // DFS from each root
  for (unsigned i = 0; i < rootNets.numElems; i++)
    dfsHelp(&nets, rootNets.elems[i]->id, visited, result);

  // Free memory
  delete [] visited;
}

// Destructor
Netlist::~Netlist()
{
  for (unsigned i = 0; i < nets.numElems; i++)
    if (nets.elems[i] != NULL) delete nets.elems[i];
}
