#include <stdio.h>
#include <stdlib.h>
#include "Netlist.h"
#include "Hash.h"

// Constructor
Netlist::Netlist(Seq<Net>* ns)
{
  // Create a hash table whose number of buckets equals the number of nets
  nameToId = new Hash<NetId> (ns->numElems);
  // Populate the hash table and assign net ids
  for (unsigned i = 0; i < ns->numElems; i++) {
    Net* net = &ns->elems[i];
    if (nameToId->member(net->name)) {
      fprintf(stderr, "Duplicate net name: '%s'\n", net->name);
      exit(EXIT_FAILURE);
    }
    else {
      net->id = i;
      nets.append(net);
      nameToId->insert(net->name, i);
    }
  }
  // Propagate net ids throughout netlist
  for (unsigned i = 0; i < nets.numElems; i++) {
    Net* net = nets.elems[i];
    for (unsigned j = 0; j < net->inputs.numElems; j++) {
      NetWire* input = & net->inputs.elems[j];
      if (! nameToId->lookup(input->name, &input->id)) {
        fprintf(stderr, "Unknown net: '%s'\n", input->name);
        exit(EXIT_FAILURE);
      }
    }
  }
}

// Destructor
Netlist::~Netlist()
{
  delete nameToId;
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


