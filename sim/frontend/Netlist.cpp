#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "Netlist.h"
#include "Hash.h"

// Constructor
Netlist::Netlist(Seq<Net*>* ns)
{
  // Create a hash table whose number of buckets equals the number of nets
  nameToId = new Hash<NetId> (ns->numElems);
  // Populate the hash table and assign net ids
  for (unsigned i = 0; i < ns->numElems; i++) {
    Net* net = ns->elems[i];
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
      NetWire* input = &net->inputs.elems[j];
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

// Roots for the topological sort
// Is the net a register or a component with no inputs?
inline bool isRoot(Net* net)
{
  return net->inputs.numElems == 0
      || !strcmp(net->prim, "reg")
      || !strcmp(net->prim, "regEn");
}

// Topological sort
// Exits with an error if combinatorial loop detected
void Netlist::topSort(Seq<Net*>* result)
{
  result->clear();

  // Track which nets have been discovered and removed
  bool* discovered = new bool [nets.numElems];
  bool* removed = new bool [nets.numElems];
  for (unsigned i = 0; i < nets.numElems; i++) {
    removed[i] = false;
    discovered[i] = false;
  }

  // Find initial roots
  Seq<Net*> roots;
  for (unsigned i = 0; i < nets.numElems; i++) {
    Net* net = nets.elems[i];
    if (isRoot(net)) {
      roots.append(net);
      discovered[net->id] = true;
    }
  }

  // For each net, find outgoing nets
  Seq<Net*>* outgoing = new SmallSeq<Net*> [nets.numElems];
  for (unsigned i = 0; i < nets.numElems; i++) {
    Net* netTo = nets.elems[i];
    if (isRoot(netTo)) continue;
    for (unsigned j = 0; j < netTo->inputs.numElems; j++) {
      NetWire* netFrom = &netTo->inputs.elems[j];
      outgoing[netFrom->id].append(netTo);
    }
  }

  // Remove a root on each iteration
  while (roots.numElems > 0) {
    // Remove a root
    Net* root = roots.pop();
    removed[root->id] = true;
    result->append(root);
    // Is any outgoing node from root now itself a root?
    for (unsigned i = 0; i < outgoing[root->id].numElems; i++) {
      Net* out = outgoing[root->id].elems[i];
      if (discovered[out->id]) continue;
      unsigned numInputs = 0;
      for (unsigned j = 0; j < out->inputs.numElems; j++) {
        if (! removed[out->inputs.elems[j].id]) {
          numInputs++;
          break;
        }
      }
      // This outgoing node is now a root
      if (numInputs == 0) {
        roots.append(out);
        discovered[out->id] = true;
      }
    }
  }

  // Combinatorial cycle?
  bool cyclic = result->numElems < nets.numElems;
  if (cyclic) {
    fprintf(stderr, "Error: combinatorial cycle detected\n");
    exit(EXIT_FAILURE);
  }

  // Free memory
  delete [] outgoing;
  delete [] discovered;
  delete [] removed;
}
