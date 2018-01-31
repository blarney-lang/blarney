// Netlist

#ifndef _NETLIST_H_
#define _NETLIST_H_

#include <string.h>
#include "Seq.h"

// Unique id for primitive components (nets)
typedef unsigned NetId;

// A net wire is a pair containing a net id and a pin number 
// (Components may have many output pins)
struct NetWire {
  NetId id;
  unsigned pin;
};

// A net parameter is a key/value pair
struct NetParam {
  char* key;
  char* val;
};

// A net is a component instance
struct Net {
  // Net id
  NetId id;

  // Primitive component name
  char* prim;

  // Inputs to the component
  SmallSeq<NetWire> inputs;

  // Parameters of the component
  SmallSeq<NetParam> params;

  // Bit-width of component
  unsigned width;

  // Lookup a parameter
  inline char* lookup(char* k)
  {
    for (int i = 0; i < params.numElems; i++)
      if (strcmp(params.elems[i].key, k) == 0)
        return params.elems[i].val;
    return NULL;
  }
};

// Netlist structure
struct Netlist {
  // A netlist is a sequence of nets, indexed by net id
  Seq<Net*> nets;

  // Add a net
  // The given net will be freed by the destructor
  void addNet(Net* net);

  // Determine roots of the netlist
  // That is, components with no outputs or external outputs
  void roots(Seq<Net*>* result);

  // Depth-first search
  void dfs(Seq<Net*>* result);

  // Destructor
  ~Netlist();
};

#endif
