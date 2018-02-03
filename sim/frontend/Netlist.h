// Netlist

#ifndef _NETLIST_H_
#define _NETLIST_H_

#include <string.h>
#include "Seq.h"
#include "Hash.h"

// Unique string for primitive components (nets)
typedef char* NetName;

// Unique int for primitive components (nets)
// (For faster lookup, compared to strings)
typedef int NetId;

// A net wire is a net id and a pin number 
// (Components may have many output pins)
struct NetWire {
  NetName name;
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
  // Net name
  NetName name;

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
  inline char* lookup(const char* k)
  {
    for (int i = 0; i < params.numElems; i++) {
      if (strcmp(params.elems[i].key, k) == 0)
        return params.elems[i].val;
    }
    return NULL;
  }
};

// Netlist structure
struct Netlist {
  // A netlist is a sequence of nets, indexed by net id
  Seq<Net*> nets;

  // Maintain mapping from net name to net id
  Hash<NetId>* nameToId;

  // Constructor
  Netlist(Seq<Net*>* nets);

  // Destructor
  ~Netlist();

  // Split register->register connections with an intermediate wire
  void splitRegReg();

  // Topological sort
  // Exits with an error if combinatorial loop detected
  void topSort(Seq<Net*>* result);

};

#endif
