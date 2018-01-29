// Netlist

#ifndef _NETLIST_H_
#define _NETLIST_H_

#include <string.h>
#include "Seq.h"

// Unique id for nets
typedef unsigned NetId;

// A net input is a pair containing a net id and a pin number 
// (Nets may have many output pins)
struct NetInput {
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
  SmallSeq<NetInput> inputs;

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

// A netlist is a sequence of nets
typedef Seq<Net> Netlist;

#endif
