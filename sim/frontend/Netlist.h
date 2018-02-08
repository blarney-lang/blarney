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

// A custom component parameter is a key/value pair
struct CustomParam {
  char* key;
  char* val;
};

// Arguments to display primitive
// (Either a string literal or a wire width)
struct DisplayArg {
  bool isString;
  union {
    char* string;
    unsigned width;
  }
};

// Arguments to a custom primitive
struct CustomArgs {
  char* name;
  SmallSeq<char*> inputs;
  SmallSeq<char*> outputs;
  SmallSeq<char*> outputWidths;
  SmallSeq<CustomParam> params;
};

// Kinds of primitive component
enum PrimTag {
    CONST
  , ADD
  , SUB
  , MUL
  , DIV
  , MOD
  , NOT
  , AND
  , OR
  , XOR
  , SHIFT_LEFT
  , SHIFT_RIGHT
  , EQUAL
  , NOT_EQUAL
  , LESS_THAN
  , LESS_THAN_EQ
  , REGISTER
  , REGISTER_EN
  , REPLICATE_BIT
  , ZERO_EXTEND
  , SIGN_EXTEND
  , SELECT_BITS
  , CONCAT
  , MUX
  , COUNT_ONES
  , DISPLAY
  , FINISH
  , CUSTOM
};

// Primitive component
struct Prim {
  PrimTag tag;
  union {
    struct { unsigned width; } add;
    struct { unsigned width; } sub;
    struct { unsigned width; } mul;
    struct { unsigned width; } div;
    struct { unsigned width; } mod;
    struct { unsigned width; } bitNot;
    struct { unsigned width; } bitAnd;
    struct { unsigned width; } bitOr;
    struct { unsigned width; } bitXor;
    struct { unsigned width; } shiftLeft;
    struct { unsigned width; } shiftRight;
    struct { unsigned width; } equal;
    struct { unsigned width; } notEqual;
    struct { unsigned width; } lessThan;
    struct { unsigned width; } lessThanEq;
    struct { unsigned width; } reg;
    struct { unsigned width; } regEn;
    struct { unsigned width; } replicateBit;
    struct { unsigned widthIn, widthOut; } zeroExtend;
    struct { unsigned widthIn, widthOut; } signExtend;
    struct { unsigned high, low; } selectBits;
    struct { unsigned widthA, widthB; } concat;
    struct { unsigned width; } mux;
    struct { unsigned width; } countOnes;
    struct { SmallSeq<DisplayArg>* args; } display;
    struct { CustomArgs* args; } custom;
  };
};

// A net is a component instance
struct Net {
  // Net name
  NetName name;

  // Net id
  NetId id;

  // Primitive component
  Prim prim;

  // Inputs to the component
  SmallSeq<NetWire> inputs;

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
