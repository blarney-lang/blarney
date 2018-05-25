-- For easy type-safe bit-vector indexing
#define bit(i)      (typedGetBit (__ :: nat i))
#define bits(hi,lo) (typedGetBits (__ :: nat hi, __ :: nat lo))
