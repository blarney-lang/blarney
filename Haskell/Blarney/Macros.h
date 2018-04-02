-- For easy type-safe bit-vector indexing
#define bit(i)      (tbit (__ :: nat i))
#define bits(hi,lo) (tbits (__ :: nat hi, __ :: nat lo))
