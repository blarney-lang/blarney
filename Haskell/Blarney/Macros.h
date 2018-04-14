-- For easy type-safe bit-vector indexing
#define bit(i)      (tbit (__ :: nat i))
#define bits(hi,lo) (tbits (__ :: nat hi, __ :: nat lo))

-- For bit patterns
#define pv(w) (varBP :: BP w _ _)
#define pn(w, x) (numBP (x :: Bit w))
