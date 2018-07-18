#ifndef _FPOPS_H_
#define _FPOPS_H_

#include <stdint.h>
#include <math.h>

inline uint32_t FPAdd32(uint32_t x, uint32_t y)
{
  float result = *((float*) &x) + *((float*) &y);
  return *((uint32_t*) &result);
}

inline uint32_t FPSub32(uint32_t x, uint32_t y)
{
  float result = *((float*) &x) - *((float*) &y);
  return *((uint32_t*) &result);
}

inline uint32_t FPMul32(uint32_t x, uint32_t y)
{
  float result = *((float*) &x) * *((float*) &y);
  return *((uint32_t*) &result);
}

inline uint32_t FPDiv32(uint32_t x, uint32_t y)
{
  float result = *((float*) &x) / *((float*) &y);
  return *((uint32_t*) &result);
}

inline uint32_t FPSqrt32(uint32_t x)
{
  float result = sqrtf(*((float*) &x));
  return *((uint32_t*) &result);
}

inline uint32_t FPCmp32(uint32_t x, uint32_t y)
{
  int eq = *((float*) &x) == *((float*) &y) ? 1 : 0;
  int lt = *((float*) &x) < *((float*) &y) ? 1 : 0;
  int le = *((float*) &x) <= *((float*) &y) ? 1 : 0;
  return (eq | (lt<<1) | (le<<2));
}

inline int32_t FPToInt32(uint32_t x)
{
  return (int32_t) *((float*) &x);
}

inline uint32_t FPFromInt32(int32_t x)
{
  float result = (float) x;
  return *((uint32_t*) &result);
}

#endif
