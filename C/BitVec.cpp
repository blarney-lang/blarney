#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include "BitVec.h"

inline uint32_t shr(uint32_t x, uint32_t y) { return y >= 32 ? 0 : x >> y; }
inline uint32_t shl(uint32_t x, uint32_t y) { return y >= 32 ? 0 : x << y; }

// Shift left core
void leftCoreBU(BU a, uint64_t i, BU r, uint32_t w, uint32_t numChunks)
{
  // Number of chunks and bits to shift
  uint32_t chunkShift = i/ChunkSize;
  uint32_t bitShift = i%ChunkSize;
  // Left shift
  for (int32_t i = numChunks-1; i >= 0; i--) {
    if (i == chunkShift)
      r[i] = (a[i-chunkShift] << bitShift);
    else if (i > chunkShift)
      r[i] = (a[i-chunkShift] << bitShift) |
             shr(a[i-chunkShift-1], ChunkSize-bitShift);
    else
      r[i] = 0;
  }
  r[numChunks-1] = fitMSCU(r[numChunks-1], w);
}

// Right shift core
void rightCoreBU(BU a, uint64_t i, BU r, uint32_t w, uint32_t numChunks)
{
  // Number of chunks and bits to shift
  uint32_t chunkShift = i/ChunkSize;
  uint32_t bitShift = i%ChunkSize;
  // Right shift
  for (uint32_t i = 0; i < numChunks; i++) {
    if ((i+chunkShift) == (numChunks-1))
      r[i] = (a[i+chunkShift] >> bitShift);
    else if ((i+chunkShift) < (numChunks-1))
      r[i] = (a[i+chunkShift] >> bitShift) |
             shl(a[i+chunkShift+1], ChunkSize-bitShift);
    else
      r[i] = 0;
  }
}

// Addition/subtraction
void addSubBU(bool sub, BU a, BU b, BU r, Chunk carry,
                uint32_t w, uint32_t numChunks)
{
  for (uint32_t i = 0; i < numChunks; i++) {
    Chunk bi = sub ? ~b[i] : b[i];
    DoubleChunk sum = a[i] + bi + carry;
    r[i] = (Chunk) sum;
    carry = (Chunk) (sum >> ChunkSize);
  }
  r[numChunks-1] = fitMSCU(r[numChunks-1], w);
}

// Multiplication
void mulCoreBU(BU a, BU b, BU r, uint32_t w, uint32_t numChunks)
{
  // Initialise result
  for (uint32_t i = 0; i < numChunks; i++) r[i] = 0;
  // Sum the partial products
  for (uint32_t i = 0; i < numChunks; i++) {
    Chunk carry = 0;
    for (uint32_t j = 0; j < numChunks-i; j++) {
      DoubleChunk mul = a[i] * b[j];
      Chunk lo = (Chunk) mul;
      Chunk hi = (Chunk) (mul >> ChunkSize);
      DoubleChunk sum = r[i+j] + lo + carry;
      r[i+j] = (Chunk) sum;
      carry = (Chunk) (hi + (sum >> ChunkSize));
    }
  }
  r[numChunks-1] = fitMSCU(r[numChunks-1], w);
}

// Division/modulus
// (Shift-and-subtract!)
void divModBU(BU a, BU b, BU q, BU r, uint32_t w, uint32_t numChunks)
{
  // Check for division by zero
  bool divByZero = true;
  for (uint32_t i = 0; i < numChunks; i++)
    if (b[i] != 0) { divByZero = false; break; }
  if (divByZero) {
    for (uint32_t i = 0; i < numChunks; i++) q[i] = r[i] = 0;
    return;
  }
  // Initialise quotient and remainder
  for (uint32_t i = 0; i < numChunks; i++) q[i] = r[i] = 0;
  // Shift and subtract
  for (int32_t i = w-1; i >= 0; i--) {
    leftCoreBU(r, 1, r, w, numChunks);
    setBitBU(r, 0, getBitBU(a, i));
    if (geBU(r, b, w)) {
      subBU(r, b, r, w);
      setBitBU(q, i, 1);
    }
  }
}

// Sign extend core
void signExtCoreBU(BU a, BU r, uint32_t aw, uint32_t rw,
                     uint32_t aChunks, uint32_t rChunks)
{
  for (uint32_t i = 0; i < aChunks; i++) r[i] = a[i];
  uint8_t sign = getBitBU(a, aw-1);
  for (uint32_t i = aw; i < aChunks * ChunkSize; i++)
    setBitBU(r, i, sign);
  Chunk ext = sign ? ~0 : 0;
  for (uint32_t i = aChunks; i < rChunks; i++) r[i] = ext;
  r[rChunks-1] = fitMSCU(r[rChunks-1], rw);
}

// Golden model & testing
// ======================

#ifdef BITVEC_TEST

// Bit-width for testing (valid range 9..24)
#define TEST_WIDTH 15

// Unsigned addition model
inline uint32_t addU(uint32_t a, uint32_t b, uint32_t w) 
  { return (a+b)%(1<<w); }

// Unsigned subtraction model
inline uint32_t subU(uint32_t a, uint32_t b, uint32_t w) 
  { return (a-b)%(1<<w); }

// Unsigned multiplier model
inline uint32_t mulU(uint32_t a, uint32_t b, uint32_t w) 
  { return (a*b)%(1<<w); }

// Unsigned division model
inline uint32_t divU(uint32_t a, uint32_t b, uint32_t w) 
  { return b == 0 ? 0 : (a/b); }

// Unsigned modulus model
inline uint32_t modU(uint32_t a, uint32_t b, uint32_t w) 
  { return b == 0 ? 0 : (a%b); }

// Unsigned left shift model
inline uint32_t leftU(uint32_t a, uint32_t b, uint32_t w) 
  { return b >= 32 ? 0 : (a<<b)%(1<<w); }

// Unsigned right shift model
inline uint32_t rightU(uint32_t a, uint32_t b, uint32_t w) 
  { return b >= 32 ? 0 : (a>>b)%(1<<w); }

// Unsigned greater than or equal
inline uint8_t geU(uint32_t a, uint32_t b, uint32_t w) 
  { return a >= b ? 1 : 0; }

// Unsigned greater than
inline uint8_t gtU(uint32_t a, uint32_t b, uint32_t w) 
  { return a > b ? 1 : 0; }

// Unsigned less than
inline uint8_t ltU(uint32_t a, uint32_t b, uint32_t w) 
  { return a < b ? 1 : 0; }

// Unsigned less than or equal
inline uint8_t leU(uint32_t a, uint32_t b, uint32_t w) 
  { return a <= b ? 1 : 0; }

// Unsigned equal
inline uint8_t eqU(uint32_t a, uint32_t b, uint32_t w) 
  { return a == b ? 1 : 0; }

// Unsigned not equal
inline uint8_t neqU(uint32_t a, uint32_t b, uint32_t w) 
  { return a != b ? 1 : 0; }

// Unsigned bitwise not
inline uint32_t notU(uint32_t a, uint32_t w) 
  { return (~a)%(1<<w); }

// Unsigned bitwise and
inline uint32_t andU(uint32_t a, uint32_t b, uint32_t w) 
  { return (a&b)%(1<<w); }

// Unsigned bitwise or
inline uint32_t orU(uint32_t a, uint32_t b, uint32_t w) 
  { return (a|b)%(1<<w); }

// Unsigned bitwise xor
inline uint32_t xorU(uint32_t a, uint32_t b, uint32_t w) 
  { return (a^b)%(1<<w); }

// Concatenation
inline uint32_t concatU(uint32_t a, uint32_t b, uint32_t aw, uint32_t bw)
  { return (a << bw)|b; }

#define CHECK_OP1_U(STR, I, M) {                                          \
  for (uint32_t i = 0; i < (1<<(TEST_WIDTH-1)); i++) {                    \
    uint8_t a[3], c[3];                                                   \
    a[0] = i&255; a[1] = (i>>8)&255; a[2] = (i>>16)&255;                  \
    c[0] = c[1] = c[2] = 0;                                               \
    I(a, c, TEST_WIDTH);                                                  \
    uint32_t r = 0;                                                       \
    r = c[0] | (c[1] << 8) | (c[2] << 16);                                \
    if (M(i, TEST_WIDTH) != r) {                                          \
      printf("%i,%i\n", i, r);                                            \
      exit(EXIT_FAILURE);                                                 \
    }                                                                     \
  }                                                                       \
  printf("%s: PASSED\n", STR);                                            \
}

#define CHECK_OP2_U(STR, I, M) {                                          \
  for (uint32_t i = 0; i < (1<<(TEST_WIDTH-1)); i++) {                    \
    for (uint32_t j = 0; j < (1<<(TEST_WIDTH-1)); j++) {                  \
      uint8_t a[3], b[3], c[3];                                           \
      a[0] = i&255; a[1] = (i>>8)&255; a[2] = (i>>16)&255;                \
      b[0] = j&255; b[1] = (j>>8)&255; b[2] = (j>>16)&255;                \
      c[0] = c[1] = c[2] = 0;                                             \
      I(a, b, c, TEST_WIDTH);                                             \
      uint32_t r = 0;                                                     \
      r = c[0] | (c[1] << 8) | (c[2] << 16);                              \
      if (M(i, j, TEST_WIDTH) != r) {                                     \
        printf("%i,%i,%i\n", i, j, r);                                    \
        exit(EXIT_FAILURE);                                               \
      }                                                                   \
    }                                                                     \
  }                                                                       \
  printf("%s: PASSED\n", STR);                                            \
}

#define CHECK_CMP2_U(STR, I, M) {                                         \
  for (uint32_t i = 0; i < (1<<(TEST_WIDTH-1)); i++) {                    \
    for (uint32_t j = 0; j < (1<<(TEST_WIDTH-1)); j++) {                  \
      uint8_t a[3], b[3];                                                 \
      a[0] = i&255; a[1] = (i>>8)&255; a[2] = (i>>16)&255;                \
      b[0] = j&255; b[1] = (j>>8)&255; b[2] = (j>>16)&255;                \
      uint8_t r = I(a, b, TEST_WIDTH);                                    \
      if (M(i, j, TEST_WIDTH) != r) {                                     \
        printf("%i,%i,%i\n", i, j, r);                                    \
        exit(EXIT_FAILURE);                                               \
      }                                                                   \
    }                                                                     \
  }                                                                       \
  printf("%s: PASSED\n", STR);                                            \
}

void testConcat()
{
  for (uint32_t i = 1; i <= 16; i++) {
    for (uint32_t j = 1; j <= 16; j++) {
      for (uint32_t n = 0; n < 1024; n++) {
        uint8_t a[2], b[2], r[4];
        uint32_t ra = abs(rand()) % (1 << i);
        uint32_t rb = abs(rand()) % (1 << j);
        a[0] = ra&255; a[1] = (ra>>8)&255;
        b[0] = rb&255; b[1] = (rb>>8)&255;
        r[0] = r[1] = r[2] = r[3] = 0;
        concatBU(a, b, r, i, j);
        uint32_t res;
        res = r[0] | (r[1] << 8) | (r[2] << 16) | (r[3] << 24);
        if (res != concatU(ra, rb, i, j)) {
          printf("{%i:%i,%i:%i}=%i\n", ra, i, rb, j, res);
          exit(EXIT_FAILURE);
        }
      }
    }
  }
  printf("BIT CONCAT: PASSED\n");
}
 
int main()
{
  printf("Testing bit-vector primitives.  Please be patient!\n");
  testConcat();
  CHECK_OP2_U("UNSIGNED ADD", addBU, addU);
  CHECK_OP2_U("UNSIGNED SUB", subBU, subU);
  CHECK_OP2_U("UNSIGNED MUL", mulBU, mulU);
  CHECK_OP2_U("UNSIGNED SHIFT LEFT", leftBU, leftU);
  CHECK_OP2_U("UNSIGNED SHIFT RIGHT", rightBU, rightU);
  CHECK_CMP2_U("UNSIGNED GE", geBU, geU);
  CHECK_CMP2_U("UNSIGNED GT", gtBU, gtU);
  CHECK_CMP2_U("UNSIGNED LE", leBU, leU);
  CHECK_CMP2_U("UNSIGNED LT", ltBU, ltU);
  CHECK_CMP2_U("UNSIGNED EQ", eqBU, eqU);
  CHECK_CMP2_U("UNSIGNED NEQ", neqBU, neqU);
  CHECK_OP1_U("UNSIGNED NOT", notBU, notU);
  CHECK_OP2_U("UNSIGNED AND", andBU, andU);
  CHECK_OP2_U("UNSIGNED OR",  orBU, orU);
  CHECK_OP2_U("UNSIGNED XOR", xorBU, xorU);
  CHECK_OP2_U("UNSIGNED DIV", divBU, divU);
  CHECK_OP2_U("UNSIGNED MOD", modBU, modU);
  return 0;
}

#endif
