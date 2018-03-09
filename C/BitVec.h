#ifndef _BITVEC_H_
#define _BITVEC_H_

// Bit-vector primitives for any (statically-known) width
// These are only used for bit vectors whose widths exceed 64

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

// Types
// =====

// Arbitrary sized bit vectors are processed in 32-bit chunks
// For testing, we use smaller (8-bit) chunks

#ifdef BITVEC_TEST
typedef uint8_t Chunk;
typedef uint16_t DoubleChunk;
#else
typedef uint32_t Chunk;
typedef uint64_t DoubleChunk;
#endif

// Chunk size in bits
#define ChunkSize (sizeof(Chunk)*8)

// Unsigned bit vectors
typedef Chunk* BU;

// Less than
inline uint8_t ltBU(BU a, BU b, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  for (int32_t i = numChunks-1; i >= 0; i--) {
    if (a[i] < b[i]) return 1;
    if (a[i] > b[i]) return 0;
  }
  return 0;
}

// Less than or equal
inline uint8_t leBU(BU a, BU b, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  for (int32_t i = numChunks-1; i >= 0; i--) {
    if (a[i] < b[i]) return 1;
    if (a[i] > b[i]) return 0;
  }
  return 1;
}

// Greater than
inline uint8_t gtBU(BU a, BU b, uint32_t w)
{
  return ltBU(b, a, w);
}

// Greater than or equal
inline uint8_t geBU(BU a, BU b, uint32_t w)
{
  return leBU(b, a, w);
}

// Equal
inline uint8_t eqBU(BU a, BU b, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  for (int32_t i = numChunks-1; i >= 0; i--)
    if (a[i] != b[i]) return 0;
  return 1;
}

// Not equal
inline uint8_t neqBU(BU a, BU b, uint32_t w)
{
  return eqBU(a, b, w) ? 0 : 1;
}

// Set bit
inline void setBitBU(BU a, uint32_t i, uint32_t bit)
{
  uint32_t c = i/ChunkSize;
  uint32_t sel = 1 << (i%ChunkSize);
  if (bit)
    a[c] = a[c] | sel;
  else
    a[c] = a[c] & ~sel;
}

// Get bit
inline uint8_t getBitBU(BU a, uint32_t i)
{
  uint32_t c = i/ChunkSize;
  uint32_t sel = 1 << (i%ChunkSize);
  return (a[c] & sel) ? 1 : 0;
}

// Convert to 64 bits
inline uint64_t fromBU(BU a, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  uint32_t maxChunks = 64/ChunkSize;
  numChunks = numChunks > maxChunks ? maxChunks : numChunks;
  uint64_t r = 0;
  for (int32_t i = numChunks-1; i >= 0; i--)
    r = (r << ChunkSize) | a[i];
  return r;
}

// Convert from 64 bits
inline void toBU(uint64_t i, BU r, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  uint32_t maxChunks = 64/ChunkSize;
  for (uint32_t j = 0; j < numChunks; j++)  {
    if (j < maxChunks) {
      r[j] = i;
      i = i >> ChunkSize;
    }
    else
      r[j] = 0;
  }
}

// Shift left core
void leftCoreBU(BU a, uint64_t i, BU r, uint32_t w, uint32_t numChunks);

// Shift left
inline void leftBU(BU a, BU b, BU r, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  leftCoreBU(a, fromBU(b, w), r, w, numChunks);
}

// Shift right core
void rightCoreBU(BU a, uint64_t i, BU r, uint32_t w, uint32_t numChunks);

// Shift right
inline void rightBU(BU a, BU b, BU r, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  rightCoreBU(a, fromBU(b, w), r, w, numChunks);
}

// Right shift and convert to 64 bits
inline uint64_t fromShiftedBU(BU a, uint32_t sh, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  Chunk tmp[numChunks];
  rightCoreBU(a, sh, tmp, w, numChunks);
  return fromBU(tmp, w);
}

// Fit the most significant chunk into range according to given width
inline Chunk fitMSCU(Chunk a, uint32_t w)
{
  uint32_t bits = w%ChunkSize;
  bits = bits == 0 ? ChunkSize : bits;
  Chunk mask = (1ul << bits) - 1;
  return a & mask;
}

// Get bits
inline void getBitsBU(BU a, BU r, uint32_t hi, uint32_t lo)
{
  uint32_t w = (hi+1)-lo;
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  rightCoreBU(a, lo, r, w, numChunks);
  r[numChunks-1] = fitMSCU(r[numChunks-1], w);
}

// Add/subtract core
void addSubBU(bool sub, BU a, BU b, BU r, Chunk carry,
                uint32_t w, uint32_t numChunks);

// Addition
inline void addBU(BU a, BU b, BU r, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  addSubBU(false, a, b, r, 0, w, numChunks);
}

// Subtraction
inline void subBU(BU a, BU b, BU r, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  addSubBU(true, a, b, r, 1, w, numChunks);
}

// Multiplication core
void mulCoreBU(BU a, BU b, BU r, uint32_t w, uint32_t numChunks);

// Multiplication
inline void mulBU(BU a, BU b, BU r, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  mulCoreBU(a, b, r, w, numChunks);
}

// Division/modulus core
void divModBU(BU a, BU b, BU q, BU r, uint32_t w, uint32_t numChunks);

// Division
inline void divBU(BU a, BU b, BU q, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  Chunk r[numChunks];
  divModBU(a, b, q, r, w, numChunks);
}

// Modulus
inline void modBU(BU a, BU b, BU r, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  Chunk q[numChunks];
  divModBU(a, b, q, r, w, numChunks);
}

// Bitwise inversion
inline void notBU(BU a, BU r, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  for (uint32_t i = 0; i < numChunks; i++) r[i] = ~a[i];
  r[numChunks-1] = fitMSCU(r[numChunks-1], w);
}

// Bitwise and
inline void andBU(BU a, BU b, BU r, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  for (uint32_t i = 0; i < numChunks; i++) r[i] = a[i] & b[i];
}

// Bitwise or
inline void orBU(BU a, BU b, BU r, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  for (uint32_t i = 0; i < numChunks; i++) r[i] = a[i] | b[i];
}

// Bitwise xor
inline void xorBU(BU a, BU b, BU r, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  for (uint32_t i = 0; i < numChunks; i++) r[i] = a[i] ^ b[i];
}

// Population count
inline uint64_t countOnesBU(BU a, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  uint64_t count = 0;
  for (uint32_t i = 0; i < numChunks; i++)
    count += __builtin_popcount(a[i]);
  return count;
}

// Population count on 64 bits
inline uint64_t countOnes(uint64_t a)
{
  return __builtin_popcountll(a);
}

// Bit concatentation
inline void concatBU(BU a, BU b, BU r, uint32_t aw, uint32_t bw)
{
  uint32_t rw = aw+bw;
  uint32_t bChunks = (bw+ChunkSize-1)/ChunkSize;
  uint32_t rChunks = (rw+ChunkSize-1)/ChunkSize;
  leftCoreBU(a, bw, r, rw, rChunks);
  for (uint32_t i = 0; i < bChunks; i++) r[i] |= b[i];
}

// Zero extend
inline void zeroExtBU(BU a, BU r, uint32_t aw, uint32_t rw)
{
  uint32_t aChunks = (aw+ChunkSize-1)/ChunkSize;
  uint32_t rChunks = (rw+ChunkSize-1)/ChunkSize;
  for (uint32_t i = 0; i < rChunks; i++) {
    if (i < aChunks)
      r[i] = a[i];
    else
      r[i] = 0;
  }
}

// Sign extend core
void signExtCoreBU(BU a, BU r, uint32_t aw, uint32_t rw,
                     uint32_t aChunks, uint32_t rChunks);

// Sign extend
inline void signExtBU(BU a, BU r, uint32_t aw, uint32_t rw)
{
  uint32_t aChunks = (aw+ChunkSize-1)/ChunkSize;
  uint32_t rChunks = (rw+ChunkSize-1)/ChunkSize;
  signExtCoreBU(a, r, aw, rw, aChunks, rChunks);
}

// Replicate bit
inline void replicateBU(uint8_t bit, BU r, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  Chunk val = bit == 0 ? 0 : ~0;
  for (uint32_t i = 0; i < numChunks; i++) r[i] = val;
  r[numChunks-1] = fitMSCU(r[numChunks-1], w);
}

// Copy
inline void copyBU(BU a, BU r, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  memcpy(r, a, numChunks * sizeof(Chunk));
}

// Print hex string
inline void printBU(BU a, uint32_t w)
{
  uint32_t numChunks = (w+ChunkSize-1)/ChunkSize;
  printf("0x");
  for (int i = numChunks-1; i >= 0; i--) printf("%08x", a[i]);
}

// Initialise RAM
template <typename T> inline void initRAM(
  const char* filename, T* ram, unsigned size)
{
  FILE* fp = fopen(filename, "rt");
  if (fp == NULL) {
    fprintf(stderr, "Can't open RAM initialistion file '%s'\n", filename);
    exit(EXIT_FAILURE);
  }
  for (unsigned i = 0; i < size; i++) ram[i] = 0;
  unsigned addr = 0;
  uint64_t data;
  while (addr < size && fscanf(fp, "%lx", &data) > 0)
    ram[addr++] = (T) data;
  fclose(fp);
}

// Initialise RAM (arbitrary width)
template <unsigned numChunks> inline void initRAMBU(
  const char* filename, uint32_t ram[][numChunks], unsigned size)
{
  FILE* fp = fopen(filename, "rt");
  if (fp == NULL) { 
    fprintf(stderr, "Can't open RAM initialistion file '%s'\n", filename);
    exit(EXIT_FAILURE);
  }
  unsigned numNibbles = numChunks * 8;
  uint32_t* nibbles = new uint32_t [numNibbles];
  for (unsigned i = 0; i < size; i++)
    for (unsigned j = 0; j < numChunks; j++)
      ram[i][j] = 0;
  unsigned addr = 0;
  unsigned nibbleCount = 0;
  while (addr < size) {
    if (nibbleCount >= numNibbles) {
      fprintf(stderr, "Hex file data wider than RAM ('%s')\n", filename);
      exit(EXIT_FAILURE);
    }
    int ch = fgetc(fp);
    if (ch == EOF || isspace(ch)) {
      // Populate RAM location
      unsigned wordCount = 0;
      for (int i = nibbleCount-1, j = 0; i >= 0; i--) {
        ram[addr][wordCount] = 
          ram[addr][wordCount] | (nibbles[i] << (4*j));
        if (j == 7) {
          j = 0;
          wordCount++;
        }
        else
          j++;
      }
      // Move to next location
      nibbleCount = 0;
      addr++;
      while (isspace(ch)) ch = fgetc(fp);
    }
    if (ch == EOF) break;
    ch = toupper(ch);
    unsigned nibble;
    if (ch >= '0' && ch <= '9') nibble = ch - '0';
    else if (ch >= 'A' && ch <= 'F') nibble = 10 + (ch - 'A');
    else {
      fprintf(stderr, "Invalid hex file '%s'\n", filename);
      exit(EXIT_FAILURE);
    }
    nibbles[nibbleCount++] = nibble;
  }
  delete [] nibbles;
  fclose(fp);
}

#endif
