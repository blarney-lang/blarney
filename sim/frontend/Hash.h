// Simple hash table with string keys

#ifndef _HASH_H_
#define _HASH_H_

#include <string.h>
#include "Seq.h"

template <class T> struct KeyValue {
  char* key;
  T value;
};

template <class T> KeyValue<T> keyValue(char* key, T value) {
  KeyValue<T> kv;
  kv.key = key;
  kv.value = value;
  return kv;
}

template <class T> class Hash
{
  private:
    // Initialisation
    void init(int numberOfBuckets)
    {
      numBuckets    = numberOfBuckets;
      buckets       = new SmallSeq< KeyValue<T> > [numBuckets];
    }

    // Hash function by Dan Bernstein
    unsigned long hash(unsigned char *str)
    {
      unsigned long hash = 5381;
      int c;

      while (c = *str++)
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

      return hash % numBuckets;
    }

  public:
    int numBuckets;
    Seq< KeyValue<T> >* buckets;

    // Constructors
    Hash(int numberOfBuckets) { init(numberOfBuckets); }

    // Copy constructor
    Hash(const Hash<T>& hash) {
      init(hash.numBuckets);
      for (int i = 0; i < hash.numBuckets; i++)
        buckets[i] = hash.buckets[i];
    }

    // Insertion
    void insert(char* key, T value) {
      int h = hash((unsigned char*) key);
      Seq< KeyValue<T> >* bucket = &buckets[h];
      for (int i = 0; i < bucket->numElems; i++) {
        if (strcmp(key, bucket->elems[i].key) == 0) {
          bucket->elems[i].value = value;
          return;
        }
      }
      bucket->append(keyValue(key, value));
    }

    // Lookup
    bool lookup(char* key, T* value) {
      unsigned long h = hash((unsigned char*) key);
      Seq< KeyValue<T> >* bucket = &buckets[h];
      for (int i = 0; i < bucket->numElems; i++) {
        if (strcmp(key, bucket->elems[i].key) == 0) {
          *value = bucket->elems[i].value;
          return true;
        }
      }
      return false;
    }

    // Membership
    bool member(char* key) {
      T tmp;
      return lookup(key, &tmp);
    }

    // Destructor
    ~Hash()
    {
      delete [] buckets;
    }
};

#endif
