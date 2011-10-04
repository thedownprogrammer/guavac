// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: Bitset.h,v 1.1 1996/03/31 19:34:21 geppetto Exp $
#ifndef _Bitset_h
#define _Bitset_h
#pragma interface

class ostream;

//
//  Class name : CBitset
//  Description : This class provides a simple and efficient implementation of
//    the standard set of boolean flags.  It provides operations that allow
//    to bitsets over the same universe (think of them as bitvectors of the
//    same length) to be compared using standard logical operations.
//    The implementation should provide constant-time operations for sets of
//    32 elements or less and reasonably fast linear operations over longer
//    sets.
//
class CBitset {
public:
  CBitset(unsigned long size);
  CBitset(const CBitset& source);
  ~CBitset();
  CBitset& operator=(const CBitset& source);
  bool operator==(const CBitset& other) const;
  bool operator!=(const CBitset& other) const;
  CBitset& operator|=(const CBitset& other);
  CBitset& operator&=(const CBitset& other);
  CBitset operator&(const CBitset& other) const;
  CBitset operator|(const CBitset& other) const;

  unsigned long GetSize() const { return fSize; }
  bool GetElement(unsigned long index) const;
  void SetElement(unsigned long index);
  void UnsetElement(unsigned long index);

  void PrintShortDebug(ostream& toStream) const;
private:
  static const unsigned long kBitsPerLong;
  unsigned long DynamicLongLength() const { return fSize / kBitsPerLong + 1; }

  unsigned long fSize;
  union {
    unsigned long fShortVector;
    unsigned long* fDynamicVector;
  };
};

#endif
