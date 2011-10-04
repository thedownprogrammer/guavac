// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: Bitset.C,v 1.1 1996/03/31 19:34:21 geppetto Exp $
#pragma implementation
#include "Bitset.h"
#include <cassert>
#include <iostream.h>

const unsigned long CBitset::kBitsPerLong = 8 * sizeof(unsigned long);

//
//  Method name : CBitset
//  Description : Constructs a bitset of the provided size.  All elements
//    will be initialized to a value of 'false,' indicating that the empty set.
//
CBitset::CBitset(unsigned long size)
  : fSize(size)
{
  if (fSize > kBitsPerLong) {
    fDynamicVector = new unsigned long[DynamicLongLength()];
    for (int i = 0; i < DynamicLongLength(); ++i) {
      fDynamicVector[i] = 0;
    }
  } else {
    fShortVector = 0;
  }
}

//
//  Method name : CBitset
//  Description : Copy constructor.
//
CBitset::CBitset(const CBitset& source)
  : fSize(source.fSize)
{
  if (fSize > kBitsPerLong) {
    fDynamicVector = new unsigned long[DynamicLongLength()];
    for (int i = 0; i < DynamicLongLength(); ++i) {
      fDynamicVector[i] = source.fDynamicVector[i];
    }
  } else {
    fShortVector = source.fShortVector;
  }
}

//
//  Method name : ~CBitset
//  Description : Destructor
//
CBitset::~CBitset()
{
  if (fSize > kBitsPerLong) {
    delete fDynamicVector;
  }
}

//
//  Method name : operator=
//  Description : Assignment operator.
//
CBitset&
CBitset::operator=(const CBitset& source)
{
  if (&source != this) {
    if (fSize > kBitsPerLong) {
      delete fDynamicVector;
    }
    fSize = source.fSize;
    if (fSize > kBitsPerLong) {
      fDynamicVector = new unsigned long[DynamicLongLength()];
      for (int i = 0; i < DynamicLongLength(); ++i) {
	fDynamicVector[i] = source.fDynamicVector[i];
      }
    } else {
      fShortVector = source.fShortVector;
    }
  }
  return *this;
}

//
//  Method name : operator==
//  Description : Returns true if this bitvector has the same members set as
//    the provided set.  This call can only be applied to vectors of the same
//    size.
//
bool
CBitset::operator==(const CBitset& other) const
{
  assert(fSize == other.fSize);
  bool equal = &other == this;
  if (!equal) {
    if (fSize > kBitsPerLong) {
      equal = true;
      for (int i = 0; equal && i < DynamicLongLength(); ++i) {
	equal = fDynamicVector[i] == other.fDynamicVector[i];
      }
    } else {
      equal = fShortVector == other.fShortVector;
    }
  }
  return equal;
}

//
//  Method name : operator!=
//  Description : Returns true if this bitvector does not have the same
//    members set as the provided set.
//    This call can only be applied to vectors of the same size.
//
bool
CBitset::operator!=(const CBitset& other) const
{
  return !(operator==(other));
}

//
//  Method name : GetElement
//  Description : Returns true if the element at the provided index is set.
//    guaranteed to be a constant-time operation.
//
bool
CBitset::GetElement(unsigned long index) const
{
  unsigned long mask = 1 << (index % kBitsPerLong);
  bool element;
  if (fSize > kBitsPerLong) {
    element = (fDynamicVector[index / kBitsPerLong] & mask) != 0;
  } else {
    element = (fShortVector & mask) != 0;
  }
  return element;
}

//
//  Method name : SetElement
//  Description : Sets the value at the provided index in this bitvector to
//    true, indicating that this element is in the set.
//    Guaranteed to be a constant-time operation.
//
void
CBitset::SetElement(unsigned long index)
{
  unsigned long mask = 1 << (index % kBitsPerLong);
  if (fSize > kBitsPerLong) {
    fDynamicVector[index / kBitsPerLong] |= mask;
  } else {
    fShortVector |= mask;
  }
}

//
//  Method name : UnsetElement
//  Description : Sets the value at the provided index in this bitvector to
//    false, indicating that this element is not in the set.
//    Guaranteed to be a constant-time operation.
//
void
CBitset::UnsetElement(unsigned long index)
{
  unsigned long mask = ~(1 << (index % kBitsPerLong));
  if (fSize > kBitsPerLong) {
    fDynamicVector[index / kBitsPerLong] &= mask;
  } else {
    fShortVector &= mask;
  }
}

//
//  Method name : operator|=
//  Description : Performs a logical OR over all elements of this bitset with
//    another bitset and assigns the results to this bitset.  Returns a
//    a reference to this bitset.  In set-terms, this computes the union of
//    the two sets.
//    Assumes that these are sets over the same size universe (the bitvectors
//    have the same length).
//
CBitset&
CBitset::operator|=(const CBitset& other)
{
  assert(fSize == other.fSize);
  if (fSize > kBitsPerLong) {
    for (int i = 0; i < DynamicLongLength(); ++i) {
      fDynamicVector[i] |= other.fDynamicVector[i];
    }
  } else {
    fShortVector |= other.fShortVector;
  }
  return *this;
}

//
//  Method name : operator&=
//  Description : Performs a logical AND over all elements of this bitset with
//    another bitset and assigns the results to this bitset.  Returns a
//    a reference to this bitset. In set-terms, this computes the intersection
//    of the two sets.
//    Assumes that these are sets over the same size universe (the bitvectors
//    have the same length).
//
CBitset&
CBitset::operator&=(const CBitset& other)
{
  assert(fSize == other.fSize);
  if (fSize > kBitsPerLong) {
    for (int i = 0; i < DynamicLongLength(); ++i) {
      fDynamicVector[i] &= other.fDynamicVector[i];
    }
  } else {
    fShortVector &= other.fShortVector;
  }
  return *this;
}

//
//  Method name : operator|
//  Description : Performs a logical OR over all elements of this bitset with
//    another bitset and returns a new bitset that is formed from this
//    union.
//    Assumes that these are sets over the same size universe (the bitvectors
//    have the same length).
//
CBitset
CBitset::operator|(const CBitset& other) const
{
  assert(fSize == other.fSize);
  CBitset result = *this;
  result |= other;
  return result;
}

//
//  Method name : operator&
//  Description : Performs a logical AND over all elements of this bitset with
//    another bitset and returns a new bitset that is formed from this
//    intersection.
//    Assumes that these are sets over the same size universe (the bitvectors
//    have the same length).
//
CBitset
CBitset::operator&(const CBitset& other) const
{
  assert(fSize == other.fSize);
  CBitset result = *this;
  result &= other;
  return result;
}

//
//  Method name : PrintShortDebug
//  Description : Dumps out a little human-readable output that displays the
//    state of this bitset.
//
void
CBitset::PrintShortDebug(ostream& toStream) const
{
  toStream << fSize << ":{";
  bool first = true;
  for (unsigned long element = 0; element < GetSize(); ++element) {
    if (GetElement(element)) {
      if (!first) {
	toStream << ", ";
      }
      toStream << element;
      first = false;
    }
  }
  toStream << "}";
}
