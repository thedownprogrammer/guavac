// $Id: JavaFieldSignature.C,v 1.4 1996/01/22 05:37:05 geppetto Exp $
// Copyright (c) 1995  David Engberg  All rights reserved
#include "JavaFieldSignature.h"
#include <iostream.h>

//
//  Method name : CJavaFieldSignature
//  Description : Default constructor.
//
CJavaFieldSignature::CJavaFieldSignature()
{
}

//
//  Method name : CJavaFieldSignature
//  Description : Constructs a field signature from its constituent type and
//    name.  The type should never be void.
//
CJavaFieldSignature::CJavaFieldSignature(const CJavaTypeSignature& type,
		      const unicode_string& name)
  : fType(type), fFieldName(name)
{
  assert(fType.GetBaseType() != CJavaTypeSignature::Void);
  fHashKey = ::Hash(fFieldName) + fType.Hash();
}

//
//  Method name : CJavaFieldSignature
//  Description : Copy constructor.
//
CJavaFieldSignature::CJavaFieldSignature(const CJavaFieldSignature& source)
  : fFieldName(source.fFieldName),
    fType(source.fType),
    fHashKey(source.fHashKey)
{
}

//
//  Method name : ~CJavaFieldSignature
//  Description : Destructor
//
CJavaFieldSignature::~CJavaFieldSignature()
{
}

//
//  Method name : operator=
//  Description : Assignment operator.
//
CJavaFieldSignature&
CJavaFieldSignature::operator=(const CJavaFieldSignature& source)
{
  if (&source != this) {
    fFieldName = source.fFieldName;
    fType = source.fType;
    fHashKey = source.fHashKey;
  }
  return *this;
}


//
//  Method name : Compare
//  Description : Returns 0 if the other field signature is identical to this
//    one, returns -1 if this one is greater, and 1 if this one is less than
//    the other.
//
int
CJavaFieldSignature::Compare(const CJavaFieldSignature& other) const
{
  int result = other.fHashKey - fHashKey;
  if (result == 0) {
    if ((result = fFieldName.compare(other.fFieldName)) == 0) {
      result = fType.Compare(other.fType);
    }
  }
  return result;
}

//
//  Method name : operator==
//  Description : Compares two field signatures for equality.
//
bool
CJavaFieldSignature::operator==(const CJavaFieldSignature& other) const
{
  return Compare(other) == 0;
}

//
//  Method name : operator<
//  Description : Returns true if this field signature does not match the
//    argument and the hash value for this signature is lower than the other.
//
bool
CJavaFieldSignature::operator<(const CJavaFieldSignature& other) const
{
  return Compare(other) > 0;
}

//
//  Method name : Initialize
//  Description : This function chews over the input strings and tries to
//     interpret it as a field signature.  If it is successful, then
//     this value is set appropriately and 'true' is returned.  If
//     not, then this value becomes invalid and a false value is given.
//
bool
CJavaFieldSignature::Initialize(const unicode_string& fieldName,
				const unicode_string& signatureString)
{
  fFieldName = fieldName;
  unicode_string::const_iterator begin = signatureString.begin();
  bool successful = fType.ParseSignatureString(begin, signatureString.end());
  fHashKey = ::Hash(fieldName) + fType.Hash();
  return successful;
}


//
//  Method name : Disassemble
//  Description : Dumps out a human-readable description of this class to
//    the provided stream.  For debugging purposes.
//
void
CJavaFieldSignature::Disassemble(ostream& toStream) const
{
  fType.Disassemble(toStream);
  toStream << " " << ::UnicodeToUTF(fFieldName);
}
