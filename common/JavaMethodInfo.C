// $Id: JavaMethodInfo.C,v 1.6 1997/06/13 18:18:49 geppetto Exp $
// Copyright (c) 1995  David Engberg  All rights reserved
#pragma implementation
#include "JavaMethodInfo.h"
#include "JavaCodeAttribute.h"
#include "JavaExceptionsTable.h"
#include <iostream.h>

//
//  Method name : CJavaMethodInfo
//  Description : Default constructor.
//
CJavaMethodInfo::CJavaMethodInfo()
  : fCodeAttribute(0),
    fExceptions(0),
    fReferenceCount(0),
    fDeprecated(false)
{
}

//
//  Method name : ~CJavaMethodInfo
//  Description : Destructor
//
CJavaMethodInfo::~CJavaMethodInfo()
{
  assert(fReferenceCount == 0);
  delete fCodeAttribute;
  delete fExceptions;
}


//
//  Method name : ParseBuffer
//  Description : Parses the input buffer at the point specified and tries
//    to interpret it as a method information entry in a Java class file.
//    If successful, a new java method information node is returned,
//    otherwise, 0 is returned. If the 'interfaceOnly' flag is true,
//    then the method is loaded without any code attributes for the
//    methods.  This can be used whenever the goal is to get the signatures
//    of methods (like during compilation) wihtout the implementations.
//
CJavaMethodInfo*
CJavaMethodInfo::ParseBuffer(string::const_iterator& javaBuffer,
			     const CJavaClassFile& classFile,
			     bool interfaceOnly)
{
  CJavaMethodInfo* result = new CJavaMethodInfo;
  result->fAccessFlags.SetFlags(CJavaClassFile::ReadJavaU2(javaBuffer));

  JavaConstantIndex nameIndex = CJavaClassFile::ReadJavaU2(javaBuffer);
  const CJavaAscizConstant* tempAscizConstant =
    DYNAMIC_CAST(CJavaAscizConstant, classFile.LookupConstant(nameIndex));
  if (tempAscizConstant == 0) {
    delete result; return 0;
  }
  unicode_string methodName = tempAscizConstant->GetUnicodeString();
  JavaConstantIndex signatureIndex = CJavaClassFile::ReadJavaU2(javaBuffer);
  tempAscizConstant = 
    DYNAMIC_CAST(CJavaAscizConstant, classFile.LookupConstant(signatureIndex));
  if (tempAscizConstant == 0) {
    delete result; return 0;
  }
  unicode_string signatureString = tempAscizConstant->GetUnicodeString();
  if (! result->fSignature.Initialize(methodName, signatureString)) {
    delete result; return 0;
  }

  unsigned long attributeCount = CJavaClassFile::ReadJavaU2(javaBuffer);
  while (attributeCount-- > 0) {
    CJavaAttribute* attribute =
      CJavaAttribute::ParseBuffer(javaBuffer, classFile);
    if (attribute != 0) {
      CJavaCodeAttribute* codeAttribute =
	DYNAMIC_CAST(CJavaCodeAttribute, attribute);
      if (codeAttribute != 0 && !interfaceOnly) {
	delete result->fCodeAttribute;
	result->fCodeAttribute = codeAttribute;
      } else {
	CJavaExceptionsTable* exceptions =
	  DYNAMIC_CAST(CJavaExceptionsTable, attribute);
	if (exceptions != 0) {
	  delete result->fExceptions;
	  result->fExceptions = exceptions;
	} else {
	  if (DYNAMIC_CAST(CJavaDeprecatedAttribute, attribute) != 0) {
	    result->fDeprecated = true;
	  }
	  delete attribute;
	}
      }
    }
  }
  return result;
}


//
//  Method name : Disassemble
//  Description : Dumps out a human-readable description of this class to
//    the provided stream.  For debugging purposes.
//
void
CJavaMethodInfo::Disassemble(ostream& toStream) const
{
  fSignature.Disassemble(toStream);
  toStream << " ";
  fAccessFlags.Disassemble(toStream);
  if (fDeprecated) {
    toStream << "  (Deprecated)";
  }
  if (fExceptions != 0) {
    toStream << endl << "    ";
    fExceptions->Disassemble(toStream);
  }
  if (fCodeAttribute != 0) {
    fCodeAttribute->Disassemble(toStream);
  }
}

//
//  Method name : Compile
//  Description : Dumps this method out as a sequence of bytes formatted in
//    the manner specified by the Java VM spec.
//
string
CJavaMethodInfo::Compile(CJavaClassFile& inClass) const
{
  string result;
  CJavaClassFile::WriteJavaU2(result, fAccessFlags.GetJavaFlags());
  CJavaClassFile::WriteJavaU2(result,
			inClass.AddAscizConstant(fSignature.GetName()));
  CJavaClassFile::WriteJavaU2(result,
		      inClass.AddAscizConstant(fSignature.CompileType()));
  unsigned short attributeCount = 0;
  attributeCount += (fCodeAttribute != 0) ? 1 : 0;
  attributeCount += fDeprecated ? 1 : 0;
  if (fExceptions != 0 && fExceptions->Size() > 0) {
    attributeCount += 1;
  }
  CJavaClassFile::WriteJavaU2(result, attributeCount);
  if (fCodeAttribute != 0) {
    result += fCodeAttribute->Compile(inClass);
  }
  if (fExceptions != 0 && fExceptions->Size()) {
    result += fExceptions->Compile(inClass);
  }
  if (fDeprecated) {
    CJavaClassFile::WriteJavaU2(result,
				inClass.AddAscizConstant("Deprecated"));
    CJavaClassFile::WriteJavaU4(result, 0);
  }
  return result;
}

//
//  Method name : AddReference
//  Description : Increments this object's reference count.
//
void
CJavaMethodInfo::AddReference() const
{
  // casting away const
  ((CJavaMethodInfo*)this)->fReferenceCount++;
}

//
//  Method name : RemoveReference
//  Description : Decrements this object's reference count.  Deletes the object
//    if the count drops to 0.
//
void
CJavaMethodInfo::RemoveReference() const
{
  assert(fReferenceCount > 0);
  // casting away const
  ((CJavaMethodInfo*)this)->fReferenceCount--;
  if (fReferenceCount == 0) {
    delete this;
  }
}

//
//  Method name : ThrowsExceptions
//  Description : This method returns true if this method throws any exceptions
//
bool
CJavaMethodInfo::ThrowsExceptions() const
{
  return fExceptions != 0 && fExceptions->Size() != 0;
}

//
//  Method name : ThrowsBegin
//  Description : This method returns an iterator over the exceptions thrown
//    by this method.
//
deque<unicode_string>::const_iterator
CJavaMethodInfo::ThrowsBegin() const
{
  if (fExceptions == 0) {
    // casting away const
    ((CJavaMethodInfo*)this)->fExceptions = new CJavaExceptionsTable;
  }
  return fExceptions->ThrowsBegin();
}

//
//  Method name : ThrowsEnd
//  Description : This returns an iterator to the end of the list of exceptions
//    thrown by this method.
//
deque<unicode_string>::const_iterator
CJavaMethodInfo::ThrowsEnd() const
{
  if (fExceptions == 0) {
    // casting away const
    ((CJavaMethodInfo*)this)->fExceptions = new CJavaExceptionsTable;
  }
  return fExceptions->ThrowsEnd();
}

//
//  Method name : IsDeprecated
//  Description : Returns true if this method is deprecated from use.
//
bool
CJavaMethodInfo::IsDeprecated() const
{
  return fDeprecated;
}
