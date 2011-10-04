// $Id: JavaFieldInfo.C,v 1.6 1997/06/13 18:18:49 geppetto Exp $
// Copyright (c) 1995  David Engberg  All rights reserved
#include "JavaFieldInfo.h"
#include "JavaAttribute.h"
#include "JavaClassFile.h"
#include <iostream.h>

//
//  Method name : CJavaFieldInfo
//  Description : Default constructor.
//
CJavaFieldInfo::CJavaFieldInfo()
  : fConstantValue(0),
    fDeprecated(false)
{
}

//
//  Method name : CJavaFieldInfo
//  Description : Constructs a field info class out of its constituent
//    parts.
//
CJavaFieldInfo::CJavaFieldInfo(const CJavaAccessFlags& modifiers,
			       const CJavaFieldSignature& signature,
			       CJavaConstantValueAttribute* constValue,
			       bool deprecated)
   : fSignature(signature),
     fAccessFlags(modifiers),
     fConstantValue(constValue),
     fDeprecated(deprecated)
{
}

//
//  Method name : CJavaFieldInfo
//  Description : Copy constructor.
//
CJavaFieldInfo::CJavaFieldInfo(const CJavaFieldInfo& source)
  : fSignature(source.fSignature),
    fAccessFlags(source.fAccessFlags),
    fConstantValue(0),
    fDeprecated(source.fDeprecated)
{
  if (source.fConstantValue != 0) {
    fConstantValue = new CJavaConstantValueAttribute(*source.fConstantValue);
  }
}

//
//  Method name : ~CJavaFieldInfo
//  Description : Destructor
//
CJavaFieldInfo::~CJavaFieldInfo()
{
  delete fConstantValue;
}

//
//  Method name : operator=
//  Description : Assignment operator
//
CJavaFieldInfo&
CJavaFieldInfo::operator=(const CJavaFieldInfo& source)
{
  if (&source != this) {
    fSignature = source.fSignature;
    fAccessFlags = source.fAccessFlags;
    fConstantValue = (source.fConstantValue == 0) ? 0 :
                       new CJavaConstantValueAttribute(*source.fConstantValue);
  }
  return *this;
}

//
//  Method name : ParseBuffer
//  Description : Parses the input buffer at the point specified and tries
//    to interpret it as a field information entry.  If it is successful,
//    the resulting field info is returned, otherwise 0 is returned.
//
CJavaFieldInfo*
CJavaFieldInfo::ParseBuffer(string::const_iterator& javaBuffer,
			    const CJavaClassFile& classFile)
{
  CJavaFieldInfo* result = new CJavaFieldInfo;
  result->fAccessFlags.SetFlags(CJavaClassFile::ReadJavaU2(javaBuffer));

  JavaConstantIndex nameIndex = CJavaClassFile::ReadJavaU2(javaBuffer);
  const CJavaAscizConstant* tempAscizConstant =
    DYNAMIC_CAST(CJavaAscizConstant, classFile.LookupConstant(nameIndex));
  if (tempAscizConstant == 0) {
    delete result; return 0;
  }
  unicode_string fieldName = tempAscizConstant->GetUnicodeString();
  JavaConstantIndex signatureIndex = CJavaClassFile::ReadJavaU2(javaBuffer);
  tempAscizConstant = 
    DYNAMIC_CAST(CJavaAscizConstant, classFile.LookupConstant(signatureIndex));
  if (tempAscizConstant == 0) {
    delete result; return 0;
  }
  unicode_string signatureString = tempAscizConstant->GetUnicodeString();
  if (! result->fSignature.Initialize(fieldName, signatureString)) {
    delete result; return 0;
  }

  unsigned long attributeCount = CJavaClassFile::ReadJavaU2(javaBuffer);
  while (attributeCount-- > 0) {
    CJavaAttribute* attribute =
      CJavaAttribute::ParseBuffer(javaBuffer, classFile);
    if (attribute != 0) {
      CJavaConstantValueAttribute* constantValue =
	DYNAMIC_CAST(CJavaConstantValueAttribute, attribute);
      if (constantValue != 0) {
	delete result->fConstantValue;
	result->fConstantValue = constantValue;
      } else {
	if (DYNAMIC_CAST(CJavaDeprecatedAttribute, attribute) != 0) {
	  result->fDeprecated = true;
	}
	delete attribute;
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
CJavaFieldInfo::Disassemble(ostream& toStream) const
{
  fSignature.Disassemble(toStream);
  toStream << " ";
  fAccessFlags.Disassemble(toStream);
  if (fConstantValue != 0) {
    toStream << " CONSTANT index: " << fConstantValue->GetConstantValue();
  }
  if (fDeprecated) {
    toStream << "  (Deprecated)";
  }
}

//
//  Method name : Compile
//  Description : This function produces a string representing this field
//    information in a format specified by the Java VM spec.
//
string
CJavaFieldInfo::Compile(CJavaClassFile& inClass) const
{
  string result;
  CJavaClassFile::WriteJavaU2(result, fAccessFlags.GetJavaFlags());
  CJavaClassFile::WriteJavaU2(result,
			inClass.AddAscizConstant(fSignature.GetFieldName()));
  CJavaClassFile::WriteJavaU2(result,
		inClass.AddAscizConstant(fSignature.GetType().Compile()));
  unsigned short attributeCount = 0;
  attributeCount += (fConstantValue != 0) ? 1 : 0;
  attributeCount += fDeprecated ? 1 : 0;
  CJavaClassFile::WriteJavaU2(result, attributeCount);
  if (fConstantValue != 0) {
    CJavaClassFile::WriteJavaU2(result,
				inClass.AddAscizConstant("ConstantValue"));
    CJavaClassFile::WriteJavaU4(result, 2);
    CJavaClassFile::WriteJavaU2(result,
				fConstantValue->GetConstantValue());
  }
  if (fDeprecated) {
    CJavaClassFile::WriteJavaU2(result,
				inClass.AddAscizConstant("Deprecated"));
    CJavaClassFile::WriteJavaU4(result, 0);
  }
  return result;			      
}

//
//  Method name : GetConstantIndex
//  Description : Returns the index in the constant pool that states what the
//    constant value of this field is.
//
JavaConstantIndex
CJavaFieldInfo::GetConstantIndex() const
{
  assert(IsConstant());
  return fConstantValue->GetConstantValue();
}

//
//  Method name : SetConstantIndex
//  Description : This method is used to specify a constant value to be used
//    for this field.  The index is to the constant pool of this field's class.
//
void
CJavaFieldInfo::SetConstantIndex(JavaConstantIndex index)
{
  assert(fAccessFlags.fFinal != 0);
  if (fConstantValue != 0) {
    delete fConstantValue;
  }
  fConstantValue = new CJavaConstantValueAttribute(index);
}

//
//  Method name : IsDeprecated
//  Description : Returns true if this method is deprecated from use.
//
bool
CJavaFieldInfo::IsDeprecated() const
{
  return fDeprecated;
}
