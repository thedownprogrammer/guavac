// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: JavaAttribute.C,v 1.5 1997/11/10 00:47:24 geppetto Exp $
#pragma implementation
#include "JavaAttribute.h"
#include "JavaCodeAttribute.h"
#include "JavaClassFile.h"
#include "JavaConstant.h"
#include "unicode_string.h"
#include "JavaExceptionsTable.h"
#include "InnerClassesTable.h"

//
//  Method name : CJavaAttribute
//  Description : Default constructor.
//
CJavaAttribute::CJavaAttribute()
{
}

//
//  Method name : ~CJavaAttribute
//  Description : Destructor
//
CJavaAttribute::~CJavaAttribute()
{
}


//
//  Method name : ParseBuffer
//  Description : Reads in bytes from the current buffer position to
//    determine which concrete subclass to create, and then manufactures one
//    of these and returns it to the caller.  If the buffer pointer doesn't
//    indicate the start of a valid known attribute, then this fails and
//    returns 0.
//
CJavaAttribute*
CJavaAttribute::ParseBuffer(string::const_iterator& buffer,
			    const CJavaClassFile& classFile)
{
  JavaConstantIndex attributeNameIndex = CJavaClassFile::ReadJavaU2(buffer);
  const CJavaAscizConstant* attributeNameString =
    DYNAMIC_CAST(CJavaAscizConstant,
		 classFile.LookupConstant(attributeNameIndex));
  CJavaAttribute* result = 0;
  if (attributeNameString != 0) {
    unicode_string javaString = attributeNameString->GetUnicodeString();
    string charString = ::UnicodeToString(javaString);
    unsigned long attributeLength = CJavaClassFile::ReadJavaU4(buffer);
    // replace this with a non-linear lookup table when time permits.
    string::const_iterator endPosition = buffer + attributeLength;
    if (charString.compare("ConstantValue") == 0) {
      if (attributeLength == 2) {
	JavaConstantIndex valueIndex = CJavaClassFile::ReadJavaU2(buffer);
	result = new CJavaConstantValueAttribute(valueIndex);
      } else {
	buffer += attributeLength;
      }
    } else if (charString.compare("SourceFile") == 0) {
      if (attributeLength == 2) {
	JavaConstantIndex fileIndex = CJavaClassFile::ReadJavaU2(buffer);
	CJavaAscizConstant* fileNameConstant = DYNAMIC_CAST(CJavaAscizConstant,
					 classFile.LookupConstant(fileIndex));
	if (fileNameConstant != 0) {
	  unicode_string fileName = fileNameConstant->GetUnicodeString();
	  result = new CJavaSourceFileAttribute(fileName);
	}
      } else {
	buffer += attributeLength;
      }
    } else if (charString.compare("Code") == 0) {
      result = CJavaCodeAttribute::ParseBuffer(buffer, classFile);
    } else if (charString.compare("Exceptions") == 0) {
      result = CJavaExceptionsTable::ParseBuffer(buffer, classFile);
    } else if (charString.compare("LineNumberTable") == 0) {
      result = CJavaLineNumberTable::ParseBuffer(buffer);
    } else if (charString.compare("LocalVariableTable") == 0) {
      result = CJavaLocalVariableTable::ParseBuffer(buffer);
    } else if (charString.compare("Deprecated") == 0) {
      result = new CJavaDeprecatedAttribute();
      buffer += attributeLength;
    } else if (charString.compare("InnerClasses") == 0) {
      result = CInnerClassesTable::ParseBuffer(buffer, classFile);
    } else {
      buffer += attributeLength;
    }
    if (buffer != endPosition) {
      delete result;
      result = 0;
    }
  }
  return result;
}


//
//  Method name : CJavaConstantValueAttribute
//  Description : Copy constructor
//
CJavaConstantValueAttribute::CJavaConstantValueAttribute(
				  const CJavaConstantValueAttribute& source)
  : fValueIndex(source.fValueIndex)
{
}

//
//  Method name : ~CJavaConstantValueAttribute
//  Description : Destructor.
//
CJavaConstantValueAttribute::~CJavaConstantValueAttribute()
{
}

//
//  Method name : CJavaConstantValueAttribute
//  Description : Constructs a constant value attribute, given the index
//     for the constant value.
//
CJavaConstantValueAttribute::CJavaConstantValueAttribute(JavaConstantIndex i)
  : fValueIndex(i)
{
}


//
//  Method name : ~CJavaSourceFileAttribute
//  Description : Destructor.
//
CJavaSourceFileAttribute::~CJavaSourceFileAttribute()
{
}

//
//  Method name : CJavaSourceFileAttribute
//  Description : Constructs a constant value attribute, given the index
//     for the constant value.
//
CJavaSourceFileAttribute::CJavaSourceFileAttribute(const unicode_string& name)
  : fFileName(name)
{
}


//
//  Method name : CJavaDeprecatedAttribute
//  Description : Constructor.
//
CJavaDeprecatedAttribute::CJavaDeprecatedAttribute()
{
}

//
//  Method name : ~CJavaDeprecatedAttribute
//  Description : Destructor.
//
CJavaDeprecatedAttribute::~CJavaDeprecatedAttribute()
{
}

