// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: JavaConstant.C,v 1.5 1996/05/03 23:40:05 geppetto Exp $
#include "JavaConstant.h"
#include "JavaClassFile.h"
#include <iostream.h>
#include "config.h"

//
//  Method name : ~CJavaConstant
//  Description : Destructor
//
CJavaConstant::~CJavaConstant()
{ 
}


//============================ CJavaClassConstant =============================
//
//  Method name : CJavaClassConstant
//  Description : Constructor.
//
CJavaClassConstant::CJavaClassConstant(JavaConstantIndex nameIndex)
  : fNameIndex(nameIndex)
{
}

//
//  Method name : ~CJavaClassConstant
//  Description : Destructor
//
CJavaClassConstant::~CJavaClassConstant()
{
}

//
//  Method name : operator==
//  Description : Compares two constants for equality.
//
bool
CJavaClassConstant::operator==(const CJavaConstant& other) const
{
  CJavaClassConstant* otherClass = DYNAMIC_CAST(CJavaClassConstant, &other);
  return otherClass != 0 && fNameIndex == otherClass->fNameIndex;
}

//
//  Method name : GetConstantType
//  Description : Returns the enumerated constant indicating the type marker
//    used for this constant in the Java class file.
//
CJavaConstant::ConstantType
CJavaClassConstant::GetConstantType() const
{
  return kClass;
}

//
//  Method name : GetWidth
//  Description : Returns the number of slots this constant takes up in the
//    constant pool table.
//
unsigned long
CJavaClassConstant::GetWidth() const
{
  return 1;
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable explanation of this constant's
//    type and value to the provided stream.  For debugging, mainly.
//
void
CJavaClassConstant::Disassemble(ostream& toStream) const
{
  toStream << "Class constant index " << fNameIndex;
}

//
//  Method name : Compile
//  Description : Writes out this constant to the provided stream in the
//    format specified by the Java VM spec.
//
void
CJavaClassConstant::Compile(ostream& toStream) const
{
  toStream << (unsigned char)GetConstantType();
  CJavaClassFile::WriteJavaU2(toStream, fNameIndex);
}

//============================ CJavaFieldConstant =============================
//
//  Method name : CJavaFieldConstant
//  Description : Constructor.
//
CJavaFieldConstant::CJavaFieldConstant(JavaConstantIndex classIndex,
				       JavaConstantIndex nameTypeIndex)
  : fClassIndex(classIndex), fNameTypeIndex(nameTypeIndex)
{
}

//
//  Method name : ~CJavaFieldConstant
//  Description : Destructor
//
CJavaFieldConstant::~CJavaFieldConstant()
{
}

//
//  Method name : operator==
//  Description : Compares two constants for equality.
//
bool
CJavaFieldConstant::operator==(const CJavaConstant& other) const
{
  CJavaFieldConstant* otherClass = DYNAMIC_CAST(CJavaFieldConstant, &other);
  return otherClass != 0 &&
    fClassIndex == otherClass->fClassIndex &&
    fNameTypeIndex == otherClass->fNameTypeIndex;
}

//
//  Method name : GetConstantType
//  Description : Returns the enumerated constant indicating the type marker
//    used for this constant in the Java class file.
//
CJavaConstant::ConstantType
CJavaFieldConstant::GetConstantType() const
{
  return kField;
}

//
//  Method name : GetWidth
//  Description : Returns the number of slots this constant takes up in the
//    constant pool table.
//
unsigned long
CJavaFieldConstant::GetWidth() const
{
  return 1;
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable explanation of this constant's
//    type and value to the provided stream.  For debugging, mainly.
//
void
CJavaFieldConstant::Disassemble(ostream& toStream) const
{
  toStream << "Field: class " << fClassIndex << ", name and type "
	   << fNameTypeIndex;
}

//
//  Method name : Compile
//  Description : Writes out this constant to the provided stream in the
//    format specified by the Java VM spec.
//
void
CJavaFieldConstant::Compile(ostream& toStream) const
{
  toStream << (unsigned char)GetConstantType();
  CJavaClassFile::WriteJavaU2(toStream, fClassIndex);
  CJavaClassFile::WriteJavaU2(toStream, fNameTypeIndex);
}

//=========================== CJavaMethodConstant =============================
//
//  Method name : CJavaMethodConstant
//  Description : Constructor.
//
CJavaMethodConstant::CJavaMethodConstant(JavaConstantIndex classIndex,
				       JavaConstantIndex nameTypeIndex)
  : fClassIndex(classIndex), fNameTypeIndex(nameTypeIndex)
{
}

//
//  Method name : ~CJavaMethodConstant
//  Description : Destructor
//
CJavaMethodConstant::~CJavaMethodConstant()
{
}

//
//  Method name : operator==
//  Description : Compares two constants for equality.
//
bool
CJavaMethodConstant::operator==(const CJavaConstant& other) const
{
  CJavaMethodConstant* otherClass = DYNAMIC_CAST(CJavaMethodConstant, &other);
  return otherClass != 0 &&
    fClassIndex == otherClass->fClassIndex &&
    fNameTypeIndex == otherClass->fNameTypeIndex;
}

//
//  Method name : GetConstantType
//  Description : Returns the enumerated constant indicating the type marker
//    used for this constant in the Java class file.
//
CJavaConstant::ConstantType
CJavaMethodConstant::GetConstantType() const
{
  return kMethod;
}

//
//  Method name : GetWidth
//  Description : Returns the number of slots this constant takes up in the
//    constant pool table.
//
unsigned long
CJavaMethodConstant::GetWidth() const
{
  return 1;
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable explanation of this constant's
//    type and value to the provided stream.  For debugging, mainly.
//
void
CJavaMethodConstant::Disassemble(ostream& toStream) const
{
  toStream << "Method: class " << fClassIndex << ", name and type "
	   << fNameTypeIndex;
}

//
//  Method name : Compile
//  Description : Writes out this constant to the provided stream in the
//    format specified by the Java VM spec.
//
void
CJavaMethodConstant::Compile(ostream& toStream) const
{
  toStream << (unsigned char)GetConstantType();
  CJavaClassFile::WriteJavaU2(toStream, fClassIndex);
  CJavaClassFile::WriteJavaU2(toStream, fNameTypeIndex);
}

//=========================== CJavaStringConstant =============================
//
//  Method name : CJavaStringConstant
//  Description : Constructor.
//
CJavaStringConstant::CJavaStringConstant(JavaConstantIndex stringIndex)
  : fStringIndex(stringIndex)
{
}

//
//  Method name : ~CJavaStringConstant
//  Description : Destructor
//
CJavaStringConstant::~CJavaStringConstant()
{
}

//
//  Method name : operator==
//  Description : Compares two constants for equality.
//
bool
CJavaStringConstant::operator==(const CJavaConstant& other) const
{
  CJavaStringConstant* otherClass = DYNAMIC_CAST(CJavaStringConstant, &other);
  return otherClass != 0 && fStringIndex == otherClass->fStringIndex;
}

//
//  Method name : GetConstantType
//  Description : Returns the enumerated constant indicating the type marker
//    used for this constant in the Java class file.
//
CJavaConstant::ConstantType
CJavaStringConstant::GetConstantType() const
{
  return kString;
}

//
//  Method name : GetWidth
//  Description : Returns the number of slots this constant takes up in the
//    constant pool table.
//
unsigned long
CJavaStringConstant::GetWidth() const
{
  return 1;
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable explanation of this constant's
//    type and value to the provided stream.  For debugging, mainly.
//
void
CJavaStringConstant::Disassemble(ostream& toStream) const
{
  toStream << "String: asciz index " << fStringIndex;
}

//
//  Method name : Compile
//  Description : Writes out this constant to the provided stream in the
//    format specified by the Java VM spec.
//
void
CJavaStringConstant::Compile(ostream& toStream) const
{
  toStream << (unsigned char)GetConstantType();
  CJavaClassFile::WriteJavaU2(toStream, fStringIndex);
}

//=========================== CJavaIntegerConstant ============================
//
//  Method name : CJavaIntegerConstant
//  Description : Constructor.
//
CJavaIntegerConstant::CJavaIntegerConstant(long number)
  : fInteger(number)
{
}

//
//  Method name : ~CJavaIntegerConstant
//  Description : Destructor
//
CJavaIntegerConstant::~CJavaIntegerConstant()
{
}

//
//  Method name : operator==
//  Description : Compares two constants for equality.
//
bool
CJavaIntegerConstant::operator==(const CJavaConstant& other) const
{
  CJavaIntegerConstant* otherClass =
    DYNAMIC_CAST(CJavaIntegerConstant, &other);
  return otherClass != 0 && fInteger == otherClass->fInteger;
}

//
//  Method name : GetConstantType
//  Description : Returns the enumerated constant indicating the type marker
//    used for this constant in the Java class file.
//
CJavaConstant::ConstantType
CJavaIntegerConstant::GetConstantType() const
{
  return kInteger;
}

//
//  Method name : GetWidth
//  Description : Returns the number of slots this constant takes up in the
//    constant pool table.
//
unsigned long
CJavaIntegerConstant::GetWidth() const
{
  return 1;
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable explanation of this constant's
//    type and value to the provided stream.  For debugging, mainly.
//
void
CJavaIntegerConstant::Disassemble(ostream& toStream) const
{
  toStream << "Integer constant value " << fInteger;
}

//
//  Method name : Compile
//  Description : Writes out this constant to the provided stream in the
//    format specified by the Java VM spec.
//
void
CJavaIntegerConstant::Compile(ostream& toStream) const
{
  toStream << (unsigned char)GetConstantType();
  CJavaClassFile::WriteJavaU4(toStream, fInteger);
}

//============================ CJavaFloatConstant =============================
//
//  Method name : CJavaFloatConstant
//  Description : Constructor.
//
CJavaFloatConstant::CJavaFloatConstant(float number)
  : fFloat(number)
{
}

//
//  Method name : ~CJavaFloatConstant
//  Description : Destructor
//
CJavaFloatConstant::~CJavaFloatConstant()
{
}

//
//  Method name : operator==
//  Description : Compares two constants for equality.
//
bool
CJavaFloatConstant::operator==(const CJavaConstant& other) const
{
  CJavaFloatConstant* otherClass = DYNAMIC_CAST(CJavaFloatConstant, &other);
  return otherClass != 0 && fFloat == otherClass->fFloat;
}

//
//  Method name : GetConstantType
//  Description : Returns the enumerated constant indicating the type marker
//    used for this constant in the Java class file.
//
CJavaConstant::ConstantType
CJavaFloatConstant::GetConstantType() const
{
  return kFloat;
}

//
//  Method name : GetWidth
//  Description : Returns the number of slots this constant takes up in the
//    constant pool table.
//
unsigned long
CJavaFloatConstant::GetWidth() const
{
  return 1;
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable explanation of this constant's
//    type and value to the provided stream.  For debugging, mainly.
//
void
CJavaFloatConstant::Disassemble(ostream& toStream) const
{
  toStream << "Floating-point constant value " << fFloat;
}

//
//  Method name : Compile
//  Description : Writes out this constant to the provided stream in the
//    format specified by the Java VM spec.
//
void
CJavaFloatConstant::Compile(ostream& toStream) const
{
  toStream << (unsigned char)GetConstantType();
  unsigned long word;
  ::memcpy(&word, ((char*)&fFloat), 4);
  CJavaClassFile::WriteJavaU4(toStream, word);
}

//============================ CJavaLongConstant =============================
//
//  Method name : CJavaLongConstant
//  Description : Constructor
//
CJavaLongConstant::CJavaLongConstant(unsigned long long value)
  : fLong(value)
{
}

//
//  Method name : ~CJavaLongConstant
//  Description : Destructor
//
CJavaLongConstant::~CJavaLongConstant()
{
}

//
//  Method name : operator==
//  Description : Compares two constants for equality.
//
bool
CJavaLongConstant::operator==(const CJavaConstant& other) const
{
  CJavaLongConstant* otherClass = DYNAMIC_CAST(CJavaLongConstant, &other);
  return otherClass != 0 && fLong == otherClass->fLong;
}

//
//  Method name : GetConstantType
//  Description : Returns the enumerated constant indicating the type marker
//    used for this constant in the Java class file.
//
CJavaConstant::ConstantType
CJavaLongConstant::GetConstantType() const
{
  return kLong;
}

//
//  Method name : GetWidth
//  Description : Returns the number of slots this constant takes up in the
//    constant pool table.
//
unsigned long
CJavaLongConstant::GetWidth() const
{
  return 2;
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable explanation of this constant's
//    type and value to the provided stream.  For debugging, mainly.
//
void
CJavaLongConstant::Disassemble(ostream& toStream) const
{
  toStream << "Long value " << (long long)fLong;
}

//
//  Method name : Compile
//  Description : Writes out this constant to the provided stream in the
//    format specified by the Java VM spec.
//
void
CJavaLongConstant::Compile(ostream& toStream) const
{
  toStream << (unsigned char)GetConstantType();
  unsigned long word;
  #ifdef WORDS_BIGENDIAN
    ::memcpy(&word, ((char*)&fLong), 4);
  #else
    ::memcpy(&word, ((char*)&fLong) + 4, 4);
  #endif
  CJavaClassFile::WriteJavaU4(toStream, word);
  #ifdef WORDS_BIGENDIAN
    ::memcpy(&word, ((char*)&fLong) + 4, 4);
  #else
    ::memcpy(&word, ((char*)&fLong), 4);
  #endif
  CJavaClassFile::WriteJavaU4(toStream, word);
}

//=========================== CJavaDoubleConstant =============================
//
//  Method name : CJavaDoubleConstant
//  Description : Constructor.
//
CJavaDoubleConstant::CJavaDoubleConstant(double number)
  : fDouble(number)
{
}

//
//  Method name : ~CJavaDoubleConstant
//  Description : Destructor
//
CJavaDoubleConstant::~CJavaDoubleConstant()
{
}

//
//  Method name : operator==
//  Description : Compares two constants for equality.
//
bool
CJavaDoubleConstant::operator==(const CJavaConstant& other) const
{
  CJavaDoubleConstant* otherClass = DYNAMIC_CAST(CJavaDoubleConstant, &other);
  return otherClass != 0 && fDouble == otherClass->fDouble;
}

//
//  Method name : GetConstantType
//  Description : Returns the enumerated constant indicating the type marker
//    used for this constant in the Java class file.
//
CJavaConstant::ConstantType
CJavaDoubleConstant::GetConstantType() const
{
  return kDouble;
}

//
//  Method name : GetWidth
//  Description : Returns the number of slots this constant takes up in the
//    constant pool table.
//
unsigned long
CJavaDoubleConstant::GetWidth() const
{
  return 2;
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable explanation of this constant's
//    type and value to the provided stream.  For debugging, mainly.
//
void
CJavaDoubleConstant::Disassemble(ostream& toStream) const
{
  toStream << "Double-precision constant value " << fDouble;
}

//
//  Method name : Compile
//  Description : Writes out this constant to the provided stream in the
//    format specified by the Java VM spec.
//
void
CJavaDoubleConstant::Compile(ostream& toStream) const
{
  toStream << (unsigned char)GetConstantType();
  unsigned long word;
  #ifdef WORDS_BIGENDIAN
    ::memcpy(&word, ((char*)&fDouble), 4);
  #else
    ::memcpy(&word, ((char*)&fDouble) + 4, 4);
  #endif
  CJavaClassFile::WriteJavaU4(toStream, word);
  #ifdef WORDS_BIGENDIAN
    ::memcpy(&word, ((char*)&fDouble) + 4, 4);
  #else
    ::memcpy(&word, ((char*)&fDouble), 4);
  #endif
  CJavaClassFile::WriteJavaU4(toStream, word);
}

//========================== CJavaInterfaceConstant ===========================
//
//  Method name : CJavaInterfaceConstant
//  Description : Constructor.
//
CJavaInterfaceConstant::CJavaInterfaceConstant(JavaConstantIndex classIndex,
				       JavaConstantIndex nameTypeIndex)
  : fClassIndex(classIndex), fNameTypeIndex(nameTypeIndex)
{
}

//
//  Method name : ~CJavaInterfaceConstant
//  Description : Destructor
//
CJavaInterfaceConstant::~CJavaInterfaceConstant()
{
}

//
//  Method name : operator==
//  Description : Compares two constants for equality.
//
bool
CJavaInterfaceConstant::operator==(const CJavaConstant& other) const
{
  CJavaInterfaceConstant* otherClass =
    DYNAMIC_CAST(CJavaInterfaceConstant, &other);
  return otherClass != 0 &&
    fClassIndex == otherClass->fClassIndex &&
    fNameTypeIndex == otherClass->fNameTypeIndex;
}

//
//  Method name : GetConstantType
//  Description : Returns the enumerated constant indicating the type marker
//    used for this constant in the Java class file.
//
CJavaConstant::ConstantType
CJavaInterfaceConstant::GetConstantType() const
{
  return kInterface;
}

//
//  Method name : GetWidth
//  Description : Returns the number of slots this constant takes up in the
//    constant pool table.
//
unsigned long
CJavaInterfaceConstant::GetWidth() const
{
  return 1;
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable explanation of this constant's
//    type and value to the provided stream.  For debugging, mainly.
//
void
CJavaInterfaceConstant::Disassemble(ostream& toStream) const
{
  toStream << "Interface:  class " << fClassIndex << ", name and type "
	   << fNameTypeIndex;
}

//
//  Method name : Compile
//  Description : Writes out this constant to the provided stream in the
//    format specified by the Java VM spec.
//
void
CJavaInterfaceConstant::Compile(ostream& toStream) const
{
  toStream << (unsigned char)GetConstantType();
  CJavaClassFile::WriteJavaU2(toStream, fClassIndex);
  CJavaClassFile::WriteJavaU2(toStream, fNameTypeIndex);
}

//============================ CJavaNameTypeConstant ==========================
//
//  Method name : CJavaNameTypeConstant
//  Description : Constructor.
//
CJavaNameTypeConstant::CJavaNameTypeConstant(JavaConstantIndex nameIndex,
					     JavaConstantIndex signatureIndex)
  : fNameIndex(nameIndex), fSignatureIndex(signatureIndex)
{
}

//
//  Method name : ~CJavaNameTypeConstant
//  Description : Destructor
//
CJavaNameTypeConstant::~CJavaNameTypeConstant()
{
}

//
//  Method name : operator==
//  Description : Compares two constants for equality.
//
bool
CJavaNameTypeConstant::operator==(const CJavaConstant& other) const
{
  CJavaNameTypeConstant* otherClass =
    DYNAMIC_CAST(CJavaNameTypeConstant, &other);
  return otherClass != 0 &&
    fNameIndex == otherClass->fNameIndex &&
    fSignatureIndex == otherClass->fSignatureIndex;
}

//
//  Method name : GetConstantType
//  Description : Returns the enumerated constant indicating the type marker
//    used for this constant in the Java class file.
//
CJavaConstant::ConstantType
CJavaNameTypeConstant::GetConstantType() const
{
  return kNameType;
}

//
//  Method name : GetWidth
//  Description : Returns the number of slots this constant takes up in the
//    constant pool table.
//
unsigned long
CJavaNameTypeConstant::GetWidth() const
{
  return 1;
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable explanation of this constant's
//    type and value to the provided stream.  For debugging, mainly.
//
void
CJavaNameTypeConstant::Disassemble(ostream& toStream) const
{
  toStream << "Name and Type constant: name " << fNameIndex
	   << ", type signature " << fSignatureIndex;
}

//
//  Method name : Compile
//  Description : Writes out this constant to the provided stream in the
//    format specified by the Java VM spec.
//
void
CJavaNameTypeConstant::Compile(ostream& toStream) const
{
  toStream << (unsigned char)GetConstantType();
  CJavaClassFile::WriteJavaU2(toStream, fNameIndex);
  CJavaClassFile::WriteJavaU2(toStream, fSignatureIndex);
}

//============================ CJavaAscizConstant =============================
//
//  Method name : CJavaAscizConstant
//  Description : Constructor.
//
CJavaAscizConstant::CJavaAscizConstant(const char* bytes, long length)
{
  fJavaString = ::UTFToUnicode(string(bytes, length));
}

//
//  Method name : CJavaAscizConstant
//  Description : Constructor.
//
CJavaAscizConstant::CJavaAscizConstant(const unicode_string& unicodeConstant)
  : fJavaString(unicodeConstant)
{
}

//
//  Method name : ~CJavaAscizConstant
//  Description : Destructor
//
CJavaAscizConstant::~CJavaAscizConstant()
{
}

//
//  Method name : operator==
//  Description : Compares two constants for equality.
//
bool
CJavaAscizConstant::operator==(const CJavaConstant& other) const
{
  CJavaAscizConstant* otherClass = DYNAMIC_CAST(CJavaAscizConstant, &other);
  return otherClass != 0 && fJavaString == otherClass->fJavaString;
}

//
//  Method name : GetConstantType
//  Description : Returns the enumerated constant indicating the type marker
//    used for this constant in the Java class file.
//
CJavaConstant::ConstantType
CJavaAscizConstant::GetConstantType() const
{
  return kAsciz;
}

//
//  Method name : GetWidth
//  Description : Returns the number of slots this constant takes up in the
//    constant pool table.
//
unsigned long
CJavaAscizConstant::GetWidth() const
{
  return 1;
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable explanation of this constant's
//    type and value to the provided stream.  For debugging, mainly.
//
void
CJavaAscizConstant::Disassemble(ostream& toStream) const
{
  toStream << "\"" << ::UnicodeToUTF(fJavaString) << "\"";
}

//
//  Method name : Compile
//  Description : Writes out this constant to the provided stream in the
//    format specified by the Java VM spec.
//
void
CJavaAscizConstant::Compile(ostream& toStream) const
{
  toStream << (unsigned char)GetConstantType();
  string utfString = ::UnicodeToUTF(fJavaString);
  CJavaClassFile::WriteJavaU2(toStream, utfString.size());
  toStream << utfString;
}
