// $Id: JavaTypeSignature.C,v 1.6 1997/06/13 18:18:49 geppetto Exp $
// Copyright (c) 1995  David Engberg  All rights reserved
#include "JavaTypeSignature.h"
#include <cstdlib>
#include <cassert>
#include <iostream.h>

const CJavaTypeSignature CJavaTypeSignature::kVoid(CJavaTypeSignature::Void);
const CJavaTypeSignature CJavaTypeSignature::kByte(CJavaTypeSignature::Byte);
const CJavaTypeSignature
                   CJavaTypeSignature::kInteger(CJavaTypeSignature::Integer);
const CJavaTypeSignature
                  CJavaTypeSignature::kLong(CJavaTypeSignature::LongInteger);
const CJavaTypeSignature
           CJavaTypeSignature::kString(::StringToUnicode("java/lang/String"));
static const unicode_string kEmptyString;
const CJavaTypeSignature CJavaTypeSignature::kNullType(kEmptyString);
const CJavaTypeSignature
           CJavaTypeSignature::kBoolean(CJavaTypeSignature::Boolean);
const CJavaTypeSignature
           CJavaTypeSignature::kDouble(CJavaTypeSignature::Double);
const CJavaTypeSignature
           CJavaTypeSignature::kFloat(CJavaTypeSignature::Float);
const CJavaTypeSignature
           CJavaTypeSignature::kShort(CJavaTypeSignature::Short);
const CJavaTypeSignature
           CJavaTypeSignature::kCharacter(CJavaTypeSignature::Character);

//
//  Method name : CJavaTypeSignature
//  Description : Default constructor.
//
CJavaTypeSignature::CJavaTypeSignature()
  : fType(Void), fArrayBounds(0), fClassName()
{
}

//
//  Method name : CJavaTypeSignature
//  Description : Copy constructor.
//
CJavaTypeSignature::CJavaTypeSignature(const CJavaTypeSignature& source)
  : fType(source.fType),
    fArrayBounds(source.fArrayBounds),
    fClassName(source.fClassName)
{
}

//
//  Method name : CJavaTypeSignature
//  Description : Constructs a type signature from a provided element from
//    the base type enumeration.  This should never be constructed using the
//    'Class' value.  The second argument is used to specify how many array
//    indices are included in this type.  So int[][] would be represented
//    by atom 'Integer' and arrayBounds = 2
//
CJavaTypeSignature::CJavaTypeSignature(JavaType baseType,
				       unsigned long arrayBounds)
  : fType(baseType), fArrayBounds(arrayBounds), fClassName()
{
}

//
//  Method name : CJavaTypeSignature
//  Description : Constructs a type signature corresponding to a class type
//    or an array of classes.  The user provides the base class type name as
//    the first argument, and then the number of array indices as the second
//    argument.  If this type isn't an array, this should be left at 0.
//
CJavaTypeSignature::CJavaTypeSignature(const unicode_string& className,
				       unsigned long arrayBounds)
  : fType(Class), fArrayBounds(arrayBounds), fClassName(className)
{
}

//
//  Method name : ~CJavaTypeSignature
//  Description : Destructor
//
CJavaTypeSignature::~CJavaTypeSignature()
{
}

//
//  Method name : operator=
//  Description : Assignment operator.
//
CJavaTypeSignature&
CJavaTypeSignature::operator=(const CJavaTypeSignature& source)
{
  if (&source != this) {
    fType = source.fType;
    fArrayBounds = source.fArrayBounds;
    fClassName = source.fClassName;
  }
  return *this;
}


//
//  Method name : Compare
//  Description : Compares two java type signatures.  If they are equal, 0
//    is returned.  If this is greater than the other, a negative is returned,
//    otherwise a positive is returned.
//
int
CJavaTypeSignature::Compare(const CJavaTypeSignature& other) const
{
  int result = other.fType - fType;
  if (result == 0) {
    result = other.fArrayBounds - fArrayBounds;
    if (result == 0 && fType == Class) {
      result = fClassName.compare(other.fClassName);
    }
  }
  return result;
}


//
//  Method name : operator==
//  Description : Compares two signatures for equality
//
bool
CJavaTypeSignature::operator==(const CJavaTypeSignature& other) const
{
  return Compare(other) == 0;
}

//
//  Method name : ParseSignatureString
//  Description : Attempts to parse the provided string as if it started with
//    the Java VM-standard signature for a type.  If it does, then this type
//    is configured to match that spec and true is returned, otherwise false
//    is returned and this class is left with the value 'void'
//
bool
CJavaTypeSignature::ParseSignatureString(
			       unicode_string::const_iterator& stringPointer,
			       unicode_string::const_iterator stringEnd)
{
  fArrayBounds = 0;
  fClassName.remove();
  fType = Void;
  bool result = stringPointer < stringEnd;
  if (result) {
    switch (*stringPointer++) {
    case 'V':
      break;
    case 'B':
      fType = Byte;
      break;
    case 'C':
      fType = Character;
      break;
    case 'D':
      fType = Double;
      break;
    case 'F':
      fType = Float;
      break;
    case 'I':
      fType = Integer;
      break;
    case 'J':
      fType = LongInteger;
      break;
    case 'L':
      {
	unicode_string::const_iterator marker = stringPointer;
	while (marker < stringEnd && *marker != ';') {
	  marker++;
	}
	if (marker == stringEnd) {
	  result = false;
	} else {
	  fClassName.replace(fClassName.begin(), fClassName.end(),
			     stringPointer, marker - stringPointer);
	  fType = Class;
	  stringPointer = marker + 1;
	}
      }
      break;
    case 'S':
      fType = Short;
      break;
    case 'Z':
      fType = Boolean;
      break;
    case '[':
      result = ParseSignatureString(stringPointer, stringEnd);
      fArrayBounds++;
      break;
    default:
      result = false;      
    }
  }
  return result;
}

//
//  Method name : ParseSignatureString
//  Description : Attempts to parse the provided string as if it started with
//    the Java VM-standard signature for a type.  If it does, then this type
//    is configured to match that spec and true is returned, otherwise false
//    is returned and this class is left with the value 'void'
//
bool
CJavaTypeSignature::ParseSignatureString(const string& utfString)
{
  fArrayBounds = 0;
  fClassName.remove();
  fType = Void;
  bool result = utfString.length() > 0;
  if (result) {
    switch (utfString[0]) {
    case 'V':
      break;
    case 'B':
      fType = Byte;
      break;
    case 'C':
      fType = Character;
      break;
    case 'D':
      fType = Double;
      break;
    case 'F':
      fType = Float;
      break;
    case 'I':
      fType = Integer;
      break;
    case 'J':
      fType = LongInteger;
      break;
    case 'L':
      {
	result = utfString[utfString.length() - 1] == ';';
	if (result) {
	  fType = Class;
	  fClassName =
	    ::UTFToUnicode(utfString.substr(1, utfString.length() - 2));
	}
      }
      break;
    case 'S':
      fType = Short;
      break;
    case 'Z':
      fType = Boolean;
      break;
    case '[':
      {
	string remainder(utfString, 1);
	result = ParseSignatureString(remainder);
	fArrayBounds++;
	break;
      }
    default:
      result = false;      
    }
  }
  return result;
}

//
//  Method name : Disassemble
//  Description : Returns a human-readable description of this signature in the
//    form of a string.
//
unicode_string
CJavaTypeSignature::Disassemble() const
{
  unicode_string result;
  switch (fType) {
  case Void:
    result = ::StringToUnicode("void");
    break;
  case Byte:
    result = ::StringToUnicode("byte");
    break;
  case Character:
    result = ::StringToUnicode("char");
    break;
  case Double:
    result = ::StringToUnicode("double");
    break;
  case Float:
    result = ::StringToUnicode("float");
    break;
  case Integer:
    result = ::StringToUnicode("int");
    break;
  case LongInteger:
    result = ::StringToUnicode("long");
    break;
  case Short:
    result = ::StringToUnicode("short");
    break;
  case Boolean:
    result = ::StringToUnicode("boolean");
    break;
  case Class:
    if (fClassName.size() > 0) {
      result = fClassName;
    } else {
      result = ::StringToUnicode("<null-reference-type>");
    }
    break;
  default:
    assert(0);
  }
  for (int arrayCount = fArrayBounds; arrayCount > 0; arrayCount--) {
    result += ::StringToUnicode("[]");
  }
  return result;
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable description of this class to
//    the provided stream.  For debugging purposes.
//
void
CJavaTypeSignature::Disassemble(ostream& toStream) const
{
  toStream << ::UnicodeToString(Disassemble());
}


//
//  Method name : Hash
//  Description : This function returns an unsigned long value that will
//    compare equal for any two identical types.
//
unsigned long
CJavaTypeSignature::Hash() const
{
  unsigned long result = (unsigned long)fType;
  if (fType == Class) {
    result = ::Hash(fClassName);
  }
  for (int i=0; i < fArrayBounds; i++) {
    result = result * 13;
  }
  return result;
}

//
//  Method name : GetWidth
//  Description : This annoying bit of cruft is here because some values have
//    a width of two slots in some locations.  Ick, whine, etc.
//
unsigned short
CJavaTypeSignature::GetWidth() const
{
  assert(fType != Void);
  if (fArrayBounds > 0) {
    return 1;
  } else {
    return ((fType == LongInteger || fType == Double) ? 2 : 1);
  }
}

//
//  Method name : SetBaseClassName
//  Description : Sets the base class name of this type to the provided name.
//
void
CJavaTypeSignature::SetBaseClassName(const unicode_string& name)
{
  assert(fType == Class);
  fClassName = name;
}

//
//  Method name : IsNumeric
//  Description : Returns true if this type is a numeric type.  This includes
//    bytes, doubles, floats, integers, shorts and longs
//
bool
CJavaTypeSignature::IsNumeric() const
{
  switch (fType) {
  case Byte: case Double: case Float:
  case Integer: case LongInteger: case Short: case Character:
    return fArrayBounds == 0;
  }
  return false;
}

//
//  Method name : IsOrdinal
//  Description : Returns true if this type is an ordinal type.  This includes
//     bytes, shorts, integers, and longs.
//
bool
CJavaTypeSignature::IsOrdinal() const
{
  switch (fType) {
  case Byte: case Integer: case LongInteger: case Short: case Character:
    return fArrayBounds == 0;
  }
  return false;
}

//
//  Method name : IsNumeric
//  Description : Returns true if this type is a non-ordinal numeric value.
//    This includes doubles and floats.
//
bool
CJavaTypeSignature::IsFloatingPoint() const
{
  switch (fType) {
  case Double: case Float:
    return fArrayBounds == 0;
  }
  return false;
}

//
//  Method name : IsReference
//  Description : Returns true if this type indicates a reference to a class
//    or array expression.
//
bool
CJavaTypeSignature::IsReference() const
{
  return fType == Class || fArrayBounds != 0;
}

//
//  Method name : GetBaseClassName
//  Description : If the base type of this type signature is Class, this will
//    return true and fill the provided reference with the name of the class.
//    Otherwise, returns false.  Note that an array of a class type will have
//    a class base type, so Foo[] will return true and put "Foo" into fillName.
//
bool
CJavaTypeSignature::GetBaseClassName(unicode_string& fillName) const
{
  if (fType == Class) {
    fillName = fClassName;
    return true;
  }
  return false;
}

//
//  Method name : Compile
//  Description : Writes out this type in the Java VM Class format.
//
string
CJavaTypeSignature::Compile() const
{
  string result;
  for (int i = 0; i < GetArrayBounds(); ++i) {
    result += '[';
  }
  switch (GetBaseType()) {
  case Void:
    result += 'V';
    break;
  case Byte:
    result += 'B';
    break;
  case Character:
    result += 'C';
    break;
  case Double:
    result += 'D';
    break;
  case Float:
    result += 'F';
    break;
  case Integer:
    result += 'I';
    break;
  case LongInteger:
    result += 'J';
    break;
  case Class:
    result += 'L';
    result += ::UnicodeToUTF(fClassName);
    result += ';';
    break;
  case Short:
    result += 'S';
    break;
  case Boolean:
    result += 'Z';
    break;
  default:
    assert(0);
    break;
  }
  return result;
}
