// $Id: JavaTypeSignature.h,v 1.4 1996/03/09 20:08:37 geppetto Exp $
// Copyright (c) 1995  David Engberg  All rights reserved
#ifndef _JavaTypeSignature_h
#define _JavaTypeSignature_h

#include "dynamic_cast.h"
#include "unicode_string.h"
class CJavaTypeExtension;
class ostream;

//
//  Class name : CJavaTypeSignature
//  Description : Java represents the type of a variable or parameter using
//    a sequence of characters that can be unpacked to give the type.  For
//    example, the integer type would be represented by "I" and an array
//    of 72 integers would be represented by "[72I"
//    This class attempts to encapsulate that in a little bit more friendly
//    form.  The one quirk is that the type 'Void' is only valid when used
//    as the return value for a function.  In any other context, a 'Void'
//    type signature can be taken as an error.
//
class CJavaTypeSignature {
public:
  typedef enum {
    Void,
    Byte,
    Character,
    Double,
    Float,
    Integer,
    LongInteger,
    Class,
    Short,
    Boolean
  } JavaType;

  CJavaTypeSignature();
  CJavaTypeSignature(JavaType baseType, unsigned long arrayBounds = 0);
  CJavaTypeSignature(const unicode_string& className,
		     unsigned long arrayBounds = 0);
  CJavaTypeSignature(const CJavaTypeSignature& source);
  ~CJavaTypeSignature();
  CJavaTypeSignature& operator=(const CJavaTypeSignature& source);
  bool operator==(const CJavaTypeSignature& other) const;
  
  bool ParseSignatureString(unicode_string::const_iterator& stringPointer,
			    unicode_string::const_iterator stringEnd);
  bool ParseSignatureString(const string& utfString);
  void Disassemble(ostream& toStream) const;
  unicode_string Disassemble() const;
  string Compile() const;

  unsigned long GetArrayBounds() const { return fArrayBounds; }
  void SetArrayBounds(unsigned long bounds) { fArrayBounds = bounds; }
  JavaType GetBaseType() const { return fType; }

  unsigned short GetWidth() const;
  unsigned long Hash() const;
  int Compare(const CJavaTypeSignature& other) const;

  bool IsNumeric() const;
  bool IsOrdinal() const;
  bool IsFloatingPoint() const;
  bool IsReference() const;
  bool GetBaseClassName(unicode_string& fillName) const;
  void SetBaseClassName(const unicode_string& name);

  static const CJavaTypeSignature kVoid;
  static const CJavaTypeSignature kInteger;
  static const CJavaTypeSignature kLong;
  static const CJavaTypeSignature kString;
  static const CJavaTypeSignature kNullType;
  static const CJavaTypeSignature kBoolean;
  static const CJavaTypeSignature kByte;
  static const CJavaTypeSignature kDouble;
  static const CJavaTypeSignature kFloat;
  static const CJavaTypeSignature kShort;
  static const CJavaTypeSignature kCharacter;
private:
  JavaType fType;
  unsigned long fArrayBounds;
  unicode_string fClassName;
};

#endif
