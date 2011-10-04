// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: JavaConstant.h,v 1.3 1996/01/22 05:37:05 geppetto Exp $
#ifndef _JavaConstant_h
#define _JavaConstant_h

#include "unicode_string.h"
#include "dynamic_cast.h"
class ostream;

typedef unsigned short JavaConstantIndex;

//
//  Class name : CJavaConstant
//  Description : This virtual base class is designed to represent a single
//    entry in a Java class's constant pool.  It doesn't actually do anything
//    in its own right other than set up the protocol for all of the other
//    constants.  When some function hands off a pointer to a CJavaConstant,
//    the user needs to use the DYNAMIC_CAST macro to figure out what the
//    actual constant is.  Messy?  yeah, so's the Java VM layout.
//
class CJavaConstant {
  DynamicCastDeclarations;
public:
  virtual ~CJavaConstant();
  
  typedef enum { kClass = 7,
		 kField = 9,
		 kMethod = 10,
		 kString = 8,
		 kInteger = 3,
		 kFloat = 4,
		 kLong = 5,
		 kDouble = 6,
		 kInterface = 11,
		 kNameType = 12,
		 kUnicode = 2,
		 kAsciz = 1 } ConstantType;
  virtual ConstantType GetConstantType() const = 0;
  virtual unsigned long GetWidth() const = 0;
  virtual void Disassemble(ostream& toStream) const = 0;
  virtual void Compile(ostream& toStream) const = 0;
  virtual bool operator==(const CJavaConstant& other) const = 0;
};


//
//  Class name : CJavaClassConstant
//  Description : This represents a CONSTANT_Class value from the Java
//    class file format.
//
class CJavaClassConstant : public CJavaConstant {
  DynamicCastDeclarations;
public:
  CJavaClassConstant(JavaConstantIndex nameIndex);
  virtual ~CJavaClassConstant();
  virtual ConstantType GetConstantType() const;
  virtual unsigned long GetWidth() const;
  virtual bool operator==(const CJavaConstant& other) const;
  JavaConstantIndex GetNameIndex() const { return fNameIndex; }
  virtual void Disassemble(ostream& toStream) const;
  virtual void Compile(ostream& toStream) const;
private:
  JavaConstantIndex fNameIndex;
};


//
//  Class name : CJavaFieldConstant
//  Description : This represents a CONSTANT_Fieldref value from the Java
//    class file format.
//
class CJavaFieldConstant : public CJavaConstant {
  DynamicCastDeclarations;
public:
  CJavaFieldConstant(JavaConstantIndex classIndex,
		     JavaConstantIndex nameTypeIndex);
  virtual ~CJavaFieldConstant();
  virtual bool operator==(const CJavaConstant& other) const;
  virtual ConstantType GetConstantType() const;
  virtual unsigned long GetWidth() const;
  JavaConstantIndex GetClassIndex() const { return fClassIndex; }
  JavaConstantIndex GetNameTypeIndex() const { return fNameTypeIndex; }
  virtual void Disassemble(ostream& toStream) const;
  virtual void Compile(ostream& toStream) const;
private:
  JavaConstantIndex fClassIndex;
  JavaConstantIndex fNameTypeIndex;
};


//
//  Class name : CJavaMethodConstant
//  Description : This represents a CONSTANT_MethodConstant value from the Java
//    class file format.
//
class CJavaMethodConstant : public CJavaConstant {
  DynamicCastDeclarations;
public:
  CJavaMethodConstant(JavaConstantIndex classIndex,
		      JavaConstantIndex nameTypeIndex);
  virtual ~CJavaMethodConstant();
  virtual ConstantType GetConstantType() const;
  virtual unsigned long GetWidth() const;
  virtual bool operator==(const CJavaConstant& other) const;
  JavaConstantIndex GetClassIndex() const { return fClassIndex; }
  JavaConstantIndex GetNameTypeIndex() const { return fNameTypeIndex; }
  virtual void Disassemble(ostream& toStream) const;
  virtual void Compile(ostream& toStream) const;
private:
  JavaConstantIndex fClassIndex;
  JavaConstantIndex fNameTypeIndex;
};


//
//  Class name : CJavaStringConstant
//  Description : This represents a CONSTANT_String value from the Java
//    class file format.
//
class CJavaStringConstant : public CJavaConstant {
  DynamicCastDeclarations;
public:
  CJavaStringConstant(JavaConstantIndex stringIndex);
  virtual ~CJavaStringConstant();
  virtual ConstantType GetConstantType() const;
  virtual unsigned long GetWidth() const;
  virtual bool operator==(const CJavaConstant& other) const;
  JavaConstantIndex GetStringIndex() const { return fStringIndex; }
  virtual void Disassemble(ostream& toStream) const;
  virtual void Compile(ostream& toStream) const;
private:
  JavaConstantIndex fStringIndex;
};


//
//  Class name : CJavaIntegerConstant
//  Description : This represents a CONSTANT_Integer value from the Java
//    class file format.
//
class CJavaIntegerConstant : public CJavaConstant {
  DynamicCastDeclarations;
public:
  CJavaIntegerConstant(long number);
  virtual ~CJavaIntegerConstant();
  virtual bool operator==(const CJavaConstant& other) const;
  virtual ConstantType GetConstantType() const;
  virtual unsigned long GetWidth() const;
  long GetInteger() const { return fInteger; }
  virtual void Disassemble(ostream& toStream) const;
  virtual void Compile(ostream& toStream) const;
private:
  long fInteger;
};


//
//  Class name : CJavaFloatConstant
//  Description : This represents a CONSTANT_Float value from the Java
//    class file format.
//
class CJavaFloatConstant : public CJavaConstant {
  DynamicCastDeclarations;
public:
  CJavaFloatConstant(float number);
  virtual ~CJavaFloatConstant();
  virtual bool operator==(const CJavaConstant& other) const;
  virtual ConstantType GetConstantType() const;
  virtual unsigned long GetWidth() const;
  float GetFloat() const { return fFloat; }
  virtual void Disassemble(ostream& toStream) const;
  virtual void Compile(ostream& toStream) const;
private:
  float fFloat;
};


//
//  Class name : CJavaLongConstant
//  Description : This represents a CONSTANT_Long value from the Java
//    class file format.
//
class CJavaLongConstant : public CJavaConstant {
  DynamicCastDeclarations;
public:
  CJavaLongConstant(unsigned long long value);
  virtual ~CJavaLongConstant();
  virtual bool operator==(const CJavaConstant& other) const;
  virtual ConstantType GetConstantType() const;
  virtual unsigned long GetWidth() const;
  virtual void Disassemble(ostream& toStream) const;
  virtual void Compile(ostream& toStream) const;
  long long GetLong() const { return fLong; }
private:
  unsigned long long fLong;
};


//
//  Class name : CJavaDoubleConstant
//  Description : This represents a CONSTANT_Double value from the Java
//    class file format.
//
class CJavaDoubleConstant : public CJavaConstant {
  DynamicCastDeclarations;
public:
  CJavaDoubleConstant(double number);
  virtual ~CJavaDoubleConstant();
  virtual bool operator==(const CJavaConstant& other) const;
  virtual ConstantType GetConstantType() const;
  virtual unsigned long GetWidth() const;
  double GetDouble() const { return fDouble; }
  virtual void Disassemble(ostream& toStream) const;
  virtual void Compile(ostream& toStream) const;
private:
  double fDouble;
};


//
//  Class name : CJavaInterfaceConstant
//  Description : This represents a CONSTANT_InterfaceMethodref value from the
//    Java class file format.
//
class CJavaInterfaceConstant : public CJavaConstant {
  DynamicCastDeclarations;
public:
  CJavaInterfaceConstant(JavaConstantIndex classIndex,
			 JavaConstantIndex nameTypeIndex);
  virtual ~CJavaInterfaceConstant();
  virtual bool operator==(const CJavaConstant& other) const;
  virtual ConstantType GetConstantType() const;
  virtual unsigned long GetWidth() const;
  JavaConstantIndex GetClassIndex() const { return fClassIndex; }
  JavaConstantIndex GetNameTypeIndex() const { return fNameTypeIndex; }
  virtual void Disassemble(ostream& toStream) const;
  virtual void Compile(ostream& toStream) const;
private:
  JavaConstantIndex fClassIndex;
  JavaConstantIndex fNameTypeIndex;
};


//
//  Class name : CJavaNameTypeConstant
//  Description : This represents a CONSTANT_NameandType value from the Java
//    class file format.
//
class CJavaNameTypeConstant : public CJavaConstant {
  DynamicCastDeclarations;
public:
  CJavaNameTypeConstant(JavaConstantIndex nameIndex,
			JavaConstantIndex signatureIndex);
  virtual ~CJavaNameTypeConstant();
  virtual bool operator==(const CJavaConstant& other) const;
  virtual ConstantType GetConstantType() const;
  virtual unsigned long GetWidth() const;
  JavaConstantIndex GetNameIndex() const { return fNameIndex; }
  JavaConstantIndex GetSignatureIndex() const { return fSignatureIndex; }
  virtual void Disassemble(ostream& toStream) const;
  virtual void Compile(ostream& toStream) const;
private:
  JavaConstantIndex fNameIndex;
  JavaConstantIndex fSignatureIndex;
};


//
//  Class name : CJavaAscizConstant
//  Description : This represents a CONSTANT_Asciz value from the Java
//    class file format.
//
class CJavaAscizConstant : public CJavaConstant {
  DynamicCastDeclarations;
public:
  CJavaAscizConstant(const char* bytes, long length);
  CJavaAscizConstant(const unicode_string& unicodeConstant);
  virtual ~CJavaAscizConstant();
  virtual bool operator==(const CJavaConstant& other) const;
  virtual ConstantType GetConstantType() const;
  virtual unsigned long GetWidth() const;
  unicode_string GetUnicodeString() const { return fJavaString; }
  virtual void Disassemble(ostream& toStream) const;
  virtual void Compile(ostream& toStream) const;
private:
  unicode_string fJavaString;
};

#endif
