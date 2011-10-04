// $Id: JavaFieldInfo.h,v 1.6 1997/06/13 18:18:49 geppetto Exp $
// Copyright (c) 1995  David Engberg  All rights reserved
#ifndef _JavaFieldInfo_h
#define _JavaFieldInfo_h

#include "JavaFieldSignature.h"
#include "JavaAccessFlags.h"
#include "JavaConstant.h"
class CJavaConstantValueAttribute;
class CJavaClassFile;
class ostream;

//
//  Class name : CJavaFieldInfo
//  Description : The field info structure is used to store the information
//    about a field on a java class object.  This includes the constant
//    pool indices for the field's name, signature, access permissions, and
//    possible constant value.
//    This class is not meant to be subclassed.
//
class CJavaFieldInfo {
public:
  CJavaFieldInfo();
  CJavaFieldInfo(const CJavaAccessFlags& modifiers,
		 const CJavaFieldSignature& signature,
		 CJavaConstantValueAttribute* adoptConstValue = 0,
		 bool deprecated = false);
  static CJavaFieldInfo* ParseBuffer(string::const_iterator& javaBuffer,
				     const CJavaClassFile& classFile);

  CJavaFieldInfo(const CJavaFieldInfo& source);
  ~CJavaFieldInfo();
  CJavaFieldInfo& operator=(const CJavaFieldInfo& source);

  const CJavaFieldSignature& GetSignature() const { return fSignature; }
  const CJavaAccessFlags& GetModifiers() const { return fAccessFlags; }
  void Disassemble(ostream& toStream) const;
  string Compile(CJavaClassFile& inClass) const;
  
  bool IsConstant() const { return fConstantValue != 0; }
  JavaConstantIndex GetConstantIndex() const;
  void SetConstantIndex(JavaConstantIndex index);
  bool IsDeprecated() const;

private:
  CJavaFieldSignature fSignature;
  CJavaAccessFlags fAccessFlags;
  CJavaConstantValueAttribute* fConstantValue;
  bool fDeprecated;
};

#endif
