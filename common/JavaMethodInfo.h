// $Id: JavaMethodInfo.h,v 1.6 1997/06/13 18:18:49 geppetto Exp $
// Copyright (c) 1995  David Engberg  All rights reserved
#ifndef _JavaMethodInfo_h
#define _JavaMethodInfo_h
#pragma interface

#include "JavaClassFile.h"
#include "JavaMethodSignature.h"
#include "JavaAccessFlags.h"
class CJavaCodeAttribute;
class CJavaExceptionsTable;
class ostream;
class CCompiler;

//
//  Class name : CJavaMethodInfo
//  Description : This class encapsulates the information found in one method
//    on a Java class.  This includes indices for the name and signature of
//    the method as well as access flags and code for the method.
//
class CJavaMethodInfo {
public:
  static CJavaMethodInfo* ParseBuffer(string::const_iterator& javaBuffer,
				      const CJavaClassFile& classFile,
				      bool interfaceOnly = false);

  const CJavaMethodSignature& GetSignature() const { return fSignature; }
  const CJavaAccessFlags& GetModifiers() const { return fAccessFlags; }
  const CJavaCodeAttribute* GetCode() const { return fCodeAttribute; }
  CJavaCodeAttribute* GetCode() { return fCodeAttribute; }

  void Disassemble(ostream& toStream) const;
  string Compile(CJavaClassFile& inClass) const;

  void AddReference() const;
  void RemoveReference() const;
  bool IsDeprecated() const;

  bool ThrowsExceptions() const;
  deque<unicode_string>::const_iterator ThrowsBegin() const;
  deque<unicode_string>::const_iterator ThrowsEnd() const;
protected:
  CJavaMethodInfo();
  ~CJavaMethodInfo();
private:
  friend class CCompiler;
  CJavaAccessFlags fAccessFlags;
  CJavaMethodSignature fSignature;
  CJavaCodeAttribute* fCodeAttribute;
  CJavaExceptionsTable* fExceptions;
  unsigned long fReferenceCount;
  bool fDeprecated;
};

#endif
