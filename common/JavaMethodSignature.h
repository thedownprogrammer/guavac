// $Id: JavaMethodSignature.h,v 1.5 1997/11/10 00:47:24 geppetto Exp $
// Copyright (c) 1995  David Engberg  All rights reserved
#ifndef _JavaMethodSignature_h
#define _JavaMethodSignature_h

#include "JavaTypeSignature.h"
#include <deque.h>
class ostream;

//
//  Class name : CJavaMethodSignature
//  Description : This class is similar to CJavaTypeSignature, but it
//    represents the signature for a function.  This includes the
//    arguments and the return value.
//    In the Java bytecode specification, this is represented as a compressed
//    string that looks like "(IC)LMyClass;" or something like that.  This
//    class is provided to encapsulate that string and allow it to only
//    be parsed once.
//    The 'Initialize' function must be called successfully on this object
//    before it is valid for use.
//
class CJavaMethodSignature {
public:
  CJavaMethodSignature();
  CJavaMethodSignature(const CJavaTypeSignature& returnType,
		       const unicode_string& name,
		       const deque<CJavaTypeSignature>& argumentTypes);
  CJavaMethodSignature(const CJavaMethodSignature& source);
  ~CJavaMethodSignature();
  CJavaMethodSignature& operator=(const CJavaMethodSignature& source);
  bool operator==(const CJavaMethodSignature& other) const;
  bool operator<(const CJavaMethodSignature& other) const;
  int Compare(const CJavaMethodSignature& other) const;
  bool EqualsIgnoreReturn(const CJavaMethodSignature& other) const;
  
  bool Initialize(const unicode_string& methodName,
		  const unicode_string& signatureString);

  unsigned long Hash() const { return fHashKey; }
  void Disassemble(ostream& toStream) const;
  unicode_string Disassemble() const;
  string CompileType() const;

  unicode_string GetName() const { return fMethodName; }
  CJavaTypeSignature GetType() const { return fReturnType; }
  deque<CJavaTypeSignature>::const_iterator ParametersBegin() const;
  deque<CJavaTypeSignature>::const_iterator ParametersEnd() const;
  void CopyParameters(deque<CJavaTypeSignature>& copyTo) const;
  unsigned long ParameterCount() const { return fArgumentTypes.size(); }
private:
  void SetHashKey();

  unicode_string fMethodName;
  deque<CJavaTypeSignature> fArgumentTypes;
  CJavaTypeSignature fReturnType;
  unsigned long fHashKey;
};

#endif
