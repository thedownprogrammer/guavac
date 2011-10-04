// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: IntermediateFunction.h,v 1.6 1997/11/10 00:48:06 geppetto Exp $
#ifndef _IntermediateFunction_h
#define _IntermediateFunction_h
#pragma interface

#include <deque>
#include "JavaMethodSignature.h"
#include "JavaAccessFlags.h"
#include "JavaFieldSignature.h"
#include "unicode_string.h"
#include "VariableDeclaration.h"
class CCompiler;
class CIntermediateClass;
class CCompoundStatement;

//
//  Class name : CIntermediateFunction
//  Description : This class is used during compilation as a half-baked
//    representation of a java function.  Since Java is not an L-attributed
//    language (you can use some names before they are declared), I have to
//    resort to this sort of messiness.
//    It is basically just a protected structure used by CCompiler.
//
class CIntermediateFunction {
  friend class CIntermediateClass;
  friend class CCompiler;
public:
  CIntermediateFunction(const CJavaMethodSignature& signature,
			const CJavaAccessFlags& modifiers,
			deque<unicode_string>* adoptThrows,
			unsigned long startLineNumber,
			bool deprecated = false);
  ~CIntermediateFunction();
  
  unsigned short GetLocalVariableLocation(unsigned short from) const;

private:
  CJavaMethodSignature fSignature;
  CJavaAccessFlags fAccessFlags;
  deque<unicode_string> fThrows;
  unsigned long fMaxLocalVariables;
  unsigned long fRealParametersSize;
  unsigned long fSyntheticParametersSize;
  CCompoundStatement* fBlock;
  unsigned long fStartLineNumber;
  unsigned long fEndLineNumber;
  bool fDeprecated;
  deque<CIntermediateClass*> fInnerClasses;
  CJavaMethodInfo* fMethodInfoAlias;

};

#endif
