// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: IntermediateClass.h,v 1.7 1997/11/10 00:48:06 geppetto Exp $
#ifndef _IntermediateClass_h
#define _IntermediateClass_h
#pragma interface

#include "unicode_string.h"
#include "JavaAccessFlags.h"
#include "JavaClassFile.h"
#include "parser_decls.h"
#include "LocalVariableRecord.h"
#include <deque>
#include <list>
class CCompiler;
class CIntermediateFunction;
class COuterLocalExpression;
class CCompoundStatement;

//
//  Class name : CIntermediateClass
//  Description : This represents a Java class during the process of
//    compilation.  It's not intended as a usable end-value, but rather
//    as an intermediate phase, tightly bound to the mechanics of compilation.
//    Therefore, you can probably ignore this class unless you're playing with
//    compilation, which is why this class is 'friend CCompiler'
//    For all intents and purposes, this is basically a protected struct.
//
class CIntermediateClass {
  friend class CCompiler;
public:
  unicode_string GetName() const { return fName; }
  unicode_string GetShortName() const;

  CJavaClassFile* GetClass() { return fRealClass; }
protected:
  CIntermediateClass(const string& sourceFileName,
		     const unicode_string& className,
		     const unicode_string& shortName,
		     CJavaAccessFlags* adoptModifiers,
		     unicode_string* adoptExtends,
		     deque<unicode_string>* adoptInterfaces,
		     bool deprecated);
  ~CIntermediateClass();

  unsigned long NextSyntheticIndex() { return ++fSyntheticInnerCount; }
  
private:
  string fSourceFileName;
  unicode_string fName;
  unicode_string fShortName;
  CJavaAccessFlags fAccessFlags;
  unicode_string* fExtends;
  deque<unicode_string> fInterfaces;
  deque<CIntermediateFunction*> fFunctions;
  bool fAnyConstructors;
  unsigned short fStaticLocalVariables;
  CCompoundStatement* fStaticInitializer;
  StatementList fStaticDeclarations;
  StatementList fNonStaticDeclarations;
  CJavaClassFile* fRealClass;
  bool fDeprecated;
  CIntermediateClass* fInsideClass;
  bool fIsInner;
  bool fIsAnonymous;
  deque<CIntermediateClass*> fInnerClasses;
  unsigned long fSyntheticInnerCount;

  list<COuterLocalExpression*> fSyntheticLocals;

  CIntermediateFunction* fCurrentFunction;

  // temporary parsing state
  unsigned short fCurrentLocalVariable;
  deque<deque<unicode_string> > fVariableScopes;
  typedef unsigned short LocalVariableIndex;
  typedef map<unicode_string, CLocalVariableRecord,
              less<unicode_string> > LocalVariableTable;
  LocalVariableTable fLocalVariables;

};

#endif
