// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: VariableDeclaration.h,v 1.6 1997/08/21 16:30:12 geppetto Exp $
#ifndef _VariableDeclaration_h
#define _VariableDeclaration_h

#include "unicode_string.h"
#include "JavaFieldSignature.h"
class CExpression;
class CCodeSequence;
class CCompileError;
class CJavaMethodInfo;
class CJavaClassFile;
class CCompiler;
class CJavaAccessFlags;
class CCompileContext;

//
//  Class name : CVariableDeclaration
//  Description : This class is used as the intermediate representation of
//    a field or variable declaration in Java source, including the
//    initializer.  This value is either a straightforward expression or
//    a CArrayInitializer, which allows shorthand initialization of arrays.
//
class CVariableDeclaration {
public:
  CVariableDeclaration();
  CVariableDeclaration(const CJavaTypeSignature& type,
		       const unicode_string& name,
		       CExpression* adoptInitializer = 0,
		       bool final = false);
  CVariableDeclaration(const CVariableDeclaration& source);
  ~CVariableDeclaration();

  CJavaTypeSignature GetType() const;
  void SetType(const CJavaTypeSignature& type);
  const CJavaFieldSignature& GetSignature() const { return fSignature; }
  unicode_string GetName() const;
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		 CCompileContext& context, const CJavaAccessFlags& modifiers,
	         unsigned short& stackUsed);
  CCompileError* GetConstantValue(CExpression*& intoPointer,
				  CCompileContext& context);
  bool IsFinal() const { return fFinal; }
  void SetFinal(bool final);

private:
  CJavaFieldSignature fSignature;
  CExpression* fInitializer;
  bool fFinal;
};

#endif
