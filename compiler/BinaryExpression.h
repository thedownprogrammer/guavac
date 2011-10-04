// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: BinaryExpression.h,v 1.4 1997/04/28 19:25:03 geppetto Exp $
#ifndef _BinaryExpression_h
#define _BinaryExpression_h
#pragma interface

#include "Expression.h"
#include "JavaTypeSignature.h"

//
//  Class name : CBinaryExpression
//  Description : Represents an expression composed of a pair of expressions
//    joined with a binary operator.  Stores the operator and both expressions.
//    I put this in a separate file since it's so ungodly huge.
//
class CBinaryExpression : public CExpression {
  DynamicCastDeclarations;
public:
  CBinaryExpression(CExpression* leftExpression, unsigned short binaryOperator,
		    CExpression* rightExpression);
  CBinaryExpression(const CBinaryExpression& source);
  virtual ~CBinaryExpression();
  virtual CExpression* Clone() const { return new CBinaryExpression(*this); }
  virtual CCompileError* CreateConstantExpression(CExpression*& result,
			  CCompileContext& context) const;
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
  const CExpression* GetLeftArgument() const { return fLeft; }
  const CExpression* GetRightArgument() const { return fRight; }
protected:
  virtual CCompileError* HandleEvaluateType(CCompileContext& context,
					    CJavaTypeSignature& setType);
  CCompileError* AppendToStringBuffer(CCodeSequence& code,
	      CExpression* childExpression, CCompileContext& compiler,
	      unsigned short& stackUsed);
private:
  void ReduceInteger(CCodeSequence& code, CJavaTypeSignature::JavaType type);

  CExpression* fLeft;
  CExpression* fRight;
  unsigned short fOperator;   // taken from parser.y/h
};

#endif
