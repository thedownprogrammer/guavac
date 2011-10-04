// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: BinaryExpression.C,v 1.8 1997/04/28 19:25:03 geppetto Exp $
#pragma implementation
#include "BinaryExpression.h"
#include "Expression.h"
#include "parser_decls.h"
#include "parser.h"  // for token values
#include "Compiler.h"
#include "JavaCodeAttribute.h"
#include "CompileContext.h"
#include "CodeSequence.h"
#include <cmath>

static const unicode_string kStringBufferName =
               ::StringToUnicode("java/lang/StringBuffer");
static const unicode_string kAppendName = ::StringToUnicode("append");
static const unicode_string kConcatName = ::StringToUnicode("concat");
static const unicode_string kToStringName = ::StringToUnicode("toString");
static const unicode_string kValueOfName = ::StringToUnicode("valueOf");

//
//  Method name : CBinaryExpression
//  Description : Constructs a binary expression from the two child expressions
//    and the operator.  The operator is stored as an unsigned short, which is
//    meant to hold characters (in the case of '+', '-', etc...) and token
//    values generated from the parser.y yacc specification.  These constants
//    are found in parser.h.
//
CBinaryExpression::CBinaryExpression(CExpression* leftExpression,
                   unsigned short binaryOperator, CExpression* rightExpression)
  : CExpression(CJavaTypeSignature::Void, false),
    fLeft(leftExpression),
    fRight(rightExpression),
    fOperator(binaryOperator)
{
  assert(leftExpression != 0 && rightExpression != 0);
  rightExpression->SetLValue(false);
}

//
//  Method name : CBinaryExpression
//  Description : Copy constructor.
//
CBinaryExpression::CBinaryExpression(const CBinaryExpression& source)
  : CExpression(source), fLeft(0), fRight(0)
{
  if (source.fLeft != 0) {
    fLeft = source.fLeft->Clone();
  }
  if (source.fRight != 0) {
    fRight = source.fRight->Clone();
  }
}

//
//  Method name : ~CBinaryExpression
//  Description : Destructor.
//
CBinaryExpression::~CBinaryExpression()
{
  delete fLeft;
  delete fRight;
}

//
//  Method name : HandleEvaluateType
//  Description : This is an overrideable hook that is used by EvaluateType to
//    polymorphically force an expression to calculate its type.  It either
//    fills in 'setType' based on the evaluated type of this expression and
//    then returns 0, or it returns an error object to the user explaining why
//    the type couldn't be determined.
//
CCompileError*
CBinaryExpression::HandleEvaluateType(CCompileContext& context,
				      CJavaTypeSignature& setType)
{
  assert(fLeft != 0 && fRight != 0);
  CJavaTypeSignature leftType;
  bool isAssignment = false;
  switch(fOperator) {
  case '=': case SHIFT_RIGHT_EQUALS: case FILL_SHIFT_RIGHT_EQUALS:
  case SHIFT_LEFT_EQUALS: case AND_EQUALS: case XOR_EQUALS:
  case OR_EQUALS: case ADD_EQUALS: case SUB_EQUALS:
  case MUL_EQUALS: case DIV_EQUALS:  case MOD_EQUALS:
    isAssignment = true;
  }
  CCompileError* error = EvaluateType(fLeft, context, leftType, isAssignment);
  if (error == 0) {
    CJavaTypeSignature rightType;
    error = EvaluateType(fRight, context, rightType);
    if (error == 0 && (leftType == CJavaTypeSignature::kVoid ||
		       rightType == CJavaTypeSignature::kVoid)) {
      error = MakeError("Invalid use of void-typed expression");
    }
    if (error == 0) {
      switch (fOperator) {
      case OR:
      case AND:
	if (rightType != CJavaTypeSignature::kBoolean ||
	    leftType != CJavaTypeSignature::kBoolean) {
	  error =
	    MakeError("Non-boolean value used in conditional expression");
	} else {
	  setType = CJavaTypeSignature::kBoolean;
	}
	break;
      case BITSHIFT_RIGHT:
      case FILL_SHIFT_RIGHT:
      case SHIFT_LEFT:
	UnaryPromote(fLeft, context);
	UnaryPromote(fRight, context);
	leftType = fLeft->GetType();
	rightType = fRight->GetType();
	if (leftType != CJavaTypeSignature::kInteger &&
	    leftType != CJavaTypeSignature::kLong) {
	  unicode_string errorMessage = leftType.Disassemble();
	  errorMessage +=
	    ::StringToUnicode(" value used in a shift expression");
	  error = MakeError(errorMessage);
	}
	if (rightType == CJavaTypeSignature::kLong) {
	  error = CCastExpression::CastPointer(fRight,
			  CJavaTypeSignature::kInteger, context);
	} else if (rightType != CJavaTypeSignature::kInteger) {
	  unicode_string errorMessage = rightType.Disassemble();
	  errorMessage +=
	    ::StringToUnicode(" value used in a shift expression");
	  error = MakeError(errorMessage);
	}
	setType = leftType;
	break;
      case '+':
	if (rightType == CJavaTypeSignature::kString ||
	    leftType == CJavaTypeSignature::kString) {
	  setType = CJavaTypeSignature::kString;
	} else {
	  if (!BinaryPromote(fLeft, fRight, context)) {
	    error = MakeError("Invalid types used with '+' operator.");
	  } else {
	    setType = fLeft->GetType();
	  }
	}
	break;
      case '/':
      case '%':
	{
	  COrdinalLiteral* integerLiteral =
	    DYNAMIC_CAST(COrdinalLiteral, fRight);
	  if (integerLiteral != 0 && integerLiteral->GetLong() == 0) {
	    error = MakeError("Invalid division by zero");
	  }
	}
	// fall through ...
      case '-':
      case '*':
	if (error == 0 && !BinaryPromote(fLeft, fRight, context)) {
	  error = MakeError("Invalid types used in arithmatic expression");
	}
	setType = fLeft->GetType();
	break;
      case LTEQ:
      case GTEQ:
      case '<':
      case '>':
	if (!BinaryPromote(fLeft, fRight, context)) {
	  error = MakeError("Incompatible types used in comparison");
	}
	setType = CJavaTypeSignature(CJavaTypeSignature::Boolean);
	break;
      case '|':
      case '&':
      case '^':
	// 9.16 -- both boolean or binary promotion on both, any integral
	if (rightType == CJavaTypeSignature::kBoolean &&
	    leftType == CJavaTypeSignature::kBoolean) {
	  setType = CJavaTypeSignature::kBoolean;
	} else {
	  if (!BinaryPromote(fLeft, fRight, context)) {
	    error = MakeError("Invalid types used in bitwise expression");
	  } else {
	    if (!fLeft->GetType().IsOrdinal() ||
		!fRight->GetType().IsOrdinal()) {
	      error = MakeError("Non-integer type used in bitwise expression");
	    } else {
	      setType = fLeft->GetType();
	    }
	  }
	}
	break;
      case '=':
	if (!context.GetCompiler().SameType(leftType, rightType)) {
	  error =
	    CCastExpression::ImplicitCastPointer(fRight, leftType, context);
	  rightType = fRight->GetType();
	}
	if (error == 0) {
	  if (! fLeft->IsLValue()) {
	    error = MakeError("Invalid value on left-hand side of assignment");
	  } else {
	    setType = leftType;
	  }
	}
	break;
      case SHIFT_RIGHT_EQUALS:
      case FILL_SHIFT_RIGHT_EQUALS:
      case SHIFT_LEFT_EQUALS:
	if (!leftType.IsOrdinal()) {
	  error = MakeError("Invalid shift-assignment on non-integral value");
	} else {
	  if (!UnaryPromote(fRight, context)) {
	    error =
	     MakeError("Cannot convert right side of shift-assignment to int");
	  } else {
	    setType = leftType;
	  }
	}
	break;
      case AND_EQUALS:
      case XOR_EQUALS:
      case OR_EQUALS:
	if (rightType == CJavaTypeSignature::kBoolean &&
	    leftType == CJavaTypeSignature::kBoolean) {
	  setType = CJavaTypeSignature::kBoolean;
	} else {
	  UnaryPromote(fRight, context);
	  if (!leftType.IsOrdinal() ||
	      !fRight->GetType().IsOrdinal()) {
	    error = MakeError("Non-integer type used in bitwise assignment");
	  } else {
	    if (!context.GetCompiler().SameType(leftType, rightType)) {
	      error = CCastExpression::CastPointer(fRight, leftType, context);
	    }
	    setType = leftType;
	  }
	}
	break;
      case ADD_EQUALS:
	if (leftType == CJavaTypeSignature::kString) {
	  setType = CJavaTypeSignature::kString;
	} else if (!leftType.IsNumeric() || !rightType.IsNumeric()) {
	  error = MakeError("Non-numeric value used in compound assignment.");
	} else {
	  if (!context.GetCompiler().SameType(leftType, rightType)) {
	    error = CCastExpression::CastPointer(fRight, leftType, context);
	  }
	  setType = leftType;
	}
	break;
      case SUB_EQUALS:
      case MUL_EQUALS:
      case DIV_EQUALS:
      case MOD_EQUALS:
	if (!leftType.IsNumeric() || !rightType.IsNumeric()) {
	  error = MakeError("Non-numeric value used in compound assignment.");
	} else {
	  if (!context.GetCompiler().SameType(leftType, rightType)) {
	    error = CCastExpression::CastPointer(fRight, leftType, context);
	  }
	  setType = leftType;
	}
	break;
      case EQUAL_COMPARE:
      case NOT_EQUAL:
	// 9.15 -- boolean both, reference both, or binary promote
	// if reference, one must descend from other (or be same type)
	// don't forget about null
	setType = CJavaTypeSignature::kBoolean;
	if (!context.GetCompiler().SameType(leftType, rightType)) {
	  if (leftType.IsReference() && rightType.IsReference()) {
	    if (leftType == CJavaTypeSignature::kNullType) {
	      if (rightType != CJavaTypeSignature::kNullType) {
		error =
		  CCastExpression::CastPointer(fLeft, rightType, context);
	      }
	    } else if (rightType == CJavaTypeSignature::kNullType) {
	      error = CCastExpression::CastPointer(fRight, leftType, context);
	    } else if (context.GetCompiler().AssignableSubtype(leftType, rightType)) {
	      error = CCastExpression::CastPointer(fLeft, rightType, context);
	    } else if (context.GetCompiler().AssignableSubtype(rightType, leftType)) {
	      error = CCastExpression::CastPointer(fRight, leftType, context);
	    } else {
	      unicode_string errorString =
		::StringToUnicode("Can't compare reference of ");
	      errorString += leftType.Disassemble();
	      errorString += ::StringToUnicode(" to a reference of ");
	      errorString += rightType.Disassemble();
	      error = MakeError(errorString);
	    }
	  } else if (leftType.IsNumeric() && rightType.IsNumeric()) {
	    error = CCastExpression::CastPointer(fRight, leftType, context);
	  } else {
	    unicode_string errorString =
	      ::StringToUnicode("Can't compare ");
	    errorString += leftType.Disassemble();
	    errorString += ::StringToUnicode(" value to a ");
	    errorString += rightType.Disassemble();
	    errorString += ::StringToUnicode(" value.");
	    error = MakeError(errorString);
	  }
	}
	break;
      default:
	assert(0);
      }
    }
  }
  return error;
}

//
//  Method name : GenerateCode
//  Description : This is used during the second pass of compilation to
//    make the actual bytecodes needed to execute this expression under the
//    Java VM.  The instructions are appended onto the end of the provided
//    block, and the expression has access to the compiler, class, and
//    method to help sort out its context.  'stackUsed' is filled with the
//    maximum expression stack depth achieved by this expression.
//
CCompileError*
CBinaryExpression::GenerateCode(CCodeSequence& code,
				CCompileContext& context,
				unsigned short& stackUsed,
				bool isStatement)
{
  CCompileError* error = 0;
  unsigned short beforeStack, afterStack, leftStack, rightStack;
  switch (fOperator) {
  case '=':
    error = fLeft->StartStoreCode(code, context, beforeStack, false);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, stackUsed);
    }
    if (error == 0) {
      error = fLeft->EndStoreCode(code, context, afterStack, isStatement);
      stackUsed += beforeStack + afterStack;
    }
    break;
  case OR:
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      unsigned long branchInstruction = code.size();
      code.Append(CJavaCodeAttribute::ifne, fLeft->GetLineNumber());
      error = fRight->GenerateCode(code, context, rightStack);
      unsigned long ifneInstruction = code.size();
      code.Append(CJavaCodeAttribute::ifne, fLeft->GetLineNumber());
      code.Append(CJavaCodeAttribute::iconst_0, fLineNumber);
      unsigned long gotoInstruction = code.size();
      code.Append(CJavaCodeAttribute::op_goto, fLineNumber);
      unsigned long trueLabel = code.CreateBlockLabel();
      code[ifneInstruction].fArguments.u4 = trueLabel;
      code[branchInstruction].fArguments.u4 = trueLabel;
      code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
      code[gotoInstruction].fArguments.u4 = code.CreateBlockLabel();
      stackUsed = ::max(leftStack, rightStack);
    }
    break;
  case AND:
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      unsigned long branchInstruction = code.size();
      code.Append(CJavaCodeAttribute::ifeq, fLeft->GetLineNumber());
      error = fRight->GenerateCode(code, context, rightStack);
      unsigned long ifInstruction = code.size();
      code.Append(CJavaCodeAttribute::ifne, fLeft->GetLineNumber());
      code[branchInstruction].fArguments.u4 = code.CreateBlockLabel();
      code.Append(CJavaCodeAttribute::iconst_0, fLineNumber);
      unsigned long gotoInstruction = code.size();
      code.Append(CJavaCodeAttribute::op_goto, fLineNumber);
      code[ifInstruction].fArguments.u4 = code.CreateBlockLabel();
      code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
      code[gotoInstruction].fArguments.u4 = code.CreateBlockLabel();
      stackUsed = ::max(leftStack, rightStack);
    }
    break;
  case EQUAL_COMPARE:
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      unsigned long branchInstruction = code.size();
      if (fLeft->GetType().IsReference()) {
	code.Append(CJavaCodeAttribute::if_acmpeq, fLineNumber);
      } else {
	switch (fLeft->GetType().GetBaseType()) {
	case CJavaTypeSignature::Integer:
	case CJavaTypeSignature::Boolean:
	case CJavaTypeSignature::Byte:
	case CJavaTypeSignature::Short:
	case CJavaTypeSignature::Character:
	  code.Append(CJavaCodeAttribute::if_icmpeq, fLineNumber);
	  break;
	case CJavaTypeSignature::Float:
	  code.Append(CJavaCodeAttribute::fcmpl, fLineNumber);
	  branchInstruction = code.size();
	  code.Append(CJavaCodeAttribute::ifeq, fLineNumber);
	  break;
	case CJavaTypeSignature::LongInteger:
	  code.Append(CJavaCodeAttribute::lcmp, fLineNumber);
	  branchInstruction = code.size();
	  code.Append(CJavaCodeAttribute::ifeq, fLineNumber);
	  break;
	case CJavaTypeSignature::Double:
	  code.Append(CJavaCodeAttribute::dcmpg, fLineNumber);
	  branchInstruction = code.size();
	  code.Append(CJavaCodeAttribute::ifeq, fLineNumber);
	  break;
	}
      }
      code.Append(CJavaCodeAttribute::iconst_0, fLineNumber);
      unsigned long gotoInstruction = code.size();
      code.Append(CJavaCodeAttribute::op_goto, fLineNumber);
      code[branchInstruction].fArguments.u4 = code.CreateBlockLabel();
      code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
      code[gotoInstruction].fArguments.u4 = code.CreateBlockLabel();
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case NOT_EQUAL:
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      unsigned long branchInstruction = code.size();
      if (fLeft->GetType().IsReference()) {
	code.Append(CJavaCodeAttribute::if_acmpne, fLineNumber);
      } else {
	switch (fLeft->GetType().GetBaseType()) {
	case CJavaTypeSignature::Integer:
	case CJavaTypeSignature::Boolean:
	case CJavaTypeSignature::Byte:
	case CJavaTypeSignature::Short:
	case CJavaTypeSignature::Character:
	  code.Append(CJavaCodeAttribute::if_icmpne, fLineNumber);
	  break;
	case CJavaTypeSignature::Float:
	  code.Append(CJavaCodeAttribute::fcmpl, fLineNumber);
	  branchInstruction = code.size();
	  code.Append(CJavaCodeAttribute::ifne, fLineNumber);
	  break;
	case CJavaTypeSignature::LongInteger:
	  code.Append(CJavaCodeAttribute::lcmp, fLineNumber);
	  branchInstruction = code.size();
	  code.Append(CJavaCodeAttribute::ifne, fLineNumber);
	  break;
	case CJavaTypeSignature::Double:
	  code.Append(CJavaCodeAttribute::dcmpg, fLineNumber);
	  branchInstruction = code.size();
	  code.Append(CJavaCodeAttribute::ifne, fLineNumber);
	  break;
	}
      }
      code.Append(CJavaCodeAttribute::iconst_0, fLineNumber);
      unsigned long gotoInstruction = code.size();
      code.Append(CJavaCodeAttribute::op_goto, fLineNumber);
      code[branchInstruction].fArguments.u4 = code.CreateBlockLabel();
      code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
      code[gotoInstruction].fArguments.u4 = code.CreateBlockLabel();
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case LTEQ:
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      unsigned long branchInstruction = code.size();
      switch (fLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Integer:
      case CJavaTypeSignature::Byte:
      case CJavaTypeSignature::Short:
      case CJavaTypeSignature::Character:
	code.Append(CJavaCodeAttribute::if_icmple, fLineNumber);
	break;
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::fcmpl, fLineNumber);
	branchInstruction = code.size();
	code.Append(CJavaCodeAttribute::ifle, fLineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::lcmp, fLineNumber);
	branchInstruction = code.size();
	code.Append(CJavaCodeAttribute::ifle, fLineNumber);
	break;
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::dcmpg, fLineNumber);
	branchInstruction = code.size();
	code.Append(CJavaCodeAttribute::ifle, fLineNumber);
	break;
      default:
	assert(0);
      }
      code.Append(CJavaCodeAttribute::iconst_0, fLineNumber);
      unsigned long gotoInstruction = code.size();
      code.Append(CJavaCodeAttribute::op_goto, fLineNumber);
      code[branchInstruction].fArguments.u4 = code.CreateBlockLabel();
      code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
      code[gotoInstruction].fArguments.u4 = code.CreateBlockLabel();
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case GTEQ:
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      unsigned long branchInstruction = code.size();
      switch (fLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Integer:
      case CJavaTypeSignature::Byte:
      case CJavaTypeSignature::Short:
      case CJavaTypeSignature::Character:
	code.Append(CJavaCodeAttribute::if_icmpge, fLineNumber);
	break;
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::fcmpl, fLineNumber);
	branchInstruction = code.size();
	code.Append(CJavaCodeAttribute::ifge, fLineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::lcmp, fLineNumber);
	branchInstruction = code.size();
	code.Append(CJavaCodeAttribute::ifge, fLineNumber);
	break;
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::dcmpg, fLineNumber);
	branchInstruction = code.size();
	code.Append(CJavaCodeAttribute::ifge, fLineNumber);
	break;
      default:
	assert(0);
      }
      code.Append(CJavaCodeAttribute::iconst_0, fLineNumber);
      unsigned long gotoInstruction = code.size();
      code.Append(CJavaCodeAttribute::op_goto, fLineNumber);
      code[branchInstruction].fArguments.u4 = code.CreateBlockLabel();
      code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
      code[gotoInstruction].fArguments.u4 = code.CreateBlockLabel();
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case '<':
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      unsigned long branchInstruction = code.size();
      switch (fLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Integer:
      case CJavaTypeSignature::Byte:
      case CJavaTypeSignature::Short:
      case CJavaTypeSignature::Character:
	code.Append(CJavaCodeAttribute::if_icmplt, fLineNumber);
	break;
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::fcmpl, fLineNumber);
	branchInstruction = code.size();
	code.Append(CJavaCodeAttribute::iflt, fLineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::lcmp, fLineNumber);
	branchInstruction = code.size();
	code.Append(CJavaCodeAttribute::iflt, fLineNumber);
	break;
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::dcmpg, fLineNumber);
	branchInstruction = code.size();
	code.Append(CJavaCodeAttribute::iflt, fLineNumber);
	break;
      default:
	assert(0);
      }
      code.Append(CJavaCodeAttribute::iconst_0, fLineNumber);
      unsigned long gotoInstruction = code.size();
      code.Append(CJavaCodeAttribute::op_goto, fLineNumber);
      code[branchInstruction].fArguments.u4 = code.CreateBlockLabel();
      code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
      code[gotoInstruction].fArguments.u4 = code.CreateBlockLabel();
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case '>':
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      unsigned long branchInstruction = code.size();
      switch (fLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Integer:
      case CJavaTypeSignature::Byte:
      case CJavaTypeSignature::Short:
      case CJavaTypeSignature::Character:
	code.Append(CJavaCodeAttribute::if_icmpgt, fLineNumber);
	break;
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::fcmpl, fLineNumber);
	branchInstruction = code.size();
	code.Append(CJavaCodeAttribute::ifgt, fLineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::lcmp, fLineNumber);
	branchInstruction = code.size();
	code.Append(CJavaCodeAttribute::ifgt, fLineNumber);
	break;
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::dcmpg, fLineNumber);
	branchInstruction = code.size();
	code.Append(CJavaCodeAttribute::ifgt, fLineNumber);
	break;
      default:
	assert(0);
      }
      code.Append(CJavaCodeAttribute::iconst_0, fLineNumber);
      unsigned long gotoInstruction = code.size();
      code.Append(CJavaCodeAttribute::op_goto, fLineNumber);
      code[branchInstruction].fArguments.u4 = code.CreateBlockLabel();
      code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
      code[gotoInstruction].fArguments.u4 = code.CreateBlockLabel();
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case BITSHIFT_RIGHT:
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      if (fLeft->GetType() == CJavaTypeSignature::kLong) {
	code.Append(CJavaCodeAttribute::lshr, fLineNumber);
      } else {
	assert(fLeft->GetType() == CJavaTypeSignature::kInteger);
	code.Append(CJavaCodeAttribute::ishr, fLineNumber);
      }
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case FILL_SHIFT_RIGHT:
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      if (fLeft->GetType() == CJavaTypeSignature::kLong) {
	code.Append(CJavaCodeAttribute::lushr, fLineNumber);
      } else {
	assert(fLeft->GetType() == CJavaTypeSignature::kInteger);
	code.Append(CJavaCodeAttribute::iushr, fLineNumber);
      }
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));    }
    break;
  case SHIFT_LEFT:
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      if (fLeft->GetType() == CJavaTypeSignature::kLong) {
	code.Append(CJavaCodeAttribute::lshl, fLineNumber);
      } else {
	assert(fLeft->GetType() == CJavaTypeSignature::kInteger);
	code.Append(CJavaCodeAttribute::ishl, fLineNumber);
      }
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case '+':
    assert(!isStatement);
    if (GetType() == CJavaTypeSignature::kString) {
      code.Append(CJavaCodeAttribute::op_new,
		  context.GetClass().AddClassConstant(kStringBufferName),
		  fLineNumber);
      code.Append(CJavaCodeAttribute::dup, fLineNumber);
      deque<CJavaTypeSignature> noArguments;
      CJavaMethodSignature initSignature(CJavaTypeSignature::kVoid,
					 CCompiler::kConstructorName,
					 noArguments);
      JavaConstantIndex index =
	context.GetClass().AddMethodConstant(kStringBufferName, initSignature);
      code.Append(CJavaCodeAttribute::invokenonvirtual, index, fLineNumber);
      error = AppendToStringBuffer(code, fLeft, context, leftStack);
      if (error == 0) {
	error =
	  AppendToStringBuffer(code, fRight, context, rightStack);
      }
      stackUsed = ::max(leftStack, rightStack) + 1;
      CJavaMethodSignature toStringSignature(CJavaTypeSignature::kString,
					     kToStringName, noArguments);
      index = context.GetClass().AddMethodConstant(kStringBufferName,
						   toStringSignature);
      code.Append(CJavaCodeAttribute::invokevirtual, index, fLineNumber);
    } else {
      error = fLeft->GenerateCode(code, context, leftStack);
      if (error == 0) {
	error = fRight->GenerateCode(code, context, rightStack);
      }
      if (error == 0) {
	switch (fLeft->GetType().GetBaseType()) {
	case CJavaTypeSignature::Float:
	  code.Append(CJavaCodeAttribute::fadd, fLineNumber);
	  break;
	case CJavaTypeSignature::LongInteger:
	  code.Append(CJavaCodeAttribute::ladd, fLineNumber);
	  break;
	case CJavaTypeSignature::Double:
	  code.Append(CJavaCodeAttribute::dadd, fLineNumber);
	  break;
	default:
	  code.Append(CJavaCodeAttribute::iadd, fLineNumber);
	  break;	
	}
	stackUsed = ::max(leftStack,
		   (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
      }
    }
    break;
  case '-':
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      switch (fLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::fsub, fLineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::lsub, fLineNumber);
	break;
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::dsub, fLineNumber);
	break;
      default:
	code.Append(CJavaCodeAttribute::isub, fLineNumber);
	break;	
      }
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case '*':
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      switch (fLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::fmul, fLineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::lmul, fLineNumber);
	break;
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::dmul, fLineNumber);
	break;
      default:
	code.Append(CJavaCodeAttribute::imul, fLineNumber);
	break;	
      }
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case '/':
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      switch (fLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::fdiv, fLineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::ldiv, fLineNumber);
	break;
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::ddiv, fLineNumber);
	break;
      default:
	code.Append(CJavaCodeAttribute::idiv, fLineNumber);
	break;	
      }
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case '%':
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      switch (fLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::frem, fLineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::lrem, fLineNumber);
	break;
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::drem, fLineNumber);
	break;
      default:
	code.Append(CJavaCodeAttribute::irem, fLineNumber);
	break;	
      }
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case '|':
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      if (fLeft->GetType() == CJavaTypeSignature::kLong) {
	code.Append(CJavaCodeAttribute::lor, fLineNumber);
      } else {
	code.Append(CJavaCodeAttribute::ior, fLineNumber);
      }
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case '&':
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      if (fLeft->GetType() == CJavaTypeSignature::kLong) {
	code.Append(CJavaCodeAttribute::land, fLineNumber);
      } else {
	code.Append(CJavaCodeAttribute::iand, fLineNumber);
      }
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case '^':
    assert(!isStatement);
    error = fLeft->GenerateCode(code, context, leftStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
    }
    if (error == 0) {
      if (fLeft->GetType() == CJavaTypeSignature::kLong) {
	code.Append(CJavaCodeAttribute::lxor, fLineNumber);
      } else {
	code.Append(CJavaCodeAttribute::ixor, fLineNumber);
      }
      stackUsed = ::max(leftStack,
		 (unsigned short)(rightStack + fLeft->GetType().GetWidth()));
    }
    break;
  case ADD_EQUALS:
    error = fLeft->StartStoreCode(code, context, beforeStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
      stackUsed = (unsigned short)rightStack +
	            fLeft->GetType().GetWidth() + beforeStack;
      if (GetType() == CJavaTypeSignature::kString) {
	CJavaTypeSignature rightType = fRight->GetType();
	unicode_string stringClassName;
	CJavaTypeSignature::kString.GetBaseClassName(stringClassName);
	if (!(rightType == CJavaTypeSignature::kString)) {
	  deque<CJavaTypeSignature> valueOfArguments;
	  if (rightType.IsReference()) {
	    valueOfArguments.push_back(
			    CJavaTypeSignature(CCompiler::kObjectName));
	  } else {
	    valueOfArguments.push_back(rightType);
	  }
	  CJavaMethodSignature valueOfSignature(CJavaTypeSignature::kString,
					  kValueOfName, valueOfArguments);
	  JavaConstantIndex valueOfIndex =
	    context.GetClass().AddMethodConstant(stringClassName,
						 valueOfSignature);
	  code.Append(CJavaCodeAttribute::invokestatic, valueOfIndex,
		      fLineNumber);
	}
	deque<CJavaTypeSignature> arguments;
	arguments.push_back(CJavaTypeSignature::kString);
	CJavaMethodSignature concatSignature(CJavaTypeSignature::kString,
					     kConcatName, arguments);
	JavaConstantIndex index =
	  context.GetClass().AddMethodConstant(stringClassName,
					       concatSignature);
	code.Append(CJavaCodeAttribute::invokenonvirtual, index, fLineNumber);
      } else {
	switch (fLeft->GetType().GetBaseType()) {
	case CJavaTypeSignature::Float:
	  code.Append(CJavaCodeAttribute::fadd, fLineNumber);
	  break;
	case CJavaTypeSignature::LongInteger:
	  code.Append(CJavaCodeAttribute::ladd, fLineNumber);
	  break;
	case CJavaTypeSignature::Double:
	  code.Append(CJavaCodeAttribute::dadd, fLineNumber);
	  break;
	default:
	  code.Append(CJavaCodeAttribute::iadd, fLineNumber);
	  ReduceInteger(code, fLeft->GetType().GetBaseType());
	  break;
	}
      }
    }
    if (error == 0) {
      error =
	fLeft->EndStoreCode(code, context, afterStack, isStatement);
      stackUsed = ::max(stackUsed, (unsigned short)(afterStack +
                 fLeft->GetType().GetWidth() + fRight->GetType().GetWidth() +
		 beforeStack));
    }
    break;
  case SUB_EQUALS:
    error = fLeft->StartStoreCode(code, context, beforeStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
      stackUsed =
	(unsigned short)rightStack + fLeft->GetType().GetWidth() + beforeStack;
      switch (fLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::fsub, fLineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::lsub, fLineNumber);
	break;
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::dsub, fLineNumber);
	break;
      default:
	code.Append(CJavaCodeAttribute::isub, fLineNumber);
	ReduceInteger(code, fLeft->GetType().GetBaseType());
	break;	
      }
    }
    if (error == 0) {
      error = fLeft->EndStoreCode(code, context, afterStack,
				  isStatement);
      stackUsed = ::max(stackUsed, (unsigned short)(afterStack +
                 fLeft->GetType().GetWidth() + fRight->GetType().GetWidth() +
		 beforeStack));
    }
    break;
  case MUL_EQUALS:
    error = fLeft->StartStoreCode(code, context, beforeStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
      stackUsed =
	(unsigned short)rightStack + fLeft->GetType().GetWidth() + beforeStack;
      switch (fLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::fmul, fLineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::lmul, fLineNumber);
	break;
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::dmul, fLineNumber);
	break;
      default:
	code.Append(CJavaCodeAttribute::imul, fLineNumber);
	ReduceInteger(code, fLeft->GetType().GetBaseType());
	break;
      }
    }
    if (error == 0) {
      error = fLeft->EndStoreCode(code, context, afterStack,
				  isStatement);
      stackUsed = ::max(stackUsed, (unsigned short)(afterStack +
                 fLeft->GetType().GetWidth() + fRight->GetType().GetWidth() +
		 beforeStack));
    }
    break;
  case DIV_EQUALS:
    error = fLeft->StartStoreCode(code, context, beforeStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
      stackUsed =
	(unsigned short)rightStack + fLeft->GetType().GetWidth() + beforeStack;
      switch (fLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::fdiv, fLineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::ldiv, fLineNumber);
	break;
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::ddiv, fLineNumber);
	break;
      default:
	code.Append(CJavaCodeAttribute::idiv, fLineNumber);
	ReduceInteger(code, fLeft->GetType().GetBaseType());
	break;	
      }
    }
    if (error == 0) {
      error = fLeft->EndStoreCode(code, context, afterStack,
				  isStatement);
      stackUsed = ::max(stackUsed, (unsigned short)(afterStack +
                 fLeft->GetType().GetWidth() + fRight->GetType().GetWidth() +
		 beforeStack));
    }
    break;
  case MOD_EQUALS:
    error = fLeft->StartStoreCode(code, context, beforeStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
      stackUsed =
	(unsigned short)rightStack + fLeft->GetType().GetWidth() + beforeStack;
      switch (fLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::frem, fLineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::lrem, fLineNumber);
	break;
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::drem, fLineNumber);
	break;
      default:
	code.Append(CJavaCodeAttribute::irem, fLineNumber);
	ReduceInteger(code, fLeft->GetType().GetBaseType());
	break;	
      }
    }
    if (error == 0) {
      error = fLeft->EndStoreCode(code, context, afterStack,
				  isStatement);
      stackUsed = ::max(stackUsed, (unsigned short)(afterStack +
                 fLeft->GetType().GetWidth() + fRight->GetType().GetWidth() +
		 beforeStack));
    }
    break;
  case FILL_SHIFT_RIGHT_EQUALS:
    error = fLeft->StartStoreCode(code, context, beforeStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context, rightStack);
      stackUsed =
	(unsigned short)rightStack + fLeft->GetType().GetWidth() + beforeStack;
      if (fLeft->GetType() == CJavaTypeSignature::kLong) {
	code.Append(CJavaCodeAttribute::lushr, fLineNumber);
      } else {
	code.Append(CJavaCodeAttribute::iushr, fLineNumber);
      }
    }
    if (error == 0) {
      error = fLeft->EndStoreCode(code, context,
				  afterStack, isStatement);
      stackUsed = ::max(stackUsed, (unsigned short)(afterStack +
                 fLeft->GetType().GetWidth() + fRight->GetType().GetWidth() +
		 beforeStack));
    }
    break;
  case SHIFT_LEFT_EQUALS:
    error = fLeft->StartStoreCode(code, context,
				  beforeStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context,
				   rightStack);
      stackUsed =
	(unsigned short)rightStack + fLeft->GetType().GetWidth() + beforeStack;
      if (fLeft->GetType() == CJavaTypeSignature::kLong) {
	code.Append(CJavaCodeAttribute::lshl, fLineNumber);
      } else {
	code.Append(CJavaCodeAttribute::ishl, fLineNumber);
      }
    }
    if (error == 0) {
      error = fLeft->EndStoreCode(code, context,
				  afterStack, isStatement);
      stackUsed = ::max(stackUsed, (unsigned short)(afterStack +
                 fLeft->GetType().GetWidth() + fRight->GetType().GetWidth() +
		 beforeStack));
    }
    break;
  case SHIFT_RIGHT_EQUALS:
    error = fLeft->StartStoreCode(code, context,
				  beforeStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context,
				   rightStack);
      stackUsed =
	(unsigned short)rightStack + fLeft->GetType().GetWidth() + beforeStack;
      if (fLeft->GetType() == CJavaTypeSignature::kLong) {
	code.Append(CJavaCodeAttribute::lshr, fLineNumber);
      } else {
	code.Append(CJavaCodeAttribute::ishr, fLineNumber);
      }
    }
    if (error == 0) {
      error = fLeft->EndStoreCode(code, context,
				  afterStack, isStatement);
      stackUsed = ::max(stackUsed, (unsigned short)(afterStack +
                 fLeft->GetType().GetWidth() + fRight->GetType().GetWidth() +
		 beforeStack));
    }
    break;
  case AND_EQUALS:
    error = fLeft->StartStoreCode(code, context,
				  beforeStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context,
				   rightStack);
      stackUsed =
	(unsigned short)rightStack + fLeft->GetType().GetWidth() + beforeStack;
      if (fLeft->GetType() == CJavaTypeSignature::kLong) {
	code.Append(CJavaCodeAttribute::land, fLineNumber);
      } else {
	code.Append(CJavaCodeAttribute::iand, fLineNumber);
      }
    }
    if (error == 0) {
      error = fLeft->EndStoreCode(code, context,
				  afterStack, isStatement);
      stackUsed = ::max(stackUsed, (unsigned short)(afterStack +
                 fLeft->GetType().GetWidth() + fRight->GetType().GetWidth() +
		 beforeStack));
    }
    break;
  case XOR_EQUALS:
    error = fLeft->StartStoreCode(code, context,
				  beforeStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context,
				   rightStack);
      stackUsed =
	(unsigned short)rightStack + fLeft->GetType().GetWidth() + beforeStack;
      if (fLeft->GetType() == CJavaTypeSignature::kLong) {
	code.Append(CJavaCodeAttribute::lxor, fLineNumber);
      } else {
	code.Append(CJavaCodeAttribute::ixor, fLineNumber);
      }
    }
    if (error == 0) {
      error = fLeft->EndStoreCode(code, context,
				  afterStack, isStatement);
      stackUsed = ::max(stackUsed, (unsigned short)(afterStack +
                 fLeft->GetType().GetWidth() + fRight->GetType().GetWidth() +
		 beforeStack));
    }
    break;
  case OR_EQUALS:
    error = fLeft->StartStoreCode(code, context,
				  beforeStack);
    if (error == 0) {
      error = fRight->GenerateCode(code, context,
				   rightStack);
      stackUsed =
	(unsigned short)rightStack + fLeft->GetType().GetWidth() + beforeStack;
      if (fLeft->GetType() == CJavaTypeSignature::kLong) {
	code.Append(CJavaCodeAttribute::lor, fLineNumber);
      } else {
	code.Append(CJavaCodeAttribute::ior, fLineNumber);
      }
    }
    if (error == 0) {
      error = fLeft->EndStoreCode(code, context,
				  afterStack, isStatement);
      stackUsed = ::max(stackUsed, (unsigned short)(afterStack +
                 fLeft->GetType().GetWidth() + fRight->GetType().GetWidth() +
		 beforeStack));
    }
    break;
  default:
    assert(0);
  }
  return error;
}


//
//  Method name : AppendToStringBuffer
//  Description : This method is used to emit the code needed to append
//    an arbitrary type value, which is already on the stack, onto the end of
//    a java.lang.StringBuffer object.  The emitted code is appended
//    onto the code object.  The 'inClass' parameter is provided to
//    give enough context so that the appropriate constants can be added
//    to the constant pool of the class.
//
CCompileError*
CBinaryExpression::AppendToStringBuffer(CCodeSequence& code,
					CExpression* childExpression,
					CCompileContext& context,
					unsigned short& stackUsed)
{
  CCompileError* error = 0;
  assert(childExpression != 0);
  CBinaryExpression* binaryChild =
    DYNAMIC_CAST(CBinaryExpression, childExpression);
  if (binaryChild != 0 && binaryChild->fOperator == '+' &&
      binaryChild->GetType() == CJavaTypeSignature::kString) {
    unsigned short leftStack, rightStack;
    error = binaryChild->AppendToStringBuffer(code,
		binaryChild->fLeft, context, leftStack);
    if (error == 0) {
      error = binaryChild->AppendToStringBuffer(code,
		binaryChild->fRight, context, rightStack);
      stackUsed = ::max(leftStack, rightStack);
    }
  } else {
    error = childExpression->GenerateCode(code, context,
					  stackUsed);
    CJavaTypeSignature type = childExpression->GetType();
    CJavaTypeSignature stringBufferType(kStringBufferName);
    deque<CJavaTypeSignature> appendArguments;
    if (type.IsReference()) {
      if (type == CJavaTypeSignature::kString) {
	appendArguments.push_front(CJavaTypeSignature::kString);
      } else {
	CJavaTypeSignature charArrayType(CJavaTypeSignature::Character, 1);
	if (type == charArrayType) {
	  appendArguments.push_front(charArrayType);
	} else {
	  appendArguments.push_front(
			    CJavaTypeSignature(CCompiler::kObjectName));
	}
      }
    } else {
      switch (type.GetBaseType()) {
      case CJavaTypeSignature::Byte:
      case CJavaTypeSignature::Integer:
      case CJavaTypeSignature::Short:
	appendArguments.push_front(CJavaTypeSignature::kInteger);
	break;
      case CJavaTypeSignature::Character:
	appendArguments.push_front(CJavaTypeSignature::kCharacter);
	break;
      case CJavaTypeSignature::Double:
	appendArguments.push_front(CJavaTypeSignature::kDouble);
	break;
      case CJavaTypeSignature::Float:
	appendArguments.push_front(CJavaTypeSignature::kFloat);
	break;
      case CJavaTypeSignature::LongInteger:
	appendArguments.push_front(CJavaTypeSignature::kLong);
	break;
      case CJavaTypeSignature::Boolean:
	appendArguments.push_front(CJavaTypeSignature::kBoolean);
	break;
      default:
	assert(0);
      }
    }
    CJavaMethodSignature appendSignature(stringBufferType,
					 kAppendName,
					 appendArguments);
    JavaConstantIndex index = context.GetClass().AddMethodConstant(
				         kStringBufferName, appendSignature);
    code.Append(CJavaCodeAttribute::invokevirtual, index, fLineNumber);
  }
  return error;
}

//
//  Method name : CreateConstantExpression
//  Description : This method asks the expression if it can generate a single
//    constant-valued equivalent of itself.  This is needed in several
//    several places in Java, but it also provides a simple constant-folding
//    optimization.  If this operation can be performed, a new literal
//    expression is allocated and assigned to 'result,' otherwise it is set to
//    0, indicating no constant folding was possible.  In any case,
//    this expression is not modified or deleted during this check.
//    If any errors are found during compilation, they are returned to the
//    caller, otherwise 0 is returned.
//
CCompileError*
CBinaryExpression::CreateConstantExpression(CExpression*& result,
					    CCompileContext& context) const
{
  CCompileError* error = 0;
  result = 0;
  assert(fLeft != 0 && fRight != 0);
  bool bothLiterals = fLeft->IsLiteral() && fRight->IsLiteral();
  COrdinalLiteral* intLeft = DYNAMIC_CAST(COrdinalLiteral, fLeft);
  COrdinalLiteral* intRight = DYNAMIC_CAST(COrdinalLiteral, fRight);
  CFloatLiteral* floatLeft = DYNAMIC_CAST(CFloatLiteral, fLeft);
  CFloatLiteral* floatRight = DYNAMIC_CAST(CFloatLiteral, fRight);
  switch (fOperator) {
  case OR:
    if (intLeft != 0) {
      if (intLeft->GetBoolean()) {
	result = new COrdinalLiteral(true);
      } else if (intRight != 0) {
	result = new COrdinalLiteral(intRight->GetBoolean());
      }
    }
    break;
  case AND:
    if (intLeft != 0) {
      if (!intLeft->GetBoolean()) {
	result = new COrdinalLiteral(false);
      } else if (intRight != 0) {
	result = new COrdinalLiteral(intRight->GetBoolean());
      }
    }
    break;
  case BITSHIFT_RIGHT:
    if (bothLiterals) {
      CJavaTypeSignature leftType = fLeft->GetType();
      if (leftType == CJavaTypeSignature::kInteger) {
	result = new COrdinalLiteral((long)intLeft->GetInteger() >>
				     (intRight->GetInteger() & 0x1f));
      } else if (leftType != CJavaTypeSignature::kLong) {
	result = new COrdinalLiteral((long long)intLeft->GetLong() >>
				     (intRight->GetInteger() & 0x3f));
      }
    }
    break;
  case FILL_SHIFT_RIGHT:
    if (bothLiterals) {
      CJavaTypeSignature leftType = fLeft->GetType();
      if (leftType == CJavaTypeSignature::kInteger) {
	result = new COrdinalLiteral(intLeft->GetInteger() >>
				     (intRight->GetInteger() & 0x1f));
      } else if (leftType != CJavaTypeSignature::kLong) {
	result = new COrdinalLiteral(intLeft->GetLong() >>
				     (intRight->GetInteger() & 0x3f));
      }
    }
    break;
  case SHIFT_LEFT:
    if (bothLiterals) {
      CJavaTypeSignature leftType = fLeft->GetType();
      if (leftType == CJavaTypeSignature::kInteger) {
	result = new COrdinalLiteral(intLeft->GetInteger() <<
				     (intRight->GetInteger() & 0x1f));
      } else if (leftType != CJavaTypeSignature::kLong) {
	result = new COrdinalLiteral(intLeft->GetLong() <<
				     (intRight->GetInteger() & 0x3f));
      }
    }
    break;
  case '+':
    if (intLeft != 0 && intRight != 0) {
      switch (intLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Integer:
	result = new COrdinalLiteral((long)intLeft->GetInteger() +
				     (long)intRight->GetInteger());
	break;
      case CJavaTypeSignature::Byte:
	result = new COrdinalLiteral(
				     (unsigned char)((signed char)intLeft->GetByte() +
						     (signed char)intRight->GetByte()));
	break;
      case CJavaTypeSignature::Short:
	result = new COrdinalLiteral((unsigned short)(intLeft->GetShort() +
						      intRight->GetShort()));
	break;
      case CJavaTypeSignature::LongInteger:
	result = new COrdinalLiteral((long long)intLeft->GetLong() +
				     (long long)intRight->GetLong());
	break;
      }
    } else if (floatLeft != 0 && floatRight != 0) {
      switch (floatLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Float:
	result = new CFloatLiteral(floatLeft->GetFloat() +
				   floatRight->GetFloat());
	break;
      case CJavaTypeSignature::Double:
	result = new CFloatLiteral(floatLeft->GetDouble() +
				   floatRight->GetDouble());
	break;
      }
    } else if (bothLiterals) {
      CStringLiteral* stringLeft = DYNAMIC_CAST(CStringLiteral, fLeft);
      CStringLiteral* stringRight = DYNAMIC_CAST(CStringLiteral, fRight);
      if (stringLeft != 0 && stringRight != 0) {
	result = new CStringLiteral(stringLeft->GetString() +
				    stringRight->GetString());
      }
    }
    break;
  case '-':
    if (intLeft != 0 && intRight != 0) {
      switch (intLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Integer:
	result = new COrdinalLiteral((long)intLeft->GetInteger() -
				     (long)intRight->GetInteger());
	break;
      case CJavaTypeSignature::Byte:
	result = new COrdinalLiteral(
				     (unsigned char)((signed char)intLeft->GetByte() -
						     (signed char)intRight->GetByte()));
	break;
      case CJavaTypeSignature::Short:
	result = new COrdinalLiteral((unsigned short)(intLeft->GetShort() -
						      intRight->GetShort()));
	break;
      case CJavaTypeSignature::LongInteger:
	result = new COrdinalLiteral((long long)intLeft->GetLong() -
				     (long long)intRight->GetLong());
	break;
      }
    } else if (floatLeft != 0 && floatRight != 0) {
      switch (floatLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Float:
	result = new CFloatLiteral(floatLeft->GetFloat() -
				   floatRight->GetFloat());
	break;
      case CJavaTypeSignature::Double:
	result = new CFloatLiteral(floatLeft->GetDouble() -
				   floatRight->GetDouble());
	break;
      }
    }
    break;
  case '*':
    if (intLeft != 0 && intRight != 0) {
      switch (intLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Integer:
	result = new COrdinalLiteral((long)intLeft->GetInteger() *
				     (long)intRight->GetInteger());
	break;
      case CJavaTypeSignature::Byte:
	result = new COrdinalLiteral(
				     (unsigned char)((signed char)intLeft->GetByte() *
						     (signed char)intRight->GetByte()));
	break;
      case CJavaTypeSignature::Short:
	result = new COrdinalLiteral((unsigned short)(intLeft->GetShort() *
						      intRight->GetShort()));
	break;
      case CJavaTypeSignature::LongInteger:
	result = new COrdinalLiteral((long long)intLeft->GetLong() *
				     (long long)intRight->GetLong());
	break;
      }
    } else if (floatLeft != 0 && floatRight != 0) {
      switch (floatLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Float:
	result = new CFloatLiteral(floatLeft->GetFloat() *
				   floatRight->GetFloat());
	break;
      case CJavaTypeSignature::Double:
	result = new CFloatLiteral(floatLeft->GetDouble() *
				   floatRight->GetDouble());
	break;
      }
    }
    break;
  case '/':
    if (intLeft != 0 && intRight != 0) {
      if (intRight->GetLong() == 0) {
	error = MakeError("Division by zero");
      } else {
	switch (intLeft->GetType().GetBaseType()) {
	case CJavaTypeSignature::Integer:
	  result = new COrdinalLiteral((long)intLeft->GetInteger() /
				       (long)intRight->GetInteger());
	  break;
	case CJavaTypeSignature::Byte:
	  result = new COrdinalLiteral(
				       (unsigned char)((signed char)intLeft->GetByte() /
						       (signed char)intRight->GetByte()));
	  break;
	case CJavaTypeSignature::Short:
	  result = new COrdinalLiteral((unsigned short)(intLeft->GetShort() /
							intRight->GetShort()));
	  break;
	case CJavaTypeSignature::LongInteger:
	  result = new COrdinalLiteral((long long)intLeft->GetLong() /
				       (long long)intRight->GetLong());
	  break;
	}
      }
    } else if (floatLeft != 0 && floatRight != 0) {
      switch (floatLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Float:
	result = new CFloatLiteral(floatLeft->GetFloat() /
				   floatRight->GetFloat());
	break;
      case CJavaTypeSignature::Double:
	result = new CFloatLiteral(floatLeft->GetDouble() /
				   floatRight->GetDouble());
	break;
      }
    }
    break;
  case '%':
    if (intLeft != 0 && intRight != 0) {
      if (intRight->GetLong() == 0) {
	error = MakeError("Division by zero");
      } else {
	switch (intLeft->GetType().GetBaseType()) {
	case CJavaTypeSignature::Integer:
	  result = new COrdinalLiteral((long)intLeft->GetInteger() %
				       (long)intRight->GetInteger());
	  break;
	case CJavaTypeSignature::Byte:
	  result = new COrdinalLiteral(
				       (unsigned char)((signed char)intLeft->GetByte() %
						       (signed char)intRight->GetByte()));
	  break;
	case CJavaTypeSignature::Short:
	  result = new COrdinalLiteral((unsigned short)(intLeft->GetShort() %
							intRight->GetShort()));
	  break;
	case CJavaTypeSignature::LongInteger:
	  result = new COrdinalLiteral((long long)intLeft->GetLong() %
				       (long long)intRight->GetLong());
	  break;
	}
      }
    } else if (floatLeft != 0 && floatRight != 0) {
      switch (floatLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Float:
	result = new CFloatLiteral((float)::fmod(floatLeft->GetFloat(),
						 floatRight->GetFloat()));
	break;
      case CJavaTypeSignature::Double:
	result = new CFloatLiteral((double)::fmod(floatLeft->GetDouble(),
						  floatRight->GetDouble()));
	break;
      }
    }
    break;
  case '|':
    if (intLeft != 0 && intRight != 0) {
      switch (intLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Integer:
	result = new COrdinalLiteral(intLeft->GetInteger() |
				     intRight->GetInteger());
	break;
      case CJavaTypeSignature::Boolean:
	result = new COrdinalLiteral(intLeft->GetBoolean() ||
				     intRight->GetBoolean());
	break;
      case CJavaTypeSignature::Byte:
	result = new COrdinalLiteral((unsigned char)(intLeft->GetByte() |
						     intRight->GetByte()));
	break;
      case CJavaTypeSignature::Short:
	result = new COrdinalLiteral((unsigned short)(intLeft->GetShort() |
						      intRight->GetShort()));
	break;
      case CJavaTypeSignature::LongInteger:
	result = new COrdinalLiteral(intLeft->GetLong() |
				     intRight->GetLong());
	break;
      }
    }
    break;
  case '&':
    if (intLeft != 0 && intRight != 0) {
      switch (intLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Integer:
	result = new COrdinalLiteral(intLeft->GetInteger() &
				     intRight->GetInteger());
	break;
      case CJavaTypeSignature::Boolean:
	result = new COrdinalLiteral(intLeft->GetBoolean() &&
				     intRight->GetBoolean());
	break;
      case CJavaTypeSignature::Byte:
	result = new COrdinalLiteral((unsigned char)(intLeft->GetByte() &
						     intRight->GetByte()));
	break;
      case CJavaTypeSignature::Short:
	result = new COrdinalLiteral((unsigned short)(intLeft->GetShort() &
						      intRight->GetShort()));
	break;
      case CJavaTypeSignature::LongInteger:
	result = new COrdinalLiteral(intLeft->GetLong() &
				     intRight->GetLong());
	break;
      }
    }
    break;
  case '^':
    if (intLeft != 0 && intRight != 0) {
      switch (intLeft->GetType().GetBaseType()) {
      case CJavaTypeSignature::Integer:
	result = new COrdinalLiteral(intLeft->GetInteger() ^
				     intRight->GetInteger());
	break;
      case CJavaTypeSignature::Boolean:
	result = new COrdinalLiteral(
				     (intLeft->GetBoolean() || intRight->GetBoolean()) &&
				     !(intLeft->GetBoolean() && intRight->GetBoolean()));
	break;
      case CJavaTypeSignature::Byte:
	result = new COrdinalLiteral((unsigned char)(intLeft->GetByte() ^
						     intRight->GetByte()));
	break;
      case CJavaTypeSignature::Short:
	result = new COrdinalLiteral((unsigned short)(intLeft->GetShort() ^
						      intRight->GetShort()));
	break;
      case CJavaTypeSignature::LongInteger:
	result = new COrdinalLiteral(intLeft->GetLong() ^
				     intRight->GetLong());
	break;
      }
    }
    break;
  case LTEQ:
    if (intLeft != 0 && intRight != 0) {
      result = new COrdinalLiteral(intLeft->GetLong() <=
				   intRight->GetLong());
    } else if (floatLeft != 0 && floatRight != 0) {
      result = new COrdinalLiteral(floatLeft->GetDouble() <=
				   floatRight->GetDouble());
    }
    break;
  case GTEQ:
    if (intLeft != 0 && intRight != 0) {
      result = new COrdinalLiteral(intLeft->GetLong() >=
				   intRight->GetLong());
    } else if (floatLeft != 0 && floatRight != 0) {
      result = new COrdinalLiteral(floatLeft->GetDouble() >=
				   floatRight->GetDouble());
    }
    break;
  case '<':
    if (intLeft != 0 && intRight != 0) {
      result = new COrdinalLiteral(intLeft->GetLong() <
				   intRight->GetLong());
    } else if (floatLeft != 0 && floatRight != 0) {
      result = new COrdinalLiteral(floatLeft->GetDouble() <
				   floatRight->GetDouble());
    }
    break;
  case '>':
    if (intLeft != 0 && intRight != 0) {
      result = new COrdinalLiteral(intLeft->GetLong() >
				   intRight->GetLong());
    } else if (floatLeft != 0 && floatRight != 0) {
      result = new COrdinalLiteral(floatLeft->GetDouble() >
				   floatRight->GetDouble());
    }
    break;
  case EQUAL_COMPARE:
    if (intLeft != 0 && intRight != 0) {
      result = new COrdinalLiteral(intLeft->GetLong() ==
				   intRight->GetLong());
    } else if (floatLeft != 0 && floatRight != 0) {
      result = new COrdinalLiteral(floatLeft->GetDouble() ==
				   floatRight->GetDouble());
    }
    break;
  case NOT_EQUAL:
    if (intLeft != 0 && intRight != 0) {
      result = new COrdinalLiteral(intLeft->GetLong() !=
				   intRight->GetLong());
    } else if (floatLeft != 0 && floatRight != 0) {
      result = new COrdinalLiteral(floatLeft->GetDouble() !=
				   floatRight->GetDouble());
    }
    break;
  }
  return error;
}

//
//  Method name : ReduceInteger
//  Description : This method is used to emit the code needed to take an
//    integer value and reduce it down to some other integer variable (like
//    a short, char, byte)
//
void
CBinaryExpression::ReduceInteger(CCodeSequence& code,
				 CJavaTypeSignature::JavaType type)
{
  switch (type) {
  case CJavaTypeSignature::Short:
    code.Append(CJavaCodeAttribute::int2short, fLineNumber);
    break;
  case CJavaTypeSignature::Byte:
    code.Append(CJavaCodeAttribute::int2byte, fLineNumber);
    break;
  case CJavaTypeSignature::Character:
    code.Append(CJavaCodeAttribute::int2char, fLineNumber);
    break;
  }
}
