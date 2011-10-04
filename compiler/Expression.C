// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: Expression.C,v 1.25 1997/11/10 00:48:06 geppetto Exp $
#pragma implementation
#include "Expression.h"
#include "parser_decls.h"
#include "parser.h"  // for token values
#include "JavaFieldSignature.h"
#include "JavaClassFile.h"
#include "Compiler.h"
#include "CompileError.h"
#include "JavaMethodInfo.h"
#include "JavaCodeAttribute.h"
#include "CompileContext.h"
#include "CodeSequence.h"
#include <algorithm>

static const unicode_string kLengthName = ::StringToUnicode("length");


//=============================== CExpression ================================

//
//  Method name : CExpression
//  Description : Protected constructor.  Used by children to construct an
//    expression instance that knows its type and knows whether or not it is
//    an lvalue-type expression that can be assigned to.
//    The 'Void' type is used as a placeholder to say 'I don't know what type
//    this is yet.'  Because of use-before-declaration in Java, types can't
//    be determined in the first compilation pass, so I stick this type in
//    as a placeholder until the type-check pass.
//
//    You could argue that it's poor encapsulation to grab the line number
//    from the global yylineno variable like I'm doing here.  I'd probably
//    agree with you, but the alternative is to pass the line number into
//    the constructors of every subclass of CExpression and then pass it into
//    this constructor, which would really make for tedious code.
//    Maybe it will be replaced some day.
//    "Aqua sea foam shame." -- Nirvana, 'All Apologies'
//
CExpression::CExpression(const CJavaTypeSignature& type, bool isLValue)
  : fType(type), fLValue(isLValue), fLineNumber(yylineno)
{
}

//
//  Method name : CExpression
//  Description : Copy constructor.
//
CExpression::CExpression(const CExpression& source)
  : fType(source.fType),
    fLValue(source.fLValue),
    fLineNumber(source.fLineNumber)
{
}

//
//  Method name : ~CExpression
//  Description : Destructor
//
CExpression::~CExpression()
{
}

//
//  Method name : EvaluateType
//  Description : This is used during the second compilation pass to tell an
//    expression to determine its type (if it hasn't done so already) and
//    pass it back to the caller through the 'setType' parameter.  This
//    call is also used to simplify constant-valued expressions (e.g. 2 + 3),
//    so the a pointer to the relevant expression is passed to this call.  If
//    a constant-folded substitution needs to be made, the original referent
//    of the pointer will be deleted and a replacement will be provided.
//    The other parameters are just to give the context so it can search for
//    fields and classes, etc.  If successful, it returns 0, otherwise it
//    returns a new error object to the user.
//
CCompileError*
CExpression::EvaluateType(CExpression*& expressionPointer,
	  CCompileContext& context, CJavaTypeSignature& setType, bool asLValue)
{
  assert(expressionPointer != 0);
  if (!asLValue) {
    expressionPointer->fLValue = false;
  }
  CCompileError* error = 0;
  if (!context.IsReachable()) {
    error = expressionPointer->MakeError("Expression cannot be reached");
  } else if (expressionPointer->fType == CJavaTypeSignature::kVoid) {
    error = expressionPointer->HandleEvaluateType(context,
						  expressionPointer->fType);
  }
  unicode_string className;
  if (error == 0 && expressionPointer->fType != CJavaTypeSignature::kNullType
      && expressionPointer->fType.GetBaseClassName(className)) {
    const CJavaClassFile* classFile =
      context.GetCompiler().LookupClass(className, context);
    if (classFile == 0) {
      unicode_string errorString = ::StringToUnicode("Unable to find class ");
      errorString += className;
      error = expressionPointer->MakeError(errorString);
    } else if (!(classFile->GetClassName() == className)) {
      expressionPointer->fType.SetBaseClassName(classFile->GetClassName());
    }
  }
  if (error == 0) {
    if (!asLValue && !expressionPointer->IsLiteral()) {
      CExpression* temporaryExpression = 0;
      error = expressionPointer->CreateConstantExpression(temporaryExpression,
							  context);
      if (temporaryExpression != 0 && error == 0) {
	temporaryExpression->InheritLineNumber(*expressionPointer);
	delete expressionPointer;
	expressionPointer = temporaryExpression;
      }
    }
    setType = expressionPointer->fType;
  }
  return error;
}

//
//  Method name : HandleEvaluateType
//  Description : This is an overrideable hook that is used by EvaluateType to
//    polymorphically force an expression to calculate its type.   It behaves
//    like EvaluateType in terms of parameters and return values.
//
CCompileError*
CExpression::HandleEvaluateType(CCompileContext& context,
				CJavaTypeSignature& setType)
{
  // This implementation is provided for children that should always have
  // their type known at the time of the first pass (like literals) ... since
  // EvaluateType knows their type, this should never be called.
  assert(0);
  return 0;
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
CExpression::CreateConstantExpression(CExpression*& result,
				      CCompileContext& context) const
{
  result = 0;
  return 0;
}

//
//  Method name : StartStoreCode
//  Description : This method is used to generate the instructions needed at
//    the beginning of a store to a variable.  The storing code is split into
//    two parts this way because Java is a little inconsistent ... storing
//    into a variable puts code after the desired expression, but storing
//    into an array puts code before and after the value, so this has to
//    cover both cases.  Override as appropriate.
//
CCompileError*
CExpression::StartStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool evaluateToo)
{
  stackUsed = 0;
  return (fLValue ? 0 : MakeError("Invalid assignment to non-variable."));
}

//
//  Method name : EndStoreCode
//  Description : This method is used to generate the instructions needed at
//    the end of a store to a variable.  The storing code is split into
//    two parts this way because Java is a little inconsistent ... storing
//    into a variable puts code after the desired expression, but storing
//    into an array puts code before and after the value, so this has to
//    cover both cases.  Override as appropriate.
//
CCompileError*
CExpression::EndStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  stackUsed = 0;
  return 0;
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
CExpression::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(0);
  return 0;
}


//
//  Method name : MakeError
//  Description : This is a protected convenience function so the subclasses
//    of CExpression can create a compile error from a string.
//
CCompileError*
CExpression::MakeError(const string& message) const
{
  return new CCompileError(::StringToUnicode(message), fLineNumber);
}

//
//  Method name : MakeError
//  Description : This is a protected convenience function so the subclasses
//    of CExpression can create a compile error from a string.
//
CCompileError*
CExpression::MakeError(const unicode_string& message) const
{
  return new CCompileError(message, fLineNumber);
}

//
//  Method name : UnaryPromote
//  Description : This static member function can be used by children to try
//    to promote a pointer to a short or byte expression to int by applying a
//    cast operation to it.  See language spec section 3.5
//    Returns true if the expression has an integer type after the call.
//
bool
CExpression::UnaryPromote(CExpression*& promotePointer,
			  CCompileContext& context)
{
  assert(promotePointer != 0);
  bool result;
  switch (promotePointer->GetType().GetBaseType()) {
  case CJavaTypeSignature::Short:
  case CJavaTypeSignature::Byte:
  case CJavaTypeSignature::Character:   // for compatibility .. violates S3.5
    {
      CCompileError* error = CCastExpression::CastPointer(promotePointer,
				      CJavaTypeSignature::kInteger, context);
      assert(error == 0);
    }
  case CJavaTypeSignature::Integer:
    result = true;
    break;
  default:
    result = false;
  }
  return result;
}

//
//  Method name : BinaryPromote
//  Description : Takes two expression pointers and tries to promote them
//    to a matching numeric type through cast wrapper objects.  If the
//    two expressions are of matching types by the end of this call, true
//    is returned (even if it didn't need to do anything), otherwise false
//    is returned.  Read section 3.6 of the language spec for more information.
//
bool
CExpression::BinaryPromote(CExpression*& left, CExpression*& right,
			   CCompileContext& context)
{
  assert(left != 0 && right != 0);
  CCompileError* error = 0;
  CJavaTypeSignature leftType = left->GetType();
  CJavaTypeSignature rightType = right->GetType();
  bool result = false;
  if (leftType == CJavaTypeSignature::kDouble) {
    result = rightType == CJavaTypeSignature::kDouble;
    if (!result) {
      if (rightType.IsNumeric()) {
	error = CCastExpression::CastPointer(right, leftType, context);
	assert(error == 0);
	result = true;
      }
    }
  } else if (rightType == CJavaTypeSignature::kDouble) {
    if (leftType.IsNumeric()) {
      error = CCastExpression::CastPointer(left, rightType, context);
      assert(error == 0);
      result = true;
    }
  } else if (leftType == CJavaTypeSignature::kFloat) {
    result = rightType == CJavaTypeSignature::kFloat;
    if (!result) {
      if (rightType.IsNumeric()) {
	error = CCastExpression::CastPointer(right, leftType, context);
	assert(error == 0);
	result = true;
      }
    }
  } else if (rightType == CJavaTypeSignature::kFloat) {
    if (leftType.IsNumeric()) {
      error = CCastExpression::CastPointer(left, rightType, context);
      assert(error == 0);
      result = true;
    }
  } else if (leftType == CJavaTypeSignature::kLong) {
    result = rightType == CJavaTypeSignature::kLong;
    if (!result) {
      if (rightType.IsNumeric()) {
	error = CCastExpression::CastPointer(right, leftType, context);
	assert(error == 0);
	result = true;
      }
    }
  } else if (rightType == CJavaTypeSignature::kLong) {
    if (leftType.IsNumeric()) {
      error = CCastExpression::CastPointer(left, rightType, context);
      assert(error == 0);
      result = true;
    }
  } else {
    result = leftType.IsNumeric() && rightType.IsNumeric();
    if (result) {
      if (!(leftType == CJavaTypeSignature::kInteger)) {
	error = CCastExpression::CastPointer(left,
				 CJavaTypeSignature::kInteger, context);
      }
      if (error == 0 && !(rightType == CJavaTypeSignature::kInteger)) {
	error = CCastExpression::CastPointer(right,
			     CJavaTypeSignature::kInteger, context);
      }
    }
  }
  return result;
}

//
//  Method name : SetType
//  Description : Protected method to allow an expression to specify its
//    compile-time type.
//
void
CExpression::SetType(const CJavaTypeSignature& type)
{
  fType = type;
}

//
//  Method name : InheritLineNumber
//  Description : This method tells an expression that it should use the
//    source line number of another expression as its own.
//
void
CExpression::InheritLineNumber(const CExpression& from)
{
  fLineNumber = from.fLineNumber;
}

//
//  Method name : IsLiteral
//  Description : This method tells the caller whether this expression has a
//    constant value that could be used in a final interface method, a 'case'
//    label, etc.  This is the default implementation, which indicates that
//    the value is not constant.
//
bool
CExpression::IsLiteral() const
{
  return false;
}

//============================= COrdinalLiteral ===============================

//
//  Method name : COrdinalLiteral
//  Description : Constructs an integer literal value with the provided type
//    signature.
//
COrdinalLiteral::COrdinalLiteral(unsigned long integerLiteral,
				 const CJavaTypeSignature& type)
  : CExpression(type)
{
  fOrdinalLiteral = integerLiteral;
}

//
//  Method name : COrdinalLiteral
//  Description : Constructs an integer literal value.
//
COrdinalLiteral::COrdinalLiteral(unsigned long integerLiteral)
  : CExpression(CJavaTypeSignature::Integer)
{
  fOrdinalLiteral = integerLiteral;
}

//
//  Method name : COrdinalLiteral
//  Description : Constructs an integer literal value.
//
COrdinalLiteral::COrdinalLiteral(long integerLiteral)
  : CExpression(CJavaTypeSignature::Integer)
{
  fOrdinalLiteral = integerLiteral;
}

//
//  Method name : COrdinalLiteral
//  Description : Constructs a two-byte literal value.  If the 'isChar' flag
//    is 'true', then this is a character literal, otherwise it is a short
//    constant value.
//
COrdinalLiteral::COrdinalLiteral(unsigned short characterLiteral, bool isChar)
  : CExpression(isChar ? CJavaTypeSignature::Character :
		CJavaTypeSignature::Short)
{
  fOrdinalLiteral = characterLiteral;
}

//
//  Method name : COrdinalLiteral
//  Description : Constructs a signed, two-byte integer literal.
//
COrdinalLiteral::COrdinalLiteral(signed short shortLiteral)
  : CExpression(CJavaTypeSignature::Short)
{
  fOrdinalLiteral = shortLiteral;
}

//
//  Method name : COrdinalLiteral
//  Description : Constructs a constant byte value.
//
COrdinalLiteral::COrdinalLiteral(unsigned char integerLiteral)
  : CExpression(CJavaTypeSignature::Byte)
{
  fOrdinalLiteral = integerLiteral;
}

//
//  Method name : COrdinalLiteral
//  Description : Constructs a constant byte value.
//
COrdinalLiteral::COrdinalLiteral(signed char integerLiteral)
  : CExpression(CJavaTypeSignature::Byte)
{
  fOrdinalLiteral = integerLiteral;
}

//
//  Method name : COrdinalLiteral
//  Description : Constructs a boolean literal value.
//
COrdinalLiteral::COrdinalLiteral(bool boolLiteral)
  : CExpression(CJavaTypeSignature::Boolean)
{
  fOrdinalLiteral = (boolLiteral ? 1 : 0);
}

//
//  Method name : COrdinalLiteral
//  Description : Constructs a double-long integer literal value.
//
COrdinalLiteral::COrdinalLiteral(unsigned long long longLiteral)
  : CExpression(CJavaTypeSignature::LongInteger)
{
  fOrdinalLiteral = longLiteral;
}

//
//  Method name : COrdinalLiteral
//  Description : Constructs a double-long integer literal value.
//
COrdinalLiteral::COrdinalLiteral(long long longLiteral)
  : CExpression(CJavaTypeSignature::LongInteger)
{
  fOrdinalLiteral = longLiteral;
}

//
//  Method name : COrdinalLiteral
//  Description : Copy constructor.
//
COrdinalLiteral::COrdinalLiteral(const COrdinalLiteral& source)
  : CExpression(source), fOrdinalLiteral(source.fOrdinalLiteral)
{
}

//
//  Method name : COrdinalLiteral
//  Description : Destructor
//
COrdinalLiteral::~COrdinalLiteral()
{
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
COrdinalLiteral::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(!isStatement);
  if (GetType() == CJavaTypeSignature::kLong) {
    if (fOrdinalLiteral == 0) {
      code.Append(CJavaCodeAttribute::lconst_0, fLineNumber);
    } else if (fOrdinalLiteral == 1) {
      code.Append(CJavaCodeAttribute::lconst_1, fLineNumber);
    } else {
      JavaConstantIndex index = AddToConstantPool(context.GetClass());
      code.Append(CJavaCodeAttribute::ldc2_w, index, fLineNumber);
    }
    stackUsed = 2;
  } else {
    CCodeSequence::Instruction instruction;
    long realLong = fOrdinalLiteral;
    if (realLong >= SHRT_MIN && realLong <= SHRT_MAX) {
      instruction.fOpcode = CJavaCodeAttribute::sipush;
      instruction.fArguments.s2 = realLong;
      code.Append(instruction, fLineNumber);
    } else {
      JavaConstantIndex index = AddToConstantPool(context.GetClass());
      code.Append(CJavaCodeAttribute::ldc, index, fLineNumber);
    }
    stackUsed = 1;
  }
  return 0;
}

//
//  Method name : AddToConstantPool
//  Description : This method is used to add this value into the constant
//    pool of the provided class.  It returns the pool index that was used.
//
JavaConstantIndex
COrdinalLiteral::AddToConstantPool(CJavaClassFile& inClass) const
{
  JavaConstantIndex index;
  if (GetType() == CJavaTypeSignature::kLong) {
    index = inClass.AddLongConstant(fOrdinalLiteral);
  } else {
    index = inClass.AddIntegerConstant(fOrdinalLiteral);
  }
  return index;
}

//
//  Method name : IsLiteral
//  Description : This method tells the caller whether this expression has a
//    constant value that could be used in a final interface method, a 'case'
//    label, etc.
//
bool
COrdinalLiteral::IsLiteral() const
{
  return true;
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
COrdinalLiteral::CreateConstantExpression(CExpression*& result,
					  CCompileContext& context) const
{
  result = Clone();
  return 0;
}

//============================= CFloatLiteral ===============================

//
//  Method name : CFloatLiteral
//  Description : Creates a 32 bit floating-point literal.
//
CFloatLiteral::CFloatLiteral(float floatLiteral)
  : CExpression(CJavaTypeSignature::Float), fValue(floatLiteral)
{
}

//
//  Method name : CFloatLiteral
//  Description : Creates a 64 bit floating-point literal.
//
CFloatLiteral::CFloatLiteral(double doubleLiteral)
  : CExpression(CJavaTypeSignature::Double), fValue(doubleLiteral)
{
}

//
//  Method name : CFloatLiteral
//  Description : Copy constructor.
//
CFloatLiteral::CFloatLiteral(const CFloatLiteral& source)
  : CExpression(source), fValue(source.fValue)
{
}

//
//  Method name : ~CFloatLiteral
//  Description : Destructor
//
CFloatLiteral::~CFloatLiteral()
{
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
CFloatLiteral::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(!isStatement);
  if (GetType() == CJavaTypeSignature::kDouble) {
    if (fValue == 0.0) {
      code.Append(CJavaCodeAttribute::dconst_0, fLineNumber);
    } else if (fValue == 1.0) {
      code.Append(CJavaCodeAttribute::dconst_1, fLineNumber);
    } else {
      JavaConstantIndex index = AddToConstantPool(context.GetClass());
      code.Append(CJavaCodeAttribute::ldc2_w, index, fLineNumber);
    }
    stackUsed = 2;
  } else {
    assert(GetType() == CJavaTypeSignature::kFloat);
    if (fValue == 0.0) {
      code.Append(CJavaCodeAttribute::fconst_0, fLineNumber);
    } else if (fValue == 1.0) {
      code.Append(CJavaCodeAttribute::fconst_1, fLineNumber);
    } else if (fValue == 2.0) {
      code.Append(CJavaCodeAttribute::fconst_2, fLineNumber);
    } else {
      JavaConstantIndex index = AddToConstantPool(context.GetClass());
      code.Append(CJavaCodeAttribute::ldc, index, fLineNumber);
    }
    stackUsed = 1;
  }
  return 0;
}

//
//  Method name : AddToConstantPool
//  Description : This method is used to add this value into the constant
//    pool of the provided class.  It returns the pool index that was used.
//
JavaConstantIndex
CFloatLiteral::AddToConstantPool(CJavaClassFile& inClass) const
{
  JavaConstantIndex index;
  if (GetType() == CJavaTypeSignature::kDouble) {
    index = inClass.AddDoubleConstant(fValue);
  } else {
    index = inClass.AddFloatConstant(fValue);
  }
  return index;
}

//
//  Method name : IsLiteral
//  Description : This method tells the caller whether this expression has a
//    constant value that could be used in a final interface method, a 'case'
//    label, etc.
//
bool
CFloatLiteral::IsLiteral() const
{
  return true;
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
CFloatLiteral::CreateConstantExpression(CExpression*& result,
					CCompileContext& context) const
{
  result = Clone();
  return 0;
}

//============================= CStringLiteral ===============================

//
//  Method name : CStringLiteral
//  Description : Constructs a string literal value out of a provided string.
//
CStringLiteral::CStringLiteral(const unicode_string& value)
  : CExpression(CJavaTypeSignature::kString.Disassemble()), fValue(value)
{
}

//
//  Method name : CStringLiteral
//  Description : Copy constructor.
//
CStringLiteral::CStringLiteral(const CStringLiteral& source)
  : CExpression(source), fValue(source.fValue)
{
}

//
//  Method name : ~CStringLiteral
//  Description : Destructor
//
CStringLiteral::~CStringLiteral()
{
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
CStringLiteral::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(!isStatement);
  JavaConstantIndex index = AddToConstantPool(context.GetClass());
  code.Append(CJavaCodeAttribute::ldc, index, fLineNumber);
  stackUsed = 1;
  return 0;
}

//
//  Method name : AddToConstantPool
//  Description : This method is used to add this value into the constant
//    pool of the provided class.  It returns the pool index that was used.
//
JavaConstantIndex
CStringLiteral::AddToConstantPool(CJavaClassFile& inClass) const
{
  JavaConstantIndex index = inClass.AddAscizConstant(fValue);
  return inClass.AddStringConstant(index);
}

//
//  Method name : IsLiteral
//  Description : This method tells the caller whether this expression has a
//    constant value that could be used in a final interface method, a 'case'
//    label, etc.
//
bool
CStringLiteral::IsLiteral() const
{
  return true;
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
CStringLiteral::CreateConstantExpression(CExpression*& result,
					CCompileContext& context) const
{
  result = Clone();
  return 0;
}

//============================== CNull =================================

//
//  Method name : CNull
//  Description : Constructs a null literal value.
//
CNull::CNull()
  : CExpression(CJavaTypeSignature::kNullType)
{
}

//
//  Method name : ~CNull
//  Description : Destructor
//
CNull::~CNull()
{
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
CNull::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(!isStatement);
  code.Append(CJavaCodeAttribute::aconst_null, fLineNumber);
  stackUsed = 1;
  return 0;
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
CNull::CreateConstantExpression(CExpression*& result,
				CCompileContext& context) const
{
  result = Clone();
  return 0;
}

//============================= CCastExpression ===============================

//
//  Method name : CCastExpression
//  Description : Constructs an expression casting a subexpression to another
//    type.
//
CCastExpression::CCastExpression(const CJavaTypeSignature& toType,
				 CExpression* adoptCastee)
  : CExpression(CJavaTypeSignature::kVoid),
    fCastToType(toType),
    fCastee(adoptCastee)
{
}

//
//  Method name : CCastExpression
//  Description : Copy constructor.
//
CCastExpression::CCastExpression(const CCastExpression& source)
  : CExpression(source),
    fCastToType(source.fCastToType),
    fCastee(0)
{
  if (source.fCastee != 0) {
    fCastee = source.fCastee->Clone();
  }
}

//
//  Method name : ~CCastExpression
//  Description : Destructor
//
CCastExpression::~CCastExpression()
{
  delete fCastee;
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
CCastExpression::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(fCastee != 0 && !isStatement);
  CJavaTypeSignature type = fCastee->GetType();
  CCompileError* error =
    fCastee->GenerateCode(code, context, stackUsed);
  if (error == 0 &&
      !context.GetCompiler().SameType(type, GetType()) &&
      !context.GetCompiler().AssignableSubtype(type, GetType())) {
    if (type.IsReference() && GetType().IsReference()) {
      if (!(type == CJavaTypeSignature::kNullType)) {
	unicode_string className = 
	  context.GetCompiler().NameClassConstant(GetType());
	JavaConstantIndex index =
	  context.GetClass().AddClassConstant(className);
	code.Append(CJavaCodeAttribute::checkcast, index, fLineNumber);
      }
    } else if (type.IsNumeric() && GetType().IsNumeric()) {
      switch (GetType().GetBaseType()) {
      case CJavaTypeSignature::Byte:
      case CJavaTypeSignature::Character:
      case CJavaTypeSignature::Short:
      case CJavaTypeSignature::Integer:
	if (type == CJavaTypeSignature::kLong) {
	  code.Append(CJavaCodeAttribute::l2i, fLineNumber);
	} else if (type == CJavaTypeSignature::kFloat) {
	  code.Append(CJavaCodeAttribute::f2i, fLineNumber);
	} else if (type == CJavaTypeSignature::kDouble) {
	  code.Append(CJavaCodeAttribute::d2i, fLineNumber);
	}
	switch(GetType().GetBaseType()) {
	case CJavaTypeSignature::Byte:
	  code.Append(CJavaCodeAttribute::int2byte, fLineNumber);
	  break;
	case CJavaTypeSignature::Character:
	  code.Append(CJavaCodeAttribute::int2char, fLineNumber);
	  break;
	case CJavaTypeSignature::Short:
	  code.Append(CJavaCodeAttribute::int2short, fLineNumber);
	  break;
	}
	break;
      case CJavaTypeSignature::LongInteger:
	if (type.IsOrdinal()) {
	  code.Append(CJavaCodeAttribute::i2l, fLineNumber);
	} else if (type == CJavaTypeSignature::kFloat) {
	  code.Append(CJavaCodeAttribute::f2l, fLineNumber);
	} else {
	  assert(type == CJavaTypeSignature::kDouble);
	  code.Append(CJavaCodeAttribute::d2l, fLineNumber);
	}
	stackUsed = ::max((unsigned short)2, stackUsed);
	break;
      case CJavaTypeSignature::Float:
	if (type == CJavaTypeSignature::kLong) {
	  code.Append(CJavaCodeAttribute::l2f, fLineNumber);
	} else if (type.IsOrdinal()) {
	  code.Append(CJavaCodeAttribute::i2f, fLineNumber);
	} else {
	  assert(type == CJavaTypeSignature::kDouble);
	  code.Append(CJavaCodeAttribute::d2f, fLineNumber);
	}
	break;
      case CJavaTypeSignature::Double:
	if (type == CJavaTypeSignature::kLong) {
	  code.Append(CJavaCodeAttribute::l2d, fLineNumber);
	} else if (type.IsOrdinal()) {
	  code.Append(CJavaCodeAttribute::i2d, fLineNumber);
	} else {
	  assert(type == CJavaTypeSignature::kFloat);
	  code.Append(CJavaCodeAttribute::f2d, fLineNumber);
	}
	stackUsed = ::max((unsigned short)2, stackUsed);
	break;
      default:
	assert(0);
	break;
      }
    } else {
      assert(0);
    }
  }
  return error;
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
CCastExpression::HandleEvaluateType(CCompileContext& context,
				    CJavaTypeSignature& setType)
{
  assert(fCastee != 0);
  setType = context.GetCompiler().FixType(fCastToType);
  fCastToType = setType;
  CJavaTypeSignature type;
  CCompileError* error = EvaluateType(fCastee, context, type);
  if (error == 0 &&
      !context.GetCompiler().SameType(type, fCastToType) &&
      !context.GetCompiler().AssignableSubtype(type, fCastToType)) {
    if (type.IsReference() && fCastToType.IsReference()) {
      if (!context.GetCompiler().CastableType(type, fCastToType)) {
	error = MakeBadCastMessage(type);
      }
    } else if (!type.IsNumeric() || !fCastToType.IsNumeric()) {
      error = MakeBadCastMessage(type);
    }
  }      
  return error;
}

//
//  Method name : MakeBadCastMessage
//  Description : An internal method used by CCastExpression to generate a
//    compile error for an invalid conversion from 'castFrom' type to this
//    type.
//
CCompileError*
CCastExpression::MakeBadCastMessage(const CJavaTypeSignature& castFrom) const
{
  unicode_string errorString = ::StringToUnicode("Cannot convert ");
  errorString += castFrom.Disassemble();
  errorString += ::StringToUnicode(" value to ");
  errorString += fCastToType.Disassemble();
  return new CCompileError(errorString, fLineNumber);
}

//
//  Method name : CastPointer
//  Description : This static method is provided so that intermediate stages
//    of compilation can change the type of an expression pointer that they
//    are holding by casting it to a different value.  This performs the
//    cast, updates the new expression, and then checks the validity of the
//    resulting typecast.  If there is a problem, it returns an error,
//    otherwise it returns 0.
//
CCompileError*
CCastExpression::CastPointer(CExpression*& expressionPointer,
			     const CJavaTypeSignature& toType,
			     CCompileContext& context)
{
  assert(expressionPointer != 0);
  CExpression* newExpression = new CCastExpression(toType, expressionPointer);
  newExpression->InheritLineNumber(*expressionPointer);
  expressionPointer = newExpression;
  CJavaTypeSignature temporaryType;
  return EvaluateType(expressionPointer, context, temporaryType);
}

//
//  Method name : ImplicitCastPointer
//  Description : This static function attempts to convert the provided
//    expression into the provided type via an implicit type cast. If an
//    implicit cast isn't possible between these types, an error is returned,
//    otherwise null is returned.
//
CCompileError*
CCastExpression::ImplicitCastPointer(CExpression*& expressionPointer,
				     const CJavaTypeSignature& toType,
				     CCompileContext& context)
{
  CCompileError* error = 0;
  CJavaTypeSignature castFrom = expressionPointer->GetType();
  if (context.GetCompiler().ImplicitCastTo(castFrom, toType)) {
    error = CastPointer(expressionPointer, toType, context);
  } else {
    COrdinalLiteral* literal =
      DYNAMIC_CAST(COrdinalLiteral, expressionPointer);
    if (literal != 0 &&
	(toType == CJavaTypeSignature::kShort ||
	 toType == CJavaTypeSignature::kCharacter ||
	 toType == CJavaTypeSignature::kByte)) {
      unsigned long long value = literal->GetLong();
      bool converted = false;
      switch (toType.GetBaseType()) {
      case CJavaTypeSignature::Short:
      case CJavaTypeSignature::Character:
	{
	  short shortValue = (short)value;
	  if (shortValue == value) {
	    delete expressionPointer;
	    expressionPointer = new COrdinalLiteral(shortValue,
				    toType == CJavaTypeSignature::kCharacter);
	    converted = true;
	  }
	}
	break;
      case CJavaTypeSignature::Byte:
	{
	  signed char byteValue = (signed char)value;
	  if (byteValue == value) {
	    delete expressionPointer;
	    expressionPointer = new COrdinalLiteral(byteValue);
	    converted = true;
	  }	
	}  
	break;
      default:
	assert(0);
      }
      if (!converted) {
	unicode_string errorString =
	  ::StringToUnicode("Cannot convert constant integer value to ");
	errorString += toType.Disassemble();
	errorString += ::StringToUnicode(" without an explicit cast.");
	error =
	  new CCompileError(errorString, expressionPointer->GetLineNumber());
      }
    } else {
      unicode_string errorString = ::StringToUnicode("Cannot convert ");
      errorString += castFrom.Disassemble();
      errorString += ::StringToUnicode(" value to ");
      errorString += toType.Disassemble();
      errorString += ::StringToUnicode(" without an explicit cast.");
      error =
	new CCompileError(errorString, expressionPointer->GetLineNumber());
    }
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
CCastExpression::CreateConstantExpression(CExpression*& result,
					  CCompileContext& context) const
{
  assert(fCastee != 0);
  if (fCastee->IsLiteral()) {
    if (fCastToType.IsNumeric()) {
      COrdinalLiteral* intLiteral = DYNAMIC_CAST(COrdinalLiteral, fCastee);
      if (intLiteral != 0) {
	switch(fCastToType.GetBaseType()) {
	case CJavaTypeSignature::Integer:
	  result = new COrdinalLiteral((long)intLiteral->GetInteger());
	  break;
	case CJavaTypeSignature::LongInteger:
	  result = new COrdinalLiteral((long long)intLiteral->GetLong());
	  break;
	case CJavaTypeSignature::Byte:
	  result = new COrdinalLiteral((signed char)intLiteral->GetByte());
	  break;
	case CJavaTypeSignature::Short:
	  result = new COrdinalLiteral((signed short)intLiteral->GetShort());
	  break;
	case CJavaTypeSignature::Character:
	  result = new COrdinalLiteral((signed short)intLiteral->GetShort(),
				       true);
	  break;
	case CJavaTypeSignature::Float:
	  result = new CFloatLiteral((float)(long long)intLiteral->GetLong());
	  break;
	case CJavaTypeSignature::Double:
	   result =
	     new CFloatLiteral((double)(long long)intLiteral->GetLong());
	  break;
	default:
	  assert(0);
	}
      } else {
	CFloatLiteral* floatLiteral = DYNAMIC_CAST(CFloatLiteral, fCastee);
	if (floatLiteral != 0) {
	  switch(fCastToType.GetBaseType()) {
	  case CJavaTypeSignature::Integer:
	    result =
	      new COrdinalLiteral((unsigned long)floatLiteral->GetDouble());
	    break;
	  case CJavaTypeSignature::LongInteger:
	    result = new COrdinalLiteral(
			   (unsigned long long)floatLiteral->GetDouble());
	    break;
	  case CJavaTypeSignature::Byte:
	    result =
	      new COrdinalLiteral((unsigned char)floatLiteral->GetDouble());
	    break;
	  case CJavaTypeSignature::Short:
	    result =
	      new COrdinalLiteral((unsigned short)floatLiteral->GetDouble(),
				  false);
	    break;
	  case CJavaTypeSignature::Character:
	    result =
	      new COrdinalLiteral((unsigned short)floatLiteral->GetDouble());
	    break;
	  case CJavaTypeSignature::Float:
	    result = new CFloatLiteral(floatLiteral->GetFloat());
	    break;
	  case CJavaTypeSignature::Double:
	    result = new CFloatLiteral(floatLiteral->GetDouble());
	    break;
	  default:
	    assert(0);
	  }
	}
      }
    }
  }
  return 0;
}

//============================= CNewArray ================================

//
//  Method name : CNewArray
//  Description : Constructs an allocation expression for an array of objects.
//
CNewArray::CNewArray(const CJavaTypeSignature& type,
		     ExpressionList* adoptArraySizes)
  : CExpression(type),
    fArraySizes(adoptArraySizes)
{
}

//
//  Method name : CNewArray
//  Description : Copy constructor.
//
CNewArray::CNewArray(const CNewArray& source)
  : CExpression(source),
    fArraySizes(0)
{
  if (source.fArraySizes != 0) {
    fArraySizes = new ExpressionList;
    for (ExpressionList::iterator i = source.fArraySizes->begin();
	 !(i == source.fArraySizes->end()); ++i) {
      fArraySizes->push_back((*i)->Clone());
    }
  }
}

//
//  Method name : ~CNewArray
//  Description : Destructor.
//
CNewArray::~CNewArray()
{
  for (ExpressionList::iterator i = fArraySizes->begin();
       !(i == fArraySizes->end()); ++i) {
    delete *i;
  }
  delete fArraySizes;
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
CNewArray::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  CCompileError* error = 0;
  assert(fArraySizes != 0 && GetType().GetArrayBounds() > 0);
  stackUsed = 0;
  for (ExpressionList::iterator sizeIterator = fArraySizes->begin();
       error == 0 && !(sizeIterator == fArraySizes->end()); ++sizeIterator) {
    CJavaTypeSignature arraySizeType;
    assert(*sizeIterator != 0);
    error = EvaluateType((*sizeIterator), context, arraySizeType);
    if (error == 0) {
      if (!UnaryPromote(*sizeIterator, context)) {
	error = MakeError("Cannot convert array size to integer type.");
      } else {
	unsigned short childStackUsed;
	error =
	  (*sizeIterator)->GenerateCode(code, context, childStackUsed);
	stackUsed += childStackUsed;
      }
    }
  }

  if (error == 0) {
    CJavaTypeSignature arrayElementType = GetType();
    if (GetType().GetArrayBounds() > 1) {
      unicode_string className =
	context.GetCompiler().NameClassConstant(arrayElementType);
      CCodeSequence::Instruction instruction;
      instruction.fOpcode = CJavaCodeAttribute::multianewarray;
      instruction.fArguments.fIndexPair.index =
	context.GetClass().AddClassConstant(className);
      instruction.fArguments.fIndexPair.u1 = fArraySizes->size();
      code.Append(instruction, fLineNumber);
    } else {
      if (arrayElementType.GetBaseType() == CJavaTypeSignature::Class) {
	arrayElementType.SetArrayBounds(0);
	unicode_string className =
	  context.GetCompiler().NameClassConstant(arrayElementType);
	JavaConstantIndex index =
	  context.GetClass().AddClassConstant(className);
	code.Append(CJavaCodeAttribute::anewarray, index, fLineNumber);
      } else {
	switch (arrayElementType.GetBaseType()) {
	case CJavaTypeSignature::Character:
	  code.Append(CJavaCodeAttribute::newarray, 5, fLineNumber);
	  break;
	case CJavaTypeSignature::Float:
	  code.Append(CJavaCodeAttribute::newarray, 6, fLineNumber);
	  break;
	case CJavaTypeSignature::Double:
	  code.Append(CJavaCodeAttribute::newarray, 7, fLineNumber);
	  break;
	case CJavaTypeSignature::Boolean:
	case CJavaTypeSignature::Byte:
	  code.Append(CJavaCodeAttribute::newarray, 8, fLineNumber);
	  break;
	case CJavaTypeSignature::Short:
	  code.Append(CJavaCodeAttribute::newarray, 9, fLineNumber);
	  break;
	case CJavaTypeSignature::Integer:
	  code.Append(CJavaCodeAttribute::newarray, 10, fLineNumber);
	  break;
	case CJavaTypeSignature::LongInteger:
	  code.Append(CJavaCodeAttribute::newarray, 11, fLineNumber);
	  break;
	default:
	  assert(0);
	}
      }
    }
    if (isStatement) {
      code.Append(CJavaCodeAttribute::pop, fLineNumber);
    }
  }
  return error;
}


//============================= CNewObject ================================

//
//  Method name : CNewObject
//  Description : Constructs an allocation expression to allocate a class
//    value.  The first parameter to this function is the name of the class
//    to allocate and the second is a list of arguments to be given to the
//    class constructor at creation time.
//
CNewObject::CNewObject(const unicode_string& className,
		       ExpressionList* adoptArgs, CExpression* explicitThis)
  : CExpression(className),
    fConstructorArguments(adoptArgs),
    fExplicitThis(explicitThis)
{
  if (fConstructorArguments == 0) {
    fConstructorArguments = new ExpressionList;
  }
}

//
//  Method name : CNewObject
//  Description : Copy constructor.
//
CNewObject::CNewObject(const CNewObject& source)
  : CExpression(source),
    fConstructorArguments(0),
    fExplicitThis(0)
{
  if (source.fConstructorArguments != 0) {
    fConstructorArguments = new ExpressionList;
    for (ExpressionList::iterator i = source.fConstructorArguments->begin();
	 !(i == source.fConstructorArguments->end()); ++i) {
      fConstructorArguments->push_back((*i)->Clone());
    }
  }
  if (source.fExplicitThis != 0) {
    fExplicitThis = source.fExplicitThis->Clone();
  }
}

//
//  Method name : ~CNewObject
//  Description : Destructor.
//
CNewObject::~CNewObject()
{
  delete fConstructorArguments;
  delete fExplicitThis;
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
CNewObject::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  CCompileError* error = 0;
  unicode_string className;
  bool isClass = GetType().GetBaseClassName(className);
  assert(isClass && GetType().GetArrayBounds() == 0);
  const CJavaClassFile* javaClass =
    context.GetCompiler().LookupClass(className, context);
  if (javaClass == 0) {
    string errorString("Cannot create an object of unknown type ");
    errorString += ::UnicodeToString(className);
    error = MakeError(errorString);
  } else if (javaClass->GetAccessFlags().fAbstract) {
    string errorString("Cannot instantiate abstract class ");
    errorString += ::UnicodeToString(javaClass->GetClassName());
    error = MakeError(errorString);
  } else if (javaClass->GetAccessFlags().fInterface) {
    string errorString("Cannot instantiate interface ");
    errorString += ::UnicodeToString(javaClass->GetClassName());
    error = MakeError(errorString);
  }

  deque<CJavaTypeSignature> argumentTypes;
  ExpressionList::const_iterator originalArgumentsBegin =
    fConstructorArguments->begin();
  ExpressionList::const_iterator originalArgumentsEnd =
    fConstructorArguments->end();
  if (error == 0) {
    code.Append(CJavaCodeAttribute::op_new,
		context.GetClass().AddClassConstant(className), fLineNumber);
    if (!isStatement) {
      code.Append(CJavaCodeAttribute::dup, fLineNumber);
    }
    stackUsed = 2;
    if (javaClass->IsInner()) {
      if (fExplicitThis != 0) {
	fConstructorArguments->push_front(fExplicitThis);
      } else if (context.GetMethod().GetModifiers().fStatic == 0) {
	fConstructorArguments->push_front(
			new CSpecialExpression(CSpecialExpression::kThis));
      } else {
	string errorString("Cannot construct instance of inner class ");
	errorString += ::UnicodeToString(javaClass->GetClassName());
	errorString += " without enclosing instance or explicit 'new'.";
	error = MakeError(errorString);
      }
      if (error == 0) {
	context.GetCompiler().GenerateSyntheticConstructorArguments(className,
						    *fConstructorArguments);
      }
    }
    for (ExpressionList::iterator i = fConstructorArguments->begin();
	 error == 0 && i != fConstructorArguments->end(); ++i) {
      CJavaTypeSignature argumentType;
      error = EvaluateType(*i, context, argumentType);
      argumentTypes.push_back(argumentType);
    }
  }
  if (error == 0) {
    string* matchErrorString = 0;
    pair<const CJavaClassFile*, const CJavaMethodInfo*>* match =
      context.GetCompiler().MatchConstructor(*javaClass, argumentTypes,
			     originalArgumentsBegin, originalArgumentsEnd,
			     matchErrorString);
    if (match == 0 || match->first != javaClass) {
      if (matchErrorString == 0) {
	string errorString;
	errorString = "No matching constructor for class ";
	errorString += ::UnicodeToString(className);
	errorString += '(';
	deque<CJavaTypeSignature>::iterator arg = argumentTypes.begin();
	while (arg != argumentTypes.end()) {
	  errorString += ::UnicodeToString((*arg).Disassemble());
	  if (++arg != argumentTypes.end()) {
	    errorString += ", ";
	  }
	}
	errorString += ')';
	error = MakeError(errorString);
      } else {
	error = MakeError(*matchErrorString);
	delete matchErrorString;
      }
    } else {
      const CJavaMethodInfo* constructorInfo = match->second;
      context.GetCompiler().WarnDeprecatedMethod(*constructorInfo,
		*match->first, context, GetLineNumber());
      if (constructorInfo->ThrowsExceptions()) {
	for (deque<unicode_string>::const_iterator i =
	       constructorInfo->ThrowsBegin();
	     error == 0 && !(i == constructorInfo->ThrowsEnd()); ++i) {
	  if (!context.Throwable(CJavaTypeSignature(*i))) {
	    unicode_string message = *i;
	    message += ::UTFToUnicode(" must be caught or declared in this "
				      "method's 'throws' list.");
	    error = MakeError(message);
	  }
	}
      }
      assert(constructorInfo->GetSignature().ParameterCount() ==
	     fConstructorArguments->size());
      deque<CJavaTypeSignature>::const_iterator parameter =
	constructorInfo->GetSignature().ParametersBegin();
      unsigned short childStackSize, maxStackSize = 0;
      for (ExpressionList::iterator i = fConstructorArguments->begin();
	   error == 0 && i != fConstructorArguments->end(); ++i) {
	if (!context.GetCompiler().SameType((*i)->GetType(), *parameter)) {
	  error = CCastExpression::CastPointer(*i, *parameter, context);
	}
	if (error == 0) {
	  error = (*i)->GenerateCode(code, context, childStackSize);
	}
	maxStackSize =
	  ::max(maxStackSize, (unsigned short)(childStackSize + stackUsed));
	stackUsed += (*i)->GetType().GetWidth();
	++parameter;
      }
      stackUsed = ::max(stackUsed, maxStackSize);
      JavaConstantIndex index =	context.GetClass().AddMethodConstant(
				     className, match->second->GetSignature());
      code.Append(CJavaCodeAttribute::invokenonvirtual, index, fLineNumber);
    }
    delete match;
  }
  return error;
}

//=========================== CTrinaryExpression ==============================

//
//  Method name : CTrinaryExpression
//  Description : Constructs a Trinary expression from the three child
//    expressions that make it up.
//
CTrinaryExpression::CTrinaryExpression(CExpression* leftExpression,
				       CExpression* middleExpression,
				       CExpression* rightExpression)
  : CExpression(CJavaTypeSignature::Void, false),
    fLeft(leftExpression),
    fMiddle(middleExpression),
    fRight(rightExpression)
{
}

//
//  Method name : CTrinaryExpression
//  Description : Copy constructor.
//
CTrinaryExpression::CTrinaryExpression(const CTrinaryExpression& source)
  : CExpression(source), fLeft(0), fMiddle(0), fRight(0)
{
  if (source.fLeft != 0) {
    fLeft = source.fLeft->Clone();
  }
  if (source.fMiddle != 0) {
    fMiddle = source.fMiddle->Clone();
  }
  if (source.fRight != 0) {
    fRight = source.fRight->Clone();
  }
}

//
//  Method name : ~CTrinaryExpression
//  Description : Destructor.
//
CTrinaryExpression::~CTrinaryExpression()
{
  delete fLeft;
  delete fMiddle;
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
CTrinaryExpression::HandleEvaluateType(CCompileContext& context,
				       CJavaTypeSignature& setType)
{
  assert(fLeft != 0 && fMiddle != 0 && fRight != 0);
  CJavaTypeSignature leftType, middleType, rightType;
  CCompileError* error = EvaluateType(fLeft, context, leftType);
  if (error == 0) {
    error = EvaluateType(fMiddle, context, middleType);
  }
  if (error == 0) {
    error = EvaluateType(fRight, context, rightType);
  }
  if (error == 0 && leftType != CJavaTypeSignature::kBoolean) {
    error = MakeError("Non-boolean expression used in conditional expression");
  }
  if (error == 0 && (rightType == CJavaTypeSignature::kVoid ||
		     middleType == CJavaTypeSignature::kVoid)) {
    error = MakeError("Invalid use of void-typed expression");
  }
  if (error == 0) {
    if (context.GetCompiler().SameType(middleType, rightType)) {
      setType = middleType;
    } else if (middleType.IsNumeric() && rightType.IsNumeric()) {
      if (!UnifyNumeric(fMiddle, fRight, context, setType) &&
	  !UnifyNumeric(fRight, fMiddle, context, setType)) {
	if (BinaryPromote(fMiddle, fRight, context)) {
	  setType = fRight->GetType();
	} else {
	  error = MakeError("Incompatible types in conditional expression");
	}
      }
    } else if (middleType.IsReference() && rightType.IsReference()) {
      if (middleType == CJavaTypeSignature::kNullType) {
	error = CCastExpression::CastPointer(fMiddle, rightType, context);
	setType = rightType;
      } else if (rightType == CJavaTypeSignature::kNullType) {
	error = CCastExpression::CastPointer(fRight, middleType, context);
	setType = middleType;
      } else if (context.GetCompiler().AssignableSubtype(middleType,
							 rightType)) {
	error = CCastExpression::CastPointer(fMiddle, rightType, context);
	setType = rightType;
      } else if (context.GetCompiler().AssignableSubtype(rightType,
							 middleType)) {
	error = CCastExpression::CastPointer(fRight, middleType, context);
	setType = middleType;
      } else {
	error = MakeError("Incompatible types in conditional expression");
      }
    } else {
      error = MakeError("Incompatible types used in conditional expression");
    }
  }
  return error;
}

//
//  Method name : UnifyNumeric
//  Description : This method is used internally to try to unify two numeric
//    values to this trinary expression according to the rules in section
//    15.24 of the language specification.  If this unification is successful,
//    this method will return true and set the value of 'setType' accordingly.
//    Otherwise, this returns false.
//    This method assumes that the types of these expressions has already
//    been successfully evaluated.
//
bool
CTrinaryExpression::UnifyNumeric(CExpression*& first, CExpression*& second,
		       CCompileContext& context, CJavaTypeSignature& setType)
{
  COrdinalLiteral* literal = DYNAMIC_CAST(COrdinalLiteral, second);
  unsigned long long value;
  if (literal != 0) {
    value = literal->GetLong();
  }
  switch (first->GetType().GetBaseType()) {
  case CJavaTypeSignature::Short:
    if (second->GetType() == CJavaTypeSignature::kByte) {
      setType = CJavaTypeSignature::kShort;
      CCompileError* error =
	CCastExpression::CastPointer(second, setType, context);
      // casting a Byte value to a Short should always work.
      assert(error == 0);
      return true;
    } 
  case CJavaTypeSignature::Character:
    if (literal != 0) {
      short shortValue = (short)value;
      setType = first->GetType();
      if (shortValue == value) {
	delete second;
	second = new COrdinalLiteral(shortValue,
				    setType == CJavaTypeSignature::kCharacter);
	return true;
      }
    }
    break;
  case CJavaTypeSignature::Byte:
    if (literal != 0) {
      setType = first->GetType();
      signed char byteValue = (signed char)value;
      if (byteValue == value) {
	delete second;
	second = new COrdinalLiteral(byteValue);
	return true;
      }
    }
    break;
  }
  return false;
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
CTrinaryExpression::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(!isStatement);
  CCompileError* error = 0;
  COrdinalLiteral* literal = DYNAMIC_CAST(COrdinalLiteral, fLeft);
  if (literal != 0) {
    if (literal->GetBoolean()) {
      error = fMiddle->GenerateCode(code, context, stackUsed);
    } else {
      error = fRight->GenerateCode(code, context, stackUsed);
    }
  } else {
    unsigned short leftStack, middleStack, rightStack;
    error = fLeft->GenerateCode(code, context, leftStack);
    unsigned long branchInstruction = code.size();
    code.Append(CJavaCodeAttribute::ifeq, fLineNumber);
    if (error == 0) {
      CCompileContext elseContext = context;
      error = fMiddle->GenerateCode(code, context, middleStack);
      unsigned long gotoInstruction = code.size();
      code.Append(CJavaCodeAttribute::op_goto, fLineNumber);
      code[branchInstruction].fArguments.u4 = code.CreateBlockLabel();
      if (error == 0) {
	error = fRight->GenerateCode(code, elseContext, rightStack);
	code[gotoInstruction].fArguments.u4 = code.CreateBlockLabel();
	stackUsed = ::max(leftStack, ::max(middleStack, rightStack));
	context.Merge(elseContext);
      }
    }
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
CTrinaryExpression::CreateConstantExpression(CExpression*& result,
					     CCompileContext& context) const
{
  result = 0;
  if (fLeft->IsLiteral()) {
    COrdinalLiteral* left = DYNAMIC_CAST(COrdinalLiteral, fLeft);
    assert(left != 0);
    CExpression* selected = left->GetBoolean() ? fMiddle : fRight;
    if (selected->IsLiteral()) {
      result = selected->Clone();
    }
  }
  return 0;
}

//=========================== CUnaryExpression ================================

//
//  Method name : CUnaryExpression
//  Description : Constructs a unary expression out of the constituent parts:
//    the child expression, an operator to apply to it, and a flag to tell
//    whether it is an lvalue or not.
//
CUnaryExpression::CUnaryExpression(CExpression* childExpression,
		     unsigned short unaryOperator, Order side, bool isLValue)
  : CExpression(CJavaTypeSignature::Void, isLValue),
    fChild(childExpression),
    fOperator(unaryOperator),
    fSide(side)
{
}

//
//  Method name : CUnaryExpression
//  Description : Copy constructor.
//
CUnaryExpression::CUnaryExpression(const CUnaryExpression& source)
  : CExpression(source),
    fChild(0),
    fOperator(source.fOperator),
    fSide(source.fSide)
{
  if (source.fChild != 0) {
    fChild = source.fChild->Clone();
  }
}

//
//  Method name : ~CUnaryExpression
//  Description : Destructor
//
CUnaryExpression::~CUnaryExpression()
{
  delete fChild;
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
CUnaryExpression::HandleEvaluateType(CCompileContext& context,
				     CJavaTypeSignature& setType)
{
  assert(fChild != 0);
  CJavaTypeSignature type;
  CCompileError* error = EvaluateType(fChild, context, type,
				      fOperator == INCR || fOperator == DECR);
  if (error == 0) {
    switch (fOperator) {
    case '-':
    case '+':
      if (!type.IsNumeric()) {
	error = MakeError("Non-numeric value used in unary expression");
      } else {
	UnaryPromote(fChild, context);
	setType = fChild->GetType();
      }
      break;
    case INCR:
    case DECR:
      if (!type.IsNumeric()) {
	error = MakeError("Non-numeric value used in unary expression");
      } else if (!fChild->IsLValue()) {
	error = MakeError("Cannot modify constant field");
      } else {
	setType = type;
      }
      break;
    case '~':
      if (!type.IsOrdinal()) {
	error = MakeError("Non-integral value used with unary '~' expression");
      } else {
	UnaryPromote(fChild, context);
	setType = fChild->GetType();
      }
      break;
    case '!':
      if (type != CJavaTypeSignature::kBoolean) {
	error = MakeError("Non-boolean value used with '!' expressions");
      } else {
	setType = type;
      }
      break;
    default:
      assert(0);
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
CUnaryExpression::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  CCompileError* error = 0;
  switch (fOperator) {
  case '+':
    assert(!isStatement);
    error = fChild->GenerateCode(code, context, stackUsed);
    break;
  case '-':
    assert(!isStatement);
    error = fChild->GenerateCode(code, context, stackUsed);
    switch (fChild->GetType().GetBaseType()) {
    case CJavaTypeSignature::Float:
      code.Append(CJavaCodeAttribute::fneg, fLineNumber);
      break;
    case CJavaTypeSignature::LongInteger:
      code.Append(CJavaCodeAttribute::lneg, fLineNumber);
      break;
    case CJavaTypeSignature::Double:
      code.Append(CJavaCodeAttribute::dneg, fLineNumber);
      break;
    default:
      code.Append(CJavaCodeAttribute::ineg, fLineNumber);
      break;
    }
    break;
  case '!':
    assert(!isStatement);
    error = fChild->GenerateCode(code, context, stackUsed);
    code.Append(CJavaCodeAttribute::ifeq, fLineNumber);
    code.Append(CJavaCodeAttribute::iconst_0, fLineNumber);
    code.Append(CJavaCodeAttribute::op_goto, fLineNumber);
    code[code.size() - 3].fArguments.u4 = code.CreateBlockLabel();
    code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
    code[code.size() - 2].fArguments.u4 = code.CreateBlockLabel();
    // their verifier doesn't seem to like this one.  :-(
    //    code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
    //    code.Append(CJavaCodeAttribute::ixor, fLineNumber);
    break;
  case '~':
    assert(!isStatement);
    error = fChild->GenerateCode(code, context, stackUsed);
    if (fChild->GetType() == CJavaTypeSignature::kLong) {
      long long negativeOne = -1;
      JavaConstantIndex index =
	context.GetClass().AddLongConstant((unsigned long long)negativeOne);
      code.Append(CJavaCodeAttribute::ldc2_w, index, fLineNumber);
      code.Append(CJavaCodeAttribute::lxor, fLineNumber);
    } else {
      code.Append(CJavaCodeAttribute::iconst_m1, fLineNumber);
      code.Append(CJavaCodeAttribute::ixor, fLineNumber);
    }
    break;
  case INCR:
    {
      CLocalVariableExpression* localVariable =
	DYNAMIC_CAST(CLocalVariableExpression, fChild);
      bool local = (localVariable != 0);
      if (local && localVariable->GetType() == CJavaTypeSignature::kInteger) {
	stackUsed = 0;
	if (fSide == kPostfix && !isStatement) {
	  localVariable->GenerateCode(code, context, stackUsed);
	}
	unsigned short localVariableIndex =
	  localVariable->GetVariableIndex(context);
	CCodeSequence::Instruction instruction;
	instruction.fOpcode = CJavaCodeAttribute::iinc;
	instruction.fArguments.fIndexPair.index = localVariableIndex;
	instruction.fArguments.fIndexPair.u1 = 1;
	code.Append(instruction, fLineNumber);
      	if (fSide == kPrefix && !isStatement) {
	  localVariable->GenerateCode(code, context, stackUsed);
	}
      } else {
	error = fChild->StartStoreCode(code, context, stackUsed);
	if (fSide == kPostfix && !isStatement && stackUsed > 0) {
	  int childWidth = fChild->GetType().GetWidth();
	  stackUsed += childWidth;
	  if (childWidth == 1) {
	    if (local) {
	      code.Append(CJavaCodeAttribute::dup, fLineNumber);
	    } else {
	      code.Append(CJavaCodeAttribute::dup_x1, fLineNumber);
	    }
	  } else {
	    assert(childWidth == 2);
	    if (local) {
	      code.Append(CJavaCodeAttribute::dup2, fLineNumber);
	    } else {
	      code.Append(CJavaCodeAttribute::dup2_x1, fLineNumber);
	    }
	  }
	}
	switch (fChild->GetType().GetBaseType()) {
	case CJavaTypeSignature::Float:
	  code.Append(CJavaCodeAttribute::fconst_1, fLineNumber);
	  code.Append(CJavaCodeAttribute::fadd, fLineNumber);
	  break;
	case CJavaTypeSignature::LongInteger:
	  code.Append(CJavaCodeAttribute::lconst_1, fLineNumber);
	  code.Append(CJavaCodeAttribute::ladd, fLineNumber);
	  break;
	case CJavaTypeSignature::Double:
	  code.Append(CJavaCodeAttribute::dconst_1, fLineNumber);
	  code.Append(CJavaCodeAttribute::dadd, fLineNumber);
	  break;
	case CJavaTypeSignature::Byte:
	  code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
	  code.Append(CJavaCodeAttribute::iadd, fLineNumber);
	  code.Append(CJavaCodeAttribute::int2byte, fLineNumber);
	  break;
	case CJavaTypeSignature::Character:
	  code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
	  code.Append(CJavaCodeAttribute::iadd, fLineNumber);
	  code.Append(CJavaCodeAttribute::int2char, fLineNumber);
	  break;
	case CJavaTypeSignature::Short:
	  code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
	  code.Append(CJavaCodeAttribute::iadd, fLineNumber);
	  code.Append(CJavaCodeAttribute::int2short, fLineNumber);
	  break;
	case CJavaTypeSignature::Integer:
	  code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
	  code.Append(CJavaCodeAttribute::iadd, fLineNumber);
	  break;
	default:
	  assert(0);
	}
	if (error == 0) {
	  unsigned short ignored;
	  error = fChild->EndStoreCode(code, context, ignored,
				       isStatement || fSide != kPrefix);
	  stackUsed += fChild->GetType().GetWidth();
	}
      }
    }
    break;
  case DECR:
    {
      CLocalVariableExpression* localVariable =
	DYNAMIC_CAST(CLocalVariableExpression, fChild);
      bool local = (localVariable != 0);
      if (local && localVariable->GetType() == CJavaTypeSignature::kInteger) {
	stackUsed = 0;
	if (fSide == kPostfix && !isStatement) {
	  localVariable->GenerateCode(code, context, stackUsed);
	}
	unsigned short localVariableIndex =
	  localVariable->GetVariableIndex(context);
	CCodeSequence::Instruction instruction;
	instruction.fOpcode = CJavaCodeAttribute::iinc;
	instruction.fArguments.fIndexPair.index = localVariableIndex;
	instruction.fArguments.fIndexPair.u1 = (unsigned char)-1;
	code.Append(instruction, fLineNumber);
	if (fSide == kPrefix && !isStatement) {
	  localVariable->GenerateCode(code, context, stackUsed);
	}
      } else {
	error = fChild->StartStoreCode(code, context, stackUsed);
	if (fSide == kPostfix && !isStatement && stackUsed > 0) {
	  int childWidth = fChild->GetType().GetWidth();
	  stackUsed += childWidth;
	  if (childWidth == 1) {
	    if (local) {
	      code.Append(CJavaCodeAttribute::dup, fLineNumber);
	    } else {
	      code.Append(CJavaCodeAttribute::dup_x1, fLineNumber);
	    }
	  } else {
	    assert(childWidth == 2);
	    if (local) {
	      code.Append(CJavaCodeAttribute::dup2, fLineNumber);
	    } else {
	      code.Append(CJavaCodeAttribute::dup2_x1, fLineNumber);
	    }
	  }
	}
	switch (fChild->GetType().GetBaseType()) {
	case CJavaTypeSignature::Float:
	  code.Append(CJavaCodeAttribute::fconst_1, fLineNumber);
	  code.Append(CJavaCodeAttribute::fsub, fLineNumber);
	  break;
	case CJavaTypeSignature::LongInteger:
	  code.Append(CJavaCodeAttribute::lconst_1, fLineNumber);
	  code.Append(CJavaCodeAttribute::lsub, fLineNumber);
	  break;
	case CJavaTypeSignature::Double:
	  code.Append(CJavaCodeAttribute::dconst_1, fLineNumber);
	  code.Append(CJavaCodeAttribute::dsub, fLineNumber);
	  break;
	case CJavaTypeSignature::Byte:
	  code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
	  code.Append(CJavaCodeAttribute::isub, fLineNumber);
	  code.Append(CJavaCodeAttribute::int2byte, fLineNumber);
	  break;
	case CJavaTypeSignature::Character:
	  code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
	  code.Append(CJavaCodeAttribute::isub, fLineNumber);
	  code.Append(CJavaCodeAttribute::int2char, fLineNumber);
	  break;
	case CJavaTypeSignature::Short:
	  code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
	  code.Append(CJavaCodeAttribute::isub, fLineNumber);
	  code.Append(CJavaCodeAttribute::int2short, fLineNumber);
	  break;
	case CJavaTypeSignature::Integer:
	  code.Append(CJavaCodeAttribute::iconst_1, fLineNumber);
	  code.Append(CJavaCodeAttribute::isub, fLineNumber);
	  break;
	default:
	  assert(0);
	}
	if (error == 0) {
	  unsigned short ignored;
	  error = fChild->EndStoreCode(code, context, ignored,
				       isStatement || fSide != kPrefix);
	  stackUsed += fChild->GetType().GetWidth();
	}
      }
    }
    break;
  default:
    assert(0);
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
CUnaryExpression::CreateConstantExpression(CExpression*& result,
					     CCompileContext& context) const
{
  result = 0;
  assert(fChild != 0);
  if (fChild->IsLiteral()) {
    COrdinalLiteral* intLiteral = DYNAMIC_CAST(COrdinalLiteral, fChild);
    CFloatLiteral* floatLiteral = DYNAMIC_CAST(CFloatLiteral, fChild);
    switch (fOperator) {
    case '-':
      switch (fChild->GetType().GetBaseType()) {
      case CJavaTypeSignature::LongInteger:
	assert(intLiteral != 0);
	result = new COrdinalLiteral(-(long long)intLiteral->GetLong());
	break;
      case CJavaTypeSignature::Integer:
	assert(intLiteral != 0);
	result = new COrdinalLiteral(-(long)intLiteral->GetInteger());
	break;
      case CJavaTypeSignature::Float:
	assert(floatLiteral != 0);
	result = new CFloatLiteral(-floatLiteral->GetFloat());
	break;
      case CJavaTypeSignature::Double:
	assert(floatLiteral != 0);
	result = new CFloatLiteral(-floatLiteral->GetDouble());
	break;
      default:
	assert(0);
      }
      break;
    case '+':
      switch (fChild->GetType().GetBaseType()) {
      case CJavaTypeSignature::LongInteger:
	assert(intLiteral != 0);
	result = new COrdinalLiteral(+intLiteral->GetLong());
	break;
      case CJavaTypeSignature::Integer:
	assert(intLiteral != 0);
	result = new COrdinalLiteral(+intLiteral->GetInteger());
	break;
      case CJavaTypeSignature::Float:
	assert(floatLiteral != 0);
	result = new CFloatLiteral(+floatLiteral->GetFloat());
	break;
      case CJavaTypeSignature::Double:
	assert(floatLiteral != 0);
	result = new CFloatLiteral(+floatLiteral->GetDouble());
	break;
      default:
	assert(0);
      }
      break;
    case '~':
      assert(intLiteral != 0);
      switch (intLiteral->GetType().GetBaseType()) {
      case CJavaTypeSignature::LongInteger:
	result = new COrdinalLiteral(~intLiteral->GetLong());
	break;
      default:
	result = new COrdinalLiteral(~intLiteral->GetInteger());
      }
      break;
    case '!':
      assert(intLiteral != 0);
      result = new COrdinalLiteral(!intLiteral->GetBoolean());
      break;
    default:
      assert(0);  // increment and decrement shouldn't have a literal child.
    }
  }  
  return 0;
}

//======================== CLocalVariableExpression ===========================

//
//  Method name : CLocalVariableExpression
//  Description : This constructor is used to make a variable name expression
//    using a field signature (a combination of type and name) and the index
//    of this variable as used in the local variable array.
//
CLocalVariableExpression::CLocalVariableExpression(
		       const CJavaFieldSignature& name, unsigned short index,
		       bool final)
  : CExpression(name.GetType(), !final),
    fName(name.GetFieldName()),
    fLocalVariableIndex(index)
{
}

//
//  Method name : CLocalVariableExpression
//  Description : Copy constructor.
//
CLocalVariableExpression::CLocalVariableExpression(
				   const CLocalVariableExpression& source)
  : CExpression(source),
    fName(source.fName),
    fLocalVariableIndex(source.fLocalVariableIndex)
{
}

//
//  Method name : ~CLocalVariableExpression
//  Description : Destructor.
//
CLocalVariableExpression::~CLocalVariableExpression()
{
}

//
//  Method name : GetVariableIndex
//  Description : Returns the local variable index that will be used to hold
//    this variable in the virtual stack machine.
//
unsigned short
CLocalVariableExpression::GetVariableIndex(const CCompileContext& context)const
{
  return context.GetLocalVariableLocation(fLocalVariableIndex);
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
CLocalVariableExpression::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(!isStatement);
  CCompileError* error = 0;
  unsigned short index = GetVariableIndex(context);
  if (!context.IsVariableInitialized(index)) {
    unicode_string errorString = ::StringToUnicode("Local variable ");
    errorString += fName;
    errorString += ::StringToUnicode(" may not be initialized at this point");
    error = MakeError(errorString);
  } else {
    stackUsed = GetType().GetWidth();
    if (GetType().IsReference()) {
      code.Append(CJavaCodeAttribute::aload, index, fLineNumber);
    } else {
      switch (GetType().GetBaseType()) {
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::dload, index, fLineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::lload, index, fLineNumber);
	break;
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::fload, index, fLineNumber);
	break;
      case CJavaTypeSignature::Integer:
      case CJavaTypeSignature::Byte:
      case CJavaTypeSignature::Character:
      case CJavaTypeSignature::Short:
      case CJavaTypeSignature::Boolean:
	code.Append(CJavaCodeAttribute::iload, index, fLineNumber);
	break;
      default:
	assert(0);
	break;
      }
    }
  }
  return error;
}


//
//  Method name : StartStoreCode
//  Description : This method is used to generate the instructions needed at
//    the beginning of a store to a variable.  The storing code is split into
//    two parts this way because Java is a little inconsistent ... storing
//    into a variable puts code after the desired expression, but storing
//    into an array puts code before and after the value, so this has to
//    cover both cases.  Override as appropriate.
//
CCompileError*
CLocalVariableExpression::StartStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool evaluateToo)
{
  CCompileError* error = 0;
  stackUsed = 0;
  if (!IsLValue()) {
    error = MakeError("Invalid local variable assignment.");
  } else {
    if (evaluateToo) {
      error = GenerateCode(code, context, stackUsed);
    }
  }
  return error;
}

//
//  Method name : EndStoreCode
//  Description : This method is used to generate the instructions needed at
//    the end of a store to a variable.  The storing code is split into
//    two parts this way because Java is a little inconsistent ... storing
//    into a variable puts code after the desired expression, but storing
//    into an array puts code before and after the value, so this has to
//    cover both cases.
//
CCompileError*
CLocalVariableExpression::EndStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  if (!isStatement) {
    stackUsed = GetType().GetWidth();
    if (stackUsed == 1) {
      code.Append(CJavaCodeAttribute::dup, fLineNumber);
    } else {
      assert(stackUsed == 2);
      code.Append(CJavaCodeAttribute::dup2, fLineNumber);
    }
  } else {
    stackUsed = 0;
  }
  unsigned short index = GetVariableIndex(context);
  if (GetType().IsReference()) {
    code.Append(CJavaCodeAttribute::astore, index, fLineNumber);
  } else {
    switch (GetType().GetBaseType()) {
    case CJavaTypeSignature::Double:
      code.Append(CJavaCodeAttribute::dstore, index, fLineNumber);
      break;
    case CJavaTypeSignature::LongInteger:
      code.Append(CJavaCodeAttribute::lstore, index, fLineNumber);
      break;
    case CJavaTypeSignature::Float:
      code.Append(CJavaCodeAttribute::fstore, index, fLineNumber);
      break;
    case CJavaTypeSignature::Integer:
    case CJavaTypeSignature::Byte:
    case CJavaTypeSignature::Character:
    case CJavaTypeSignature::Short:
    case CJavaTypeSignature::Boolean:
      code.Append(CJavaCodeAttribute::istore, index, fLineNumber);
      break;
    default:
      assert(0);
      break;
    }
  }
  context.InitializeVariable(index);
  return 0;
}


//========================== COuterLocalExpression ============================

//
//  Method name : COuterLocalExpression
//  Description : Constructor.
//
COuterLocalExpression::COuterLocalExpression(const CVariableDeclaration& decl,
					     unsigned short localVariableIndex)
  : CExpression(CJavaTypeSignature::Void, false),
    fDeclaration(decl),
    fLocalVariableIndex(localVariableIndex),
    fSurrogate(0)
{
}

//
//  Method name : COuterLocalExpression
//  Description : Copy constructor.
//
COuterLocalExpression::COuterLocalExpression(
                                  const COuterLocalExpression& source)
  : CExpression(source),
    fDeclaration(source.fDeclaration),
    fLocalVariableIndex(source.fLocalVariableIndex),
    fSurrogate(0)
{
  if (source.fSurrogate != 0) {
    fSurrogate = source.fSurrogate->Clone();
  }
}

//
//  Method name : ~COuterLocalExpression
//  Description : Destructor.
//
COuterLocalExpression::~COuterLocalExpression()
{
  delete fSurrogate;
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
COuterLocalExpression::HandleEvaluateType(CCompileContext& context,
					  CJavaTypeSignature& setType)
{
  fSurrogate = new CClassFieldExpression(GetLabel());
  CJavaTypeSignature surrogateType;
  CCompileError* error = EvaluateType(fSurrogate, context, surrogateType);
  if (error == 0) {
    setType = surrogateType;
  } else {
    delete error;
    delete fSurrogate;
    if (!fDeclaration.IsFinal()) {
      string errorString("Cannot access non-final local variable: ");
      errorString += ::UnicodeToString(fDeclaration.GetName());
      error = MakeError(errorString);
    } else {
      fSurrogate = new CClassFieldExpression(CCompiler::kSyntheticFieldPrefix +
					     GetLabel());
      error = fSurrogate->EvaluateType(fSurrogate, context, setType);
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
COuterLocalExpression::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(fSurrogate != 0);
  CCompileError* error =
    fSurrogate->GenerateCode(code, context, stackUsed, isStatement);;
  return error;
}

//========================== CClassFieldExpression ============================

//
//  Method name : CClassFieldExpression
//  Description : Constructs an unknown class field out of its name only.
//
CClassFieldExpression::CClassFieldExpression(const unicode_string& label)
  : CExpression(CJavaTypeSignature::Void, true),
    fLabel(label),
    fClassAlias(0),
    fBase(0)
{
}

//
//  Method name : CClassFieldExpression
//  Description : Constructs an unknown class field out of its name only.
//
CClassFieldExpression::CClassFieldExpression(CExpression* adoptBase,
					     const unicode_string& label)
  : CExpression(CJavaTypeSignature::Void, true),
    fLabel(label),
    fClassAlias(0),
    fBase(adoptBase)
{
}

//
//  Method name : CClassFieldExpression
//  Description : Copy constructor.
//
CClassFieldExpression::CClassFieldExpression(
                                  const CClassFieldExpression& source)
  : CExpression(source),
    fLabel(source.fLabel),
    fFieldInfo(source.fFieldInfo),
    fClassAlias(source.fClassAlias),
    fBase(0)
{
  if (source.fBase != 0) {
    fBase = source.fBase->Clone();
  }
}

//
//  Method name : ~CClassFieldExpression
//  Description : Destructor.
//
CClassFieldExpression::~CClassFieldExpression()
{
  delete fBase;
}

//
//  Method name : IsStatic
//  Description : Returns true if this is a static field.
//
bool
CClassFieldExpression::IsStatic() const
{
  return fClassAlias != 0 && fFieldInfo.GetModifiers().fStatic != 0;
}

//
//  Method name : IsArrayLength
//  Description : Returns true if this field is used to get the length of an
//    array.
//
bool
CClassFieldExpression::IsArrayLength() const
{
  bool result = fBase != 0;
  if (result) {
    CJavaTypeSignature childType = fBase->GetType();
    result = childType.IsReference() && childType.GetArrayBounds() > 0 &&
      fLabel == kLengthName;
  }
  return result;
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
CClassFieldExpression::HandleEvaluateType(CCompileContext& context,
					  CJavaTypeSignature& setType)
{
  assert(fClassAlias == 0);
  CCompileError* error = 0;
  unicode_string::size_type lastSlash = fLabel.find_last_of('/');
  if (fBase != 0) {
    assert(lastSlash == unicode_string::npos);
    error = EvaluateBaseType(context, setType);
  } else if (lastSlash == unicode_string::npos) {
    fClassAlias =
      context.GetCompiler().FindField(fLabel, context.GetClass(), fFieldInfo);
    if (fClassAlias != 0) {
      setType = fFieldInfo.GetSignature().GetType();
      if (!IsStatic()) {
	fBase = new CSpecialExpression(CSpecialExpression::kThis);
	fBase->InheritLineNumber(*this);
      }
    } else {
      // try looking for the name in an enclosing scope.
      unicode_string outerName = context.GetClass().GetOuterName();
      if (outerName.length() > 0) {
	const CJavaClassFile* outer =
	  context.GetCompiler().LookupClass(outerName, context);
	assert(outer != 0);
	fClassAlias =
	  context.GetCompiler().FindField(fLabel, *outer, fFieldInfo);
	if (fClassAlias != 0) {
	  setType = fFieldInfo.GetSignature().GetType();
	  if (!IsStatic()) {
	    if (context.GetClass().GetAccessFlags().fStatic != 0) {
	      error = MakeError("Cannot access non-static field: " +
				::UnicodeToUTF(fLabel));
	    } else {
	      fBase = new CClassFieldExpression(
		      new CSpecialExpression(CSpecialExpression::kThis),
		      CCompiler::kOuterThisName);
	      fBase->InheritLineNumber(*this);
	      error = EvaluateBaseType(context, setType);
	    }
	  }
	}
      }

      if (fClassAlias == 0 && error == 0) {
	string errorString("Unknown variable: ");
	errorString += ::UnicodeToUTF(fLabel);
	error = MakeError(errorString);
      }
    }
  } else {
    unicode_string baseName(fLabel, 0, lastSlash);
    fLabel.remove(0, lastSlash + 1);
    fBase = new CClassFieldExpression(baseName);
    fBase->InheritLineNumber(*this);
    error = EvaluateBaseType(context, setType);
    if (error != 0) {
      const CJavaClassFile* explicitClass =
	context.GetCompiler().LookupClass(baseName, context);
      if (explicitClass != 0) {
	delete error;
	error = 0;
	delete fBase;
	fBase = 0;
	fClassAlias =
	  context.GetCompiler().FindField(fLabel, *explicitClass, fFieldInfo);
	if (fClassAlias == 0) {
	  string errorString("No field ");
	  errorString += ::UnicodeToUTF(fLabel);
	  errorString += " found on class ";
	  errorString += ::UnicodeToUTF(baseName);
	  error = MakeError(errorString);
	} else {
	  setType = fFieldInfo.GetSignature().GetType();
	  if (!IsStatic()) {
	    if (context.GetCompiler().DescendsFrom(context.GetClass(),
						   *fClassAlias)) {
	      fBase = new CSpecialExpression(CSpecialExpression::kThis);
	      fBase->InheritLineNumber(*this);
	    } else {
	      string errorString("Invalid access to non-static field: ");
	      errorString += ::UnicodeToUTF(fLabel);
	      error = MakeError(errorString);
	    }
	  }
	}
      }
    }
  }
  if (error == 0 && fClassAlias != 0) {
    if (fClassAlias != &context.GetClass() &&
	fFieldInfo.GetModifiers().fPublic == 0) {
      if (fFieldInfo.GetModifiers().fPrivate != 0) {
	string errorString("Invalid access to private field ");
	errorString += ::UnicodeToUTF(fLabel);
	error = MakeError(errorString);
      } else if (!(fClassAlias->GetPackageName() ==
		   context.GetClass().GetPackageName())) {
	if (fFieldInfo.GetModifiers().fProtected == 0 ||
	    DYNAMIC_CAST(CSpecialExpression, fBase) == 0 ||
	    !context.GetCompiler().DescendsFrom(context.GetClass(),
						*fClassAlias)) {
	  string errorString("Field ");
	  errorString += ::UnicodeToString(fLabel);
	  errorString += " is not accessible to this class.";
	  error = MakeError(errorString); 
	}
      }
    }
    if (error == 0) {
      SetType(setType);
      if (fFieldInfo.GetModifiers().fFinal != 0 &&
	  !context.GetCompiler().InClassInitializers()) {
	fLValue = false;
      }
    }
  }
  return error;
}

//
//  Method name : EvaluateBaseType
//  Description : This internal method is used by the field expression to
//    figure out the type of the fBase base expression that is being held,
//    and then use this to assess the type of this value, which is assigned
//    to the 'setType' parameter.
//    If any problems arise during this evaluation, an error is created and
//    returned to the caller.
//
CCompileError*
CClassFieldExpression::EvaluateBaseType(CCompileContext& context,
					CJavaTypeSignature& setType)
{
  CCompileError* error = 0;
  assert(fBase != 0);
  CJavaTypeSignature childType;
  error = EvaluateType(fBase, context, childType, IsLValue());
  if (error == 0) {
    if (!childType.IsReference()) {
      string errorString("Cannot access field ");
      errorString += ::UnicodeToUTF(fLabel);
      errorString += " through non-reference value";
      error = MakeError(errorString);
    } else if (childType.GetArrayBounds() > 0) {
      if (fLabel == kLengthName) {
	setType = CJavaTypeSignature::kInteger;
	fLValue = false;
      } else {
	error = MakeError("Invalid field access on array value.");
      }
    } else {
      unicode_string className;
      bool isClass = childType.GetBaseClassName(className);
      assert(isClass);
      const CJavaClassFile* classFile =
	context.GetCompiler().LookupClass(className, context);
      if (classFile == 0) {
	string errorString("Unknown class name: ");
	errorString += ::UnicodeToUTF(className);
	error = MakeError(errorString);
      } else {
	fClassAlias =
	  context.GetCompiler().FindField(fLabel, *classFile, fFieldInfo);
	if (fClassAlias == 0) {
	  string errorString("No field ");
	  errorString += ::UnicodeToUTF(fLabel);
	  errorString += " found on class ";
	  errorString += ::UnicodeToUTF(className);
	  error = MakeError(errorString);
	} else {
	  setType = fFieldInfo.GetSignature().GetType();
	}
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
CClassFieldExpression::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  CCompileError* error = 0;
  if (IsArrayLength()) {
    error = fBase->GenerateCode(code, context, stackUsed);
    code.Append(CJavaCodeAttribute::arraylength, fLineNumber);
  } else if (IsStatic()) {
    assert(fClassAlias != 0);
    if (fBase != 0) {
      // don't bother generating code to load and pop a local variable.
      if (DYNAMIC_CAST(CLocalVariableExpression, fBase) == 0) {
	error = fBase->GenerateCode(code, context, stackUsed);
	code.Append(CJavaCodeAttribute::pop, fBase->GetLineNumber());
      }
    }
    JavaConstantIndex index = 
      context.GetClass().AddFieldConstant(fClassAlias->GetClassName(),
					  fLabel, GetType());
    code.Append(CJavaCodeAttribute::getstatic, index, fLineNumber);
    stackUsed = (unsigned short)GetType().GetWidth();
  } else {
    assert(fClassAlias != 0 && fBase != 0);
    error = fBase->GenerateCode(code, context, stackUsed);
    JavaConstantIndex index = 
      context.GetClass().AddFieldConstant(fClassAlias->GetClassName(),
					  fLabel, GetType());
    code.Append(CJavaCodeAttribute::getfield, index, fLineNumber);
    stackUsed = ::max(stackUsed, (unsigned short)GetType().GetWidth());
  }
  return error;
}

//
//  Method name : StartStoreCode
//  Description : This method is used to generate the instructions needed at
//    the beginning of a store to a variable.  The storing code is split into
//    two parts this way because Java is a little inconsistent ... storing
//    into a variable puts code after the desired expression, but storing
//    into an array puts code before and after the value, so this has to
//    cover both cases.  Override as appropriate.
//
CCompileError*
CClassFieldExpression::StartStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool evaluateToo)
{
  CCompileError* error = 0;
  assert(!IsArrayLength() && fClassAlias != 0);
  if (!IsLValue()) {
    error = MakeError("Invalid field assignment.");
  } else if (IsStatic()) {
    if (evaluateToo) {
      JavaConstantIndex index = 
	context.GetClass().AddFieldConstant(fClassAlias->GetClassName(),
					    fLabel, GetType());
      code.Append(CJavaCodeAttribute::getstatic, index, fLineNumber);
      stackUsed = GetType().GetWidth();
    } else {
      stackUsed = 0;
    }
  } else {
    error = fBase->GenerateCode(code, context, stackUsed);
    if (evaluateToo) {
      code.Append(CJavaCodeAttribute::dup, fLineNumber);
      JavaConstantIndex index =
	context.GetClass().AddFieldConstant(fClassAlias->GetClassName(),
					    fLabel, GetType());
      code.Append(CJavaCodeAttribute::getfield, index, fLineNumber);
      stackUsed = ::max(stackUsed, (unsigned short)GetType().GetWidth()) + 1;
    }
  }
  return error;
}

//
//  Method name : EndStoreCode
//  Description : This method is used to generate the instructions needed at
//    the end of a store to a variable.  The storing code is split into
//    two parts this way because Java is a little inconsistent ... storing
//    into a variable puts code after the desired expression, but storing
//    into an array puts code before and after the value, so this has to
//    cover both cases.
//
CCompileError*
CClassFieldExpression::EndStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(!IsArrayLength() && fClassAlias != 0);
  bool isStatic = IsStatic();
  if (!isStatement) {
    stackUsed = GetType().GetWidth();
    if (stackUsed == 1) {
      code.Append(isStatic ? CJavaCodeAttribute::dup :
		  CJavaCodeAttribute::dup_x1, fLineNumber);
    } else {
      assert(stackUsed == 2);
      code.Append(isStatic ? CJavaCodeAttribute::dup2 :
		  CJavaCodeAttribute::dup2_x1, fLineNumber);
    }
  } else {
    stackUsed = 0;
  }
  JavaConstantIndex index =
    context.GetClass().AddFieldConstant(fClassAlias->GetClassName(),
					fLabel, GetType());
  code.Append(isStatic ? CJavaCodeAttribute::putstatic :
	      CJavaCodeAttribute::putfield, index, fLineNumber);
  return 0;
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
CClassFieldExpression::CreateConstantExpression(CExpression*& result,
						CCompileContext& context) const
{
  CCompileError* error = 0;
  result = 0;
  if (!IsArrayLength()) {
    error = context.GetCompiler().FindConstantField(result,
		 fClassAlias->GetClassName(), fFieldInfo.GetSignature(),
                 context, GetLineNumber());
  }
  return error;
}


//=========================== CArrayInitializer ===============================

//
//  Method name : CArrayInitializer
//  Description : Creates an array initializer expression from a list of
//    sub-expressions.
//
CArrayInitializer::CArrayInitializer(ExpressionList* adoptExpressions)
  : CExpression(CJavaTypeSignature::Void, false)
{
  if (adoptExpressions != 0) {
    fExpressions = *adoptExpressions;
    delete adoptExpressions;
  }
}

//
//  Method name : CArrayInitializer
//  Description : Copy constructor.
//
CArrayInitializer::CArrayInitializer(const CArrayInitializer& source)
  : CExpression(source)
{
  for (ExpressionList::const_iterator i = source.fExpressions.begin();
       !(i == source.fExpressions.end()); ++i) {
    fExpressions.push_back((*i)->Clone());
  }
}

//
//  Method name : ~CArrayInitializer
//  Description : Destructor
//
CArrayInitializer::~CArrayInitializer()
{
  for (ExpressionList::iterator i = fExpressions.begin();
       i != fExpressions.end(); i++) {
    delete *i;
  }
}

//
//  Method name : SetArrayType
//  Description : This method is used by the holder of an array initializer
//    to tell it what type it is.  This is backwards from all of the rest
//    of the expressions, which figure out their own type ... with
//    initializers, their type is imposed from the outside based on their
//    context.  This must be called before 'GenerateCode' can be used.
//
void
CArrayInitializer::SetArrayType(const CJavaTypeSignature& type)
{
  if (type.GetArrayBounds() > 0) {
    SetType(type);
    CJavaTypeSignature childType = type;
    childType.SetArrayBounds(childType.GetArrayBounds() - 1);
    for (ExpressionList::iterator i = fExpressions.begin();
	 i != fExpressions.end(); ++i) {
      CArrayInitializer* child = DYNAMIC_CAST(CArrayInitializer, *i);
      if (child != 0) {
	child->SetArrayType(childType);
      }
    }
  }
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
CArrayInitializer::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(!isStatement);
  CCompileError* error = 0;
  if (GetType().GetArrayBounds() == 0) {
    error = MakeError("Invalid non-array type for an array initializer.");
  } else {
    error = context.GetCompiler().CheckType(GetType());
  }
  if (error == 0) {
    CJavaTypeSignature elementType = GetType();
    elementType.SetArrayBounds(elementType.GetArrayBounds() - 1);
    unsigned long arraySize = fExpressions.size();
    ExpressionList* dummyExpressionList = new ExpressionList;
    dummyExpressionList->push_front(new COrdinalLiteral(arraySize));
    CNewArray dummyArray(GetType(), dummyExpressionList);
    error = dummyArray.GenerateCode(code, context, stackUsed);
    CJavaTypeSignature childType;
    unsigned short maxElementStack = 0;
    unsigned long elementIndex = 0;
    CJavaCodeAttribute::Opcode storeOpcode;
    if (elementType.IsReference()) {
      storeOpcode = CJavaCodeAttribute::aastore;
    } else {
      switch (elementType.GetBaseType()) {
      case CJavaTypeSignature::Integer:
	storeOpcode = CJavaCodeAttribute::iastore;
	break;
      case CJavaTypeSignature::LongInteger:
	storeOpcode = CJavaCodeAttribute::lastore;
	break;
      case CJavaTypeSignature::Float:
	storeOpcode = CJavaCodeAttribute::fastore;
	break;
      case CJavaTypeSignature::Double:
	storeOpcode = CJavaCodeAttribute::dastore;
	break;
      case CJavaTypeSignature::Byte:
      case CJavaTypeSignature::Boolean:
	storeOpcode = CJavaCodeAttribute::bastore;
	break;
      case CJavaTypeSignature::Character:
	storeOpcode = CJavaCodeAttribute::castore;
	break;
      case CJavaTypeSignature::Short:
	storeOpcode = CJavaCodeAttribute::sastore;
	break;
      default:
	assert(0);
      }
    }
    for (ExpressionList::iterator i = fExpressions.begin();
	 error == 0 && i != fExpressions.end(); ++i, ++elementIndex) {
      code.Append(CJavaCodeAttribute::dup, fLineNumber);
      error = EvaluateType(*i, context, childType);
      if (error == 0) {
	if (!context.GetCompiler().SameType(childType, elementType)) {
	  error = CCastExpression::CastPointer(*i, elementType, context);
	}
	if (error == 0) {
	  unsigned short childStack;
	  COrdinalLiteral arrayIndexLiteral(elementIndex);
	  error = arrayIndexLiteral.GenerateCode(code,
						 context, childStack);
	  assert(error == 0);
	  error = (*i)->GenerateCode(code, context, childStack);
	  maxElementStack =
	    ::max(maxElementStack, (unsigned short)(childStack + 2));
	  code.Append(storeOpcode, fLineNumber);
	}
      }
    }
    stackUsed = ::max(stackUsed, (unsigned short)(maxElementStack + 1));
  }
  return error;
}

//=============================== CArrayIndex =================================

//
//  Method name : CArrayIndex
//  Description : Constructs an array index expression out of a base
//    expression and a subscript expression:
//         baseExpression [ subscriptExpression ]
//
CArrayIndex::CArrayIndex(CExpression* baseExpression, CExpression* index)
  : CExpression(CJavaTypeSignature::Void, true),
    fBase(baseExpression),
    fIndex(index)
{
}

//
//  Method name : CArrayIndex
//  Description : Copy constructor.
//
CArrayIndex::CArrayIndex(const CArrayIndex& source)
  : CExpression(source), fBase(0), fIndex(0)
{
  if (source.fBase != 0) {
    fBase = source.fBase->Clone();
  }
  if (source.fIndex != 0) {
    fIndex = source.fIndex->Clone();
  }
}

//
//  Method name : ~CArrayIndex
//  Description : Destructor
//
CArrayIndex::~CArrayIndex()
{
  delete fBase;
  delete fIndex;
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
CArrayIndex::HandleEvaluateType(CCompileContext& context,
				CJavaTypeSignature& setType)
{
  assert(fBase != 0 && fIndex != 0);
  CJavaTypeSignature baseType, indexType;
  CCompileError* error = EvaluateType(fBase, context, baseType);
  if (error == 0) {
    error = EvaluateType(fIndex, context, indexType);
  }
  if (error == 0) {
    if (baseType.GetArrayBounds() == 0) {
      error = MakeError("Index taken of non-array typed expression.");
    } else if (!UnaryPromote(fIndex, context)) {
      error = MakeError("Non-integer expression used in array index.");
    } else {
      baseType.SetArrayBounds(baseType.GetArrayBounds() - 1);
      setType = baseType;
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
CArrayIndex::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(!isStatement);
  unsigned short baseStackUsed, indexStackUsed;
  CCompileError* error =
    fBase->GenerateCode(code, context, baseStackUsed);
  if (error == 0) {
    error = fIndex->GenerateCode(code, context, indexStackUsed);
  }
  if (error == 0) {
    stackUsed = ::max(baseStackUsed, (unsigned short)(indexStackUsed + 1));
    CJavaTypeSignature baseType = fBase->GetType();
    baseType.SetArrayBounds(baseType.GetArrayBounds() - 1);
    if (baseType.IsReference()) {
      code.Append(CJavaCodeAttribute::aaload, fLineNumber);
    } else {
      switch(baseType.GetBaseType()) {
      case CJavaTypeSignature::Integer:
	code.Append(CJavaCodeAttribute::iaload, fLineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::laload, fLineNumber);
	break;
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::faload, fLineNumber);
	break;
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::daload, fLineNumber);
	break;
      case CJavaTypeSignature::Byte:
      case CJavaTypeSignature::Boolean:
	code.Append(CJavaCodeAttribute::baload, fLineNumber);
	break;
      case CJavaTypeSignature::Character:
	code.Append(CJavaCodeAttribute::caload, fLineNumber);
	break;
      case CJavaTypeSignature::Short:
	code.Append(CJavaCodeAttribute::saload, fLineNumber);
	break;
      default:
	assert(0);
      }
    }
  }
  return error;
}

//
//  Method name : StartStoreCode
//  Description : This method is used to generate the instructions needed at
//    the beginning of a store to a variable.  The storing code is split into
//    two parts this way because Java is a little inconsistent ... storing
//    into a variable puts code after the desired expression, but storing
//    into an array puts code before and after the value, so this has to
//    cover both cases.  Override as appropriate.
//
CCompileError*
CArrayIndex::StartStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool evaluateToo)
{
  CCompileError* error = 0;
  if (!IsLValue()) {
    error = MakeError("Invalid array assignment.");
  } else {
    unsigned short baseStack, indexStack;
    error = fBase->GenerateCode(code, context, baseStack);
    if (error == 0) {
      error = fIndex->GenerateCode(code, context, indexStack);
    }
    stackUsed = ::max(baseStack, (unsigned short)(indexStack + 1));
    if (evaluateToo) {
      code.Append(CJavaCodeAttribute::dup2, fLineNumber);
      CJavaTypeSignature baseType = fBase->GetType();
      baseType.SetArrayBounds(baseType.GetArrayBounds() - 1);
      if (baseType.IsReference()) {
	code.Append(CJavaCodeAttribute::aaload, fLineNumber);
      } else {
	switch(baseType.GetBaseType()) {
	case CJavaTypeSignature::Integer:
	  code.Append(CJavaCodeAttribute::iaload, fLineNumber);
	  break;
	case CJavaTypeSignature::LongInteger:
	  code.Append(CJavaCodeAttribute::laload, fLineNumber);
	  break;
	case CJavaTypeSignature::Float:
	  code.Append(CJavaCodeAttribute::faload, fLineNumber);
	  break;
	case CJavaTypeSignature::Double:
	  code.Append(CJavaCodeAttribute::daload, fLineNumber);
	  break;
	case CJavaTypeSignature::Byte:
	  code.Append(CJavaCodeAttribute::baload, fLineNumber);
	  break;
	case CJavaTypeSignature::Character:
	  code.Append(CJavaCodeAttribute::caload, fLineNumber);
	  break;
	case CJavaTypeSignature::Short:
	  code.Append(CJavaCodeAttribute::saload, fLineNumber);
	  break;
	default:
	  assert(0);
	}
      }
      stackUsed += 2;
    }
  }
  return error;
}

//
//  Method name : EndStoreCode
//  Description : This method is used to generate the instructions needed at
//    the end of a store to a variable.  The storing code is split into
//    two parts this way because Java is a little inconsistent ... storing
//    into a variable puts code after the desired expression, but storing
//    into an array puts code before and after the value, so this has to
//    cover both cases.
//
CCompileError*
CArrayIndex::EndStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  if (!isStatement) {
    stackUsed = GetType().GetWidth();
    if (stackUsed == 1) {
      code.Append(CJavaCodeAttribute::dup_x2, fLineNumber);
    } else {
      assert(stackUsed == 2);
      code.Append(CJavaCodeAttribute::dup2_x2, fLineNumber);
    }
  } else {
    stackUsed = 0;
  }
  if (GetType().IsReference()) {
    code.Append(CJavaCodeAttribute::aastore, fLineNumber);
  } else {
    CJavaTypeSignature baseType = GetType();
    baseType.SetArrayBounds(baseType.GetArrayBounds() - 1);
    switch (GetType().GetBaseType()) {
    case CJavaTypeSignature::Integer:
      code.Append(CJavaCodeAttribute::iastore, fLineNumber);
      break;
    case CJavaTypeSignature::LongInteger:
      code.Append(CJavaCodeAttribute::lastore, fLineNumber);
      break;
    case CJavaTypeSignature::Float:
      code.Append(CJavaCodeAttribute::fastore, fLineNumber);
      break;
    case CJavaTypeSignature::Double:
      code.Append(CJavaCodeAttribute::dastore, fLineNumber);
      break;
    case CJavaTypeSignature::Byte:
    case CJavaTypeSignature::Boolean:
      code.Append(CJavaCodeAttribute::bastore, fLineNumber);
      break;
    case CJavaTypeSignature::Character:
      code.Append(CJavaCodeAttribute::castore, fLineNumber);
      break;
    case CJavaTypeSignature::Short:
      code.Append(CJavaCodeAttribute::sastore, fLineNumber);
      break;
    default:
      assert(0);
    }
  }
  return 0;
}

//=============================== CMethodCall =================================

//
//  Method name : CMethodCall
//  Description : Constructs a method call expression node made up of
//    the target, method name and arguments to the method.
//
CMethodCall::CMethodCall(CExpression* adoptTarget,
			 const unicode_string& methodName,
			 ExpressionList* adoptArguments,
			 bool forceNonvirtual)
  : CExpression(CJavaTypeSignature::Void, false),
    fTarget(adoptTarget),
    fMethodName(methodName),
    fClassAlias(0),
    fMethodAlias(0),
    fForceNonvirtual(forceNonvirtual)
{
  if (adoptArguments != 0) {
    fArguments = *adoptArguments;
    delete adoptArguments;
  }
}

//
//  Method name : CMethodCall
//  Description : Copy constructor.
//
CMethodCall::CMethodCall(const CMethodCall& source)
  : CExpression(source),
    fTarget(0),
    fMethodName(source.fMethodName),
    fClassAlias(source.fClassAlias),
    fMethodAlias(source.fMethodAlias),
    fForceNonvirtual(source.fForceNonvirtual)
{
  if (source.fTarget != 0) {
    fTarget = source.fTarget->Clone();
  }
  for (ExpressionList::const_iterator i = source.fArguments.begin();
       !(i == source.fArguments.end()); ++i) {
    fArguments.push_back((*i)->Clone());
  }
}

//
//  Method name : ~CMethodCall
//  Description : Destructor
//
CMethodCall::~CMethodCall()
{
  delete fTarget;
  for (ExpressionList::iterator i = fArguments.begin();
       i != fArguments.end(); i++) {
    delete *i;
  }
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
CMethodCall::HandleEvaluateType(CCompileContext& context,
				CJavaTypeSignature& setType)
{
  CCompileError* error = 0;
  deque<CJavaTypeSignature> argumentTypes;
  for (ExpressionList::iterator i = fArguments.begin();
       error == 0 && i != fArguments.end(); i++) {
    CJavaTypeSignature argumentType;
    error = EvaluateType(*i, context, argumentType);
    argumentTypes.push_back(argumentType);
  }
  if (error == 0) {
    if (fTarget == 0) {
      unicode_string::size_type lastSlash = fMethodName.find_last_of('/');
      if (lastSlash == unicode_string::npos) {
	string* matchErrorString = 0;
	pair<const CJavaClassFile*, const CJavaMethodInfo*>* match =
	      context.GetCompiler().MatchMethod(fMethodName,
			  context.GetClass(), argumentTypes, matchErrorString);

	bool innerMatch = false;
	if (match == 0 && matchErrorString == 0) {
	  unicode_string outerName = context.GetClass().GetOuterName();
	  if (outerName.length() > 0) {
	    const CJavaClassFile* outer =
	      context.GetCompiler().LookupClass(outerName, context);
	    assert(outer != 0);
	    match = context.GetCompiler().MatchMethod(fMethodName, *outer,
					argumentTypes, matchErrorString);
	    innerMatch = true;
	  }
	}

	if (match != 0) {
	  fClassAlias = match->first;
	  fMethodAlias = match->second;
	  context.GetCompiler().WarnDeprecatedMethod(*fMethodAlias,
			     *fClassAlias, context, GetLineNumber());
	  setType = fMethodAlias->GetSignature().GetType();
	  if (fMethodAlias->GetModifiers().fStatic == 0) {
	    if (innerMatch) {
	      if (context.GetClass().GetAccessFlags().fStatic != 0) {
		error = MakeError("Cannot access non-static method: " +
				  ::UnicodeToUTF(fMethodName));
	      } else {
		fTarget = new CClassFieldExpression(
		      new CSpecialExpression(CSpecialExpression::kThis),
		      CCompiler::kOuterThisName);
		fTarget->InheritLineNumber(*this);
	      }
	    } else {
	      fTarget = new CSpecialExpression(CSpecialExpression::kThis);
	      fTarget->InheritLineNumber(*this);
	    }
	  }
	} else if (matchErrorString != 0) {
	  error = MakeError(*matchErrorString);
	  delete matchErrorString;
	} else {
	  string errorString("No matching method ");
	  errorString += ::UnicodeToString(fMethodName);
	  errorString += '(';
	  deque<CJavaTypeSignature>::iterator arg = argumentTypes.begin();
	  while (arg != argumentTypes.end()) {
	    errorString += ::UnicodeToString((*arg).Disassemble());
	    if (++arg != argumentTypes.end()) {
	      errorString += ", ";
	    }
	  }
	  errorString += ") found on class ";
	  errorString +=
	    ::UnicodeToString(context.GetClass().GetClassName());
	  error = MakeError(errorString);
	}
	delete match;
      } else {
	unicode_string prefix(fMethodName, 0, lastSlash);
	unicode_string shortName(fMethodName, lastSlash + 1);
	fMethodName = shortName;
	const CJavaClassFile* javaClass =
	  context.GetCompiler().LookupClass(prefix, context);
	if (javaClass != 0) {
	  string* matchErrorString = 0;
	  pair<const CJavaClassFile*, const CJavaMethodInfo*>* match =
	    context.GetCompiler().MatchMethod(fMethodName, *javaClass,
					      argumentTypes, matchErrorString);
	  if (match == 0) {
	    if (matchErrorString == 0) {
	      unicode_string errorString = ::StringToUnicode("Class ");
	      errorString += prefix;
	      errorString += ::StringToUnicode(" has no matching method ");
	      errorString += shortName;
	      errorString += (unicode_char)'(';
	      for (deque<CJavaTypeSignature>::iterator i =
		     argumentTypes.begin(); !(i == argumentTypes.end());) {
		errorString += (*i).Disassemble();
		if (!(++i == argumentTypes.end())) {
		  errorString += (unicode_char)',';
		  errorString += (unicode_char)' ';
		}
	      }
	      errorString += (unicode_char)')';
	      error = MakeError(errorString);
	    } else {
	      error = MakeError(*matchErrorString);
	      delete matchErrorString;
	    }
	  } else {
	    fClassAlias = match->first;
	    fMethodAlias = match->second;
	    context.GetCompiler().WarnDeprecatedMethod(*fMethodAlias,
				       *fClassAlias, context, GetLineNumber());
	    assert(fClassAlias != 0 && fMethodAlias != 0);
	    setType = fMethodAlias->GetSignature().GetType();
	    if (fMethodAlias->GetModifiers().fStatic == 0) {
	      string errorString = "Invalid use of non-static method ";
	      errorString += ::UnicodeToString(fMethodName);
	      error = MakeError(errorString);
	    }
	  }
	} else {
	  fTarget = new CClassFieldExpression(prefix);
	    fTarget->InheritLineNumber(*this);
	}
      }
    }
    CJavaTypeSignature targetType;
    if (error == 0 && fTarget != 0) {
      error = EvaluateType(fTarget, context, targetType);
    }
    if (error == 0 && fTarget != 0 && fMethodAlias == 0) {
      if (!targetType.IsReference()) {
	unicode_string errorString = ::StringToUnicode("Method ");
	errorString += fMethodName;
	errorString += ::StringToUnicode(" called in invalid type: ");
	errorString += targetType.Disassemble();
	error = MakeError(errorString);
      } else {
	const CJavaClassFile* javaClass = 0;
	if (targetType.GetArrayBounds() > 0) {
	  javaClass = context.GetCompiler().LookupClass(CCompiler::kObjectName,
							context);
	} else {
	  unicode_string className;
	  bool isClass = targetType.GetBaseClassName(className);
	  assert(isClass);
	  javaClass = context.GetCompiler().LookupClass(className, context);
	}
	if (javaClass == 0) {
	  unicode_string errorString = ::StringToUnicode("Method ");
	  errorString += fMethodName;
	  errorString += ::StringToUnicode(" called in invalid class: ");
	  errorString += targetType.Disassemble();
	  error = MakeError(errorString);
	} else {
	  string* matchErrorString = 0;
	  pair<const CJavaClassFile*, const CJavaMethodInfo*>* match =
	    context.GetCompiler().MatchMethod(fMethodName, *javaClass,
					      argumentTypes, matchErrorString);
	  if (match == 0) {
	    if (matchErrorString == 0) {
	      string errorString("No matching method ");
	      errorString += ::UnicodeToString(fMethodName);
	      errorString += '(';
	      deque<CJavaTypeSignature>::iterator arg = argumentTypes.begin();
	      while (arg != argumentTypes.end()) {
		errorString += ::UnicodeToString((*arg).Disassemble());
		if (++arg != argumentTypes.end()) {
		  errorString += ", ";
		}
	      }
	      errorString += ") found on class ";
	      errorString += ::UnicodeToString(targetType.Disassemble());
	      error = MakeError(errorString);
	    } else {
	      error = MakeError(*matchErrorString);
	      delete matchErrorString;
	    }
	  } else {
	    fClassAlias = match->first;
	    fMethodAlias = match->second;
	    context.GetCompiler().WarnDeprecatedMethod(*fMethodAlias,
				       *fClassAlias, context, GetLineNumber());
	    assert(fClassAlias != 0 && fMethodAlias != 0);
	    setType = fMethodAlias->GetSignature().GetType();
	    delete match;
	  }
	}
      }
    }
  }
  // XXXX fix:  stackUsed is being set based on the original argument sizes,
  // not the sizes after casting to match the formal parameters, so passing
  // integer 5 to a parameter taking a double is only going to count for 1
  // spot, not 2.
  if (error == 0 && fMethodAlias != 0) {
    assert(fMethodAlias->GetSignature().ParameterCount() == fArguments.size());
    deque<CJavaTypeSignature>::const_iterator parameter =
      fMethodAlias->GetSignature().ParametersBegin();
    for (ExpressionList::iterator argument = fArguments.begin();
	 error == 0 && argument != fArguments.end(); ++argument, ++parameter) {
      if (!context.GetCompiler().SameType((*argument)->GetType(),
					  *parameter)) {
	error = CCastExpression::CastPointer(*argument, *parameter, context);
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
CMethodCall::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(fMethodAlias != 0 && fClassAlias != 0);
  CCompileError* error = 0;
  if (fClassAlias != &context.GetClass() &&
      fMethodAlias->GetModifiers().fPublic == 0) {
    if (fMethodAlias->GetModifiers().fPrivate != 0) {
      error = MakeError("Invalid access to private method");
    } else if (!(fClassAlias->GetPackageName() ==
		 context.GetClass().GetPackageName())) {
      if (fMethodAlias->GetModifiers().fProtected == 0 ||
	  DYNAMIC_CAST(CSpecialExpression, fTarget) == 0 ||
	  !context.GetCompiler().DescendsFrom(context.GetClass(),
					      *fClassAlias)) {
	string errorString("Method ");
	errorString += ::UnicodeToString(fMethodName);
	errorString += " is not accessible to this class.";
	error = MakeError(errorString);
      }
    }
  }
  if (fMethodAlias->ThrowsExceptions()) {
    for (deque<unicode_string>::const_iterator i = fMethodAlias->ThrowsBegin();
       error == 0 && !(i == fMethodAlias->ThrowsEnd()); ++i) {
      if (!context.Throwable(CJavaTypeSignature(*i))) {
	unicode_string message = *i;
	message += ::UTFToUnicode(" must be caught or declared in this "
				  "method's 'throws' list.");
	error = MakeError(message);
      }
    }
  }
  bool forceNonvirtual = fForceNonvirtual;
  stackUsed = 0;
  unsigned short childStackSize, maxStackSize = 0;
  if (error == 0 && fTarget != 0) {
    error = fTarget->GenerateCode(code, context, childStackSize);
    maxStackSize = childStackSize;
    stackUsed += fTarget->GetType().GetWidth();
    CSpecialExpression* target = DYNAMIC_CAST(CSpecialExpression, fTarget);
    if (target != 0 && target->GetType() == CSpecialExpression::kSuper) {
      forceNonvirtual = true;
    }
  }
  unsigned char argumentCount = 0;
  if (fTarget != 0 && fMethodAlias->GetModifiers().fStatic != 0) {
    code.Append(CJavaCodeAttribute::pop, fLineNumber);
  }
  if (error == 0) {
    for (ExpressionList::iterator arg = fArguments.begin();
	 error == 0 && arg != fArguments.end(); ++arg) {
      error = (*arg)->GenerateCode(code, context, childStackSize);
      maxStackSize =
	::max(maxStackSize, (unsigned short)(childStackSize + stackUsed));
      stackUsed += (*arg)->GetType().GetWidth();
      argumentCount += (*arg)->GetType().GetWidth();
    }
  }
  if (error == 0) {
    // there are four possibilities ... static, final, interface, virtual
    if (fMethodAlias->GetModifiers().fStatic != 0) {
      assert(fClassAlias->GetAccessFlags().fInterface == 0);
      JavaConstantIndex index =
	context.GetClass().AddMethodConstant(fClassAlias->GetClassName(),
				  fMethodAlias->GetSignature());
      code.Append(CJavaCodeAttribute::invokestatic, index, fLineNumber);
    } else if (forceNonvirtual || fMethodAlias->GetModifiers().fFinal != 0) {
      assert(fClassAlias->GetAccessFlags().fInterface == 0);
      JavaConstantIndex index =
	context.GetClass().AddMethodConstant(fClassAlias->GetClassName(),
				  fMethodAlias->GetSignature());
      code.Append(CJavaCodeAttribute::invokenonvirtual, index, fLineNumber);
    } else if (fClassAlias->GetAccessFlags().fInterface != 0) {
      JavaConstantIndex index =
	context.GetClass().AddInterfaceConstant(fClassAlias->GetClassName(),
				     fMethodAlias->GetSignature());
      CCodeSequence::Instruction instruction;
      instruction.fOpcode = CJavaCodeAttribute::invokeinterface;
      instruction.fArguments.fIndexPair.index = index;
      instruction.fArguments.fIndexPair.u1 = argumentCount + 1;
      code.Append(instruction, fLineNumber);
    } else {
      assert(fClassAlias->GetAccessFlags().fInterface == 0);
      JavaConstantIndex index =
	context.GetClass().AddMethodConstant(fClassAlias->GetClassName(),
				  fMethodAlias->GetSignature());
      code.Append(CJavaCodeAttribute::invokevirtual, index, fLineNumber);
    }
    if (isStatement && GetType() != CJavaTypeSignature::kVoid) {
      if (GetType().GetWidth() == 1) {
	code.Append(CJavaCodeAttribute::pop, fLineNumber);
      } else {
	assert(GetType().GetWidth() == 2);
	code.Append(CJavaCodeAttribute::pop2, fLineNumber);
      }
    }
  }
  stackUsed = ::max(stackUsed, maxStackSize);
  if (GetType() != CJavaTypeSignature::kVoid) {
    stackUsed = ::max(stackUsed, (unsigned short)GetType().GetWidth());
  }
  return error;
}

//=========================== CSpecialExpression ==============================

//
//  Method name : CSpecialExpression
//  Description : Constructs a dopey hard-coded special expression value.
//
CSpecialExpression::CSpecialExpression(CSpecialExpression::Type type)
  : CExpression(CJavaTypeSignature::Void, false),
    fType(type)
{
}

//
//  Method name : CSpecialExpression
//  Description : Copy constructor.
//
CSpecialExpression::CSpecialExpression(const CSpecialExpression& source)
  : CExpression(source),
    fType(source.fType)
{
}

//
//  Method name : ~CSpecialExpression
//  Description : Destructor
//
CSpecialExpression::~CSpecialExpression()
{
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
CSpecialExpression::HandleEvaluateType(CCompileContext& context,
				       CJavaTypeSignature& setType)
{
  CCompileError* error = 0;
  switch (fType) {
  case kThis:
    setType = CJavaTypeSignature(context.GetClass().GetClassName());
    break;
  case kSuper:
    if (context.GetClass().GetSuperclassName().size() == 0) {  // java.lang.Object
      error = MakeError("Invalid use of 'super' in Object.");
    }
    setType = CJavaTypeSignature(context.GetClass().GetSuperclassName());
    break;
  default:
    assert(0);
    break;
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
CSpecialExpression::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(!isStatement);
  CCompileError* error = 0;
  switch (fType) {
  case kThis:
    if (context.GetMethod().GetModifiers().fStatic != 0) {
      error = MakeError("Invalid use of 'this' in a static function");
    }
    code.Append(CJavaCodeAttribute::aload_0, fLineNumber);
    break;
  case kSuper:
    if (context.GetMethod().GetModifiers().fStatic != 0) {
      error = MakeError("Invalid use of 'super' in a static function");
    }
    code.Append(CJavaCodeAttribute::aload_0, fLineNumber);
    break;
  default:
    assert(0);
  }
  stackUsed = 1;
  return error;
}

//============================== CInstanceof ==================================

//
//  Method name : CInstanceof
//  Description : Constructs an expression to model the instanceof operator
//
CInstanceof::CInstanceof(CExpression* adoptBase,
			 const CJavaTypeSignature& toType)
  : CExpression(CJavaTypeSignature::Boolean, false),
    fBase(adoptBase),
    fToType(toType)
{
}

//
//  Method name : CInstanceof
//  Description : Copy constructor.
//
CInstanceof::CInstanceof(const CInstanceof& source)
  : CExpression(source), fBase(0), fToType(source.fToType)
{
  if (source.fBase != 0) {
    fBase = source.fBase->Clone();
  }
}

//
//  Method name : ~CInstanceof
//  Description : Destructor
//
CInstanceof::~CInstanceof()
{
  delete fBase;
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
CInstanceof::GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool isStatement)
{
  assert(!isStatement);
  CJavaTypeSignature baseType;
  CCompileError* error = EvaluateType(fBase, context, baseType);
  if (error == 0) {
    error = fBase->GenerateCode(code, context, stackUsed);
  }
  if (error == 0 && !fToType.IsReference()) {
    error = MakeError("instanceof requires a reference type.");
  }
  fToType = context.GetCompiler().FixType(fToType);
  //  if (error == 0) {
  //    error = context.GetCompiler().CheckType(fToType);
  //  }
  //  if (error == 0) {
  //    error = context.GetCompiler().CheckType(baseType);
  //  }
  if (error == 0) {
    if (baseType.GetArrayBounds() > 0) {
      error = MakeError("Invalid instanceof check against array type.");
    } else if (!fToType.IsReference()) {
      unicode_string errorString = fToType.Disassemble();
      errorString += ::StringToUnicode(" is not a reference type.");
      error = MakeError(errorString);
    } else if (!baseType.IsReference()) {
      unicode_string errorString = baseType.Disassemble();
      errorString += ::StringToUnicode(" is not a reference type.");
      error = MakeError(errorString);
    } else if (!context.GetCompiler().CastableType(baseType, fToType)) {
      unicode_string errorString = fToType.Disassemble();
      errorString += ::StringToUnicode(" is not an assignable subtype of ");
      errorString += baseType.Disassemble();
      error = MakeError(errorString);
    } else {
      unicode_string className =
	context.GetCompiler().NameClassConstant(fToType);
      JavaConstantIndex index =
	context.GetClass().AddClassConstant(className);
      code.Append(CJavaCodeAttribute::instanceof, index, fLineNumber);
    }
  }
  stackUsed = 1;
  return error;
}
