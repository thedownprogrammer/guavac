// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: Expression.h,v 1.14 1997/11/10 00:48:06 geppetto Exp $
#ifndef _Expression_h
#define _Expression_h
#pragma interface

#include "dynamic_cast.h"
#include "JavaTypeSignature.h"
#include "parser_decls.h"
#include "JavaConstant.h"
#include "JavaFieldInfo.h"
class CJavaFieldSignature;
class CCompiler;
class CJavaClassFile;
class CCompileError;
class CJavaMethodInfo;
class CCompileContext;

//
//  Class name : CExpression
//  Description : An expression is used during compilation to represent a
//    value expression in Java.  CExpression is an abstract base class that
//    provides a common interface to the various types of expressions used
//    in the language.
//
class CExpression {
  DynamicCastDeclarations;
public:
  virtual ~CExpression();
  const CJavaTypeSignature& GetType() const { return fType; }
  bool IsLValue() const { return fLValue; }
  void SetLValue(bool isLValue) { fLValue = isLValue; }
  virtual bool IsLiteral() const;
  virtual CExpression* Clone() const = 0;

  static CCompileError* EvaluateType(CExpression*& expressionPointer,
			 CCompileContext& context, CJavaTypeSignature& setType,
			 bool asLValue = false);
  virtual CCompileError* CreateConstantExpression(CExpression*& result,
			  CCompileContext& context) const;
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
  virtual CCompileError* StartStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool evaluateToo = true);
  virtual CCompileError* EndStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
  void InheritLineNumber(const CExpression& from);
  unsigned long GetLineNumber() const { return fLineNumber; }
protected:
  CExpression(const CJavaTypeSignature& type, bool isLValue = false);
  CExpression(const CExpression& source);
  virtual CCompileError* HandleEvaluateType(CCompileContext& context,
					    CJavaTypeSignature& setType);
  
  CCompileError* MakeError(const unicode_string& message) const;
  CCompileError* MakeError(const string& message) const;

  static bool UnaryPromote(CExpression*& promotePointer,
			   CCompileContext& context);
  static bool BinaryPromote(CExpression*& left, CExpression*& right,
			   CCompileContext& context);
  void SetType(const CJavaTypeSignature& type);
  CJavaTypeSignature fType;
  bool fLValue;
  unsigned long fLineNumber;
};

//
//  Class name : COrdinalLiteral
//  Description : Represents a literal of an ordinal type.  This includes
//     integers, characters, boolean, and long values.  Bytes and shorts
//     aren't included since they can't be expressed as a literal in a Java
//     program.
//
class COrdinalLiteral : public CExpression {
  DynamicCastDeclarations;
public:
  COrdinalLiteral(unsigned long integerLiteral,
		  const CJavaTypeSignature& type);
  COrdinalLiteral(unsigned long integerLiteral);
  COrdinalLiteral(long integerLiteral);
  COrdinalLiteral(unsigned short characterLiteral, bool isChar = true);
  COrdinalLiteral(signed short shortLiteral);
  COrdinalLiteral(unsigned char byteLiteral);
  COrdinalLiteral(signed char byteLiteral);
  COrdinalLiteral(bool booleanLiteral);
  COrdinalLiteral(unsigned long long longLiteral);
  COrdinalLiteral(long long longLiteral);
  COrdinalLiteral(const COrdinalLiteral& source);
  virtual ~COrdinalLiteral();
  virtual CExpression* Clone() const { return new COrdinalLiteral(*this); }

  virtual bool IsLiteral() const;
  unsigned long GetInteger() const { return fOrdinalLiteral; }
  bool GetBoolean() const { return fOrdinalLiteral != 0; }
  unsigned short GetCharacter() const { return fOrdinalLiteral; }
  unsigned short GetShort() const { return fOrdinalLiteral; }
  unsigned char GetByte() const { return fOrdinalLiteral; }
  unsigned long long GetLong() const { return fOrdinalLiteral; }

  virtual CCompileError* CreateConstantExpression(CExpression*& result,
			  CCompileContext& context) const;
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
  JavaConstantIndex AddToConstantPool(CJavaClassFile& inClass) const;
private:
  unsigned long long fOrdinalLiteral;
};

//
//  Class name : CFloatLiteral
//  Description : Represents a literal of an floating-point type.
//     This includes float and double values.
//
class CFloatLiteral : public CExpression {
  DynamicCastDeclarations;
public:
  CFloatLiteral(float floatLiteral);
  CFloatLiteral(double doubleLiteral);
  CFloatLiteral(const CFloatLiteral& source);
  virtual ~CFloatLiteral();
  virtual CExpression* Clone() const { return new CFloatLiteral(*this); }
  
  virtual bool IsLiteral() const;
  float GetFloat() const { return fValue; }
  double GetDouble() const { return fValue; }
  virtual CCompileError* CreateConstantExpression(CExpression*& result,
			  CCompileContext& context) const;
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
  JavaConstantIndex AddToConstantPool(CJavaClassFile& inClass) const;
private:
  double fValue;
};


//
//  Class name : CStringLiteral
//  Description : Represents a string literal used in a Java program.
//
class CStringLiteral : public CExpression {
  DynamicCastDeclarations;
public:
  CStringLiteral(const unicode_string& value);
  CStringLiteral(const CStringLiteral& source);
  virtual ~CStringLiteral();
  virtual bool IsLiteral() const;
  virtual CExpression* Clone() const { return new CStringLiteral(*this); }
  unicode_string GetString() const { return fValue; }
  virtual CCompileError* CreateConstantExpression(CExpression*& result,
			  CCompileContext& context) const;
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
  JavaConstantIndex AddToConstantPool(CJavaClassFile& inClass) const;
private:
  unicode_string fValue;
};


//
//  Class name : CNull
//  Description : Represents a reference expression with a value of null.
//
class CNull : public CExpression {
  DynamicCastDeclarations;
public:
  CNull();
  virtual ~CNull();
  virtual CExpression* Clone() const { return new CNull(); }
  virtual CCompileError* CreateConstantExpression(CExpression*& result,
			  CCompileContext& context) const;
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
};

//
//  Class name : CCastExpression
//  Description : Represents an expression casting one value to a different
//    type.
//
class CCastExpression : public CExpression {
  DynamicCastDeclarations;
public:
  CCastExpression(const CJavaTypeSignature& toType, CExpression* adoptCastee);
  CCastExpression(const CCastExpression& source);
  virtual ~CCastExpression();
  virtual CExpression* Clone() const { return new CCastExpression(*this); }
  virtual CCompileError* CreateConstantExpression(CExpression*& result,
			  CCompileContext& context) const;
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
  static CCompileError* CastPointer(CExpression*& expressionPointer,
				const CJavaTypeSignature& toType,
				CCompileContext& context);
  static CCompileError* ImplicitCastPointer(CExpression*& expressionPointer,
					    const CJavaTypeSignature& toType,
					    CCompileContext& context);
protected:
  virtual CCompileError* HandleEvaluateType(CCompileContext& context,
					    CJavaTypeSignature& setType);
private:
  CCompileError* MakeBadCastMessage(const CJavaTypeSignature& castFrom) const;
  CJavaTypeSignature fCastToType;
  CExpression* fCastee;
};


//
//  Class name : CNewArray
//  Description : Represents an expression dynamically creating a new array
//    of values using the Java 'new' keyword.
//
class CNewArray : public CExpression {
  DynamicCastDeclarations;
public:
  CNewArray(const CJavaTypeSignature& type, ExpressionList* adoptArraySizes);
  CNewArray(const CNewArray& source);
  virtual ~CNewArray();
  virtual CExpression* Clone() const { return new CNewArray(*this); }
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
private:
  ExpressionList* fArraySizes;
};


//
//  Class name : CNewObject
//  Description : Represents an expression dynamically creating a new instance
//    of a class using the Java 'new' keyword.
//
class CNewObject : public CExpression {
  DynamicCastDeclarations;
public:
  CNewObject(const unicode_string& className, ExpressionList* adoptArgs = 0,
	     CExpression* explicitThis = 0);
  CNewObject(const CNewObject& source);
  virtual ~CNewObject();
  virtual CExpression* Clone() const { return new CNewObject(*this); }
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
private:
  ExpressionList* fConstructorArguments;
  CExpression* fExplicitThis;
};

//
//  Class name : CTrinaryExpression
//  Description : Represents an expression composed of three parts.  Currently,
//    the only instance of this in Java is the  A ? B : C  syntax.
//
class CTrinaryExpression : public CExpression {
  DynamicCastDeclarations;
public:
  CTrinaryExpression(CExpression* leftExpression,
		     CExpression* middleExpression,
		     CExpression* rightExpression);
  CTrinaryExpression(const CTrinaryExpression& source);
  virtual ~CTrinaryExpression();
  virtual CExpression* Clone() const { return new CTrinaryExpression(*this); }
  virtual CCompileError* CreateConstantExpression(CExpression*& result,
			  CCompileContext& context) const;
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
protected:
  virtual CCompileError* HandleEvaluateType(CCompileContext& context,
					    CJavaTypeSignature& setType);
private:
  bool UnifyNumeric(CExpression*& first, CExpression*& second,
		    CCompileContext& context, CJavaTypeSignature& setType);

  CExpression* fLeft;
  CExpression* fMiddle;
  CExpression* fRight;
};

//
//  Class name : CUnaryExpression
//  Description : Represents an expression composed of a unary operator
//    applied to another expression.  Stores the child expression, the
//    operator, and some sense of whether it was prefix or postfix.
//
class CUnaryExpression : public CExpression {
  DynamicCastDeclarations;
public:
  typedef enum { kPrefix, kPostfix } Order;
  CUnaryExpression(CExpression* childExpression, unsigned short unaryOperator,
		   Order side, bool isLValue);
  CUnaryExpression(const CUnaryExpression& source);
  virtual ~CUnaryExpression();
  virtual CExpression* Clone() const { return new CUnaryExpression(*this); }
  virtual CCompileError* CreateConstantExpression(CExpression*& result,
			  CCompileContext& context) const;
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
protected:
  virtual CCompileError* HandleEvaluateType(CCompileContext& context,
					    CJavaTypeSignature& setType);
private:
  CExpression* fChild;
  unsigned short fOperator;
  Order fSide;
};


//
//  Class name : CLocalVariableExpression
//  Description : A variable expression is used to mark the usage of a local
//    variable in a program.  These are distinct from non-local variables
//    (class fields) in that they can be uniquely bound during the first pass
//    of compilation.  (i.e. local variables must be declared before use.)
//
class CLocalVariableExpression : public CExpression {
  DynamicCastDeclarations;
public:
  CLocalVariableExpression(const CJavaFieldSignature& name,
			   unsigned short index,
			   bool final);
  CLocalVariableExpression(const CLocalVariableExpression& source);
  virtual ~CLocalVariableExpression();
  virtual CExpression* Clone() const
     { return new CLocalVariableExpression(*this); }
  unsigned short GetVariableIndex(const CCompileContext& context) const;
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
  virtual CCompileError* StartStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool evaluateToo = true);
  virtual CCompileError* EndStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
private:
  unicode_string fName;
  unsigned short fLocalVariableIndex;
};


//
//  Class name : CClassFieldExpression
//  Description : This type of expression represents the use of a variable
//    in a function which isn't declared as part of that function.  The
//    first pass of the compiler just assumes for the time being that it
//    must be a valid field, and doesn't even bother looking it up, since
//    use-before-set is legal in Java.
//    The semantic analyzer checks it later for validity and figures out
//    whether it is a static field like FooClass.x or whether it is a
//    member field off of 'this.'  At that point, it constructs an expression
//    of the _real_ type and holds it internally.
//
class CClassFieldExpression : public CExpression {
  DynamicCastDeclarations;
public:
  CClassFieldExpression(const unicode_string& label);
  CClassFieldExpression(CExpression* adoptBase, const unicode_string& label);
  CClassFieldExpression(const CClassFieldExpression& source);
  virtual ~CClassFieldExpression();
  virtual CExpression* Clone() const
     { return new CClassFieldExpression(*this); }
  
  bool IsStatic() const;
  bool IsArrayLength() const;
  unicode_string GetFieldString() const { return fLabel; }

  virtual CCompileError* CreateConstantExpression(CExpression*& result,
			  CCompileContext& context) const;
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
  virtual CCompileError* StartStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool evaluateToo = true);
  virtual CCompileError* EndStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
protected:
  virtual CCompileError* HandleEvaluateType(CCompileContext& context,
					    CJavaTypeSignature& setType);
  CCompileError* EvaluateBaseType(CCompileContext& context,
				  CJavaTypeSignature& setType);
private:
  unicode_string fLabel;

  CJavaFieldInfo fFieldInfo;
  const CJavaClassFile* fClassAlias;
  CExpression* fBase;
};


//
//  Class name : COuterLocalExpression
//  Description : This type of expression is caused by an inner class making
//    reference to a local variable in the outer class's block.  This is a
//    little bit of a mess because this expression is constructed before
//    we get a chance to find a conflicting field on the inner class, so
//    this may end up having to revise itself.
//
class COuterLocalExpression : public CExpression {
  DynamicCastDeclarations;
public:
  COuterLocalExpression(const CVariableDeclaration& declaration,
			unsigned short localVariableIndex);
  COuterLocalExpression(const COuterLocalExpression& source);
  virtual ~COuterLocalExpression();
  virtual CExpression* Clone() const
     { return new COuterLocalExpression(*this); }

  unicode_string GetLabel() const { return fDeclaration.GetName(); }
  CVariableDeclaration GetLocalDeclaration() const { return fDeclaration; }
  unsigned short GetLocalVariableIndex() const { return fLocalVariableIndex; }
  bool IsLocalFinal() const { return fDeclaration.IsFinal(); }

  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
protected:
  virtual CCompileError* HandleEvaluateType(CCompileContext& context,
					    CJavaTypeSignature& setType);
private:
  CVariableDeclaration fDeclaration;
  unsigned short fLocalVariableIndex;
  CExpression* fSurrogate;
};


//
//  Class name : CArrayInitializer
//  Description : This type of expression is only used in initialization of
//    arrays using a special hokey syntax.  It's basically a wrapper for
//    a list of expressions that should all match the type of the array.
//
class CArrayInitializer : public CExpression {
  DynamicCastDeclarations;
public:
  CArrayInitializer(ExpressionList* adoptExpressions = 0);
  CArrayInitializer(const CArrayInitializer& source);
  virtual ~CArrayInitializer();
  virtual CExpression* Clone() const { return new CArrayInitializer(*this); }
  void SetArrayType(const CJavaTypeSignature& type);
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
private:
  ExpressionList fExpressions;
};

//
//  Class name : CArrayIndex
//  Description : This represents an expression made using the array element
//    notation to extract an entry from an array value.
//
class CArrayIndex : public CExpression {
  DynamicCastDeclarations;
public:
  CArrayIndex(CExpression* baseExpression, CExpression* index);
  CArrayIndex(const CArrayIndex& source);
  virtual ~CArrayIndex();
  virtual CExpression* Clone() const { return new CArrayIndex(*this); }
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
  virtual CCompileError* StartStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
		   bool evaluateToo = true);
  virtual CCompileError* EndStoreCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
protected:
  virtual CCompileError* HandleEvaluateType(CCompileContext& context,
					    CJavaTypeSignature& setType);
private:
  CExpression* fBase;
  CExpression* fIndex;
};

//
//  Class name : CMethodCall
//  Description : This expression represents the invocation of a method
//    upon an object.  This combines the target object, the name of the
//    method, and a list of the arguments.
//
class CMethodCall : public CExpression {
  DynamicCastDeclarations;
public:
  CMethodCall(CExpression* adoptTarget, const unicode_string& methodName,
	     ExpressionList* adoptArguments = 0, bool forceNonvirtual = false);
  CMethodCall(const CMethodCall& source);
  virtual ~CMethodCall();
  virtual CExpression* Clone() const { return new CMethodCall(*this); }
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
protected:
  virtual CCompileError* HandleEvaluateType(CCompileContext& context,
					    CJavaTypeSignature& setType);
private:
  CExpression* fTarget;
  unicode_string fMethodName;
  ExpressionList fArguments;
  const CJavaClassFile* fClassAlias;
  const CJavaMethodInfo* fMethodAlias;
  bool fForceNonvirtual;
};

//
//  Class name : CSpecialExpression
//  Description : This is kind of a catch-all for expressions that require
//    special handling, and are generally inconsistent throughout.
//
class CSpecialExpression : public CExpression {
  DynamicCastDeclarations;
public:
  typedef enum { kThis, kSuper } Type;
  CSpecialExpression(Type expressionType);
  CSpecialExpression(const CSpecialExpression& source);
  virtual ~CSpecialExpression();
  virtual CExpression* Clone() const { return new CSpecialExpression(*this); }
  Type GetType() const { return fType; }

  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
protected:
  virtual CCompileError* HandleEvaluateType(CCompileContext& context,
					    CJavaTypeSignature& setType);
private:
  Type fType;
};

//
//  Class name : CInstanceof
//  Description : This is used for the expression  (foo instanceof String)
//    which checks if an object descends from a given base.
//
class CInstanceof : public CExpression {
  DynamicCastDeclarations;
public:
  CInstanceof(CExpression* adoptBase, const CJavaTypeSignature& toType);
  CInstanceof(const CInstanceof& source);
  virtual ~CInstanceof();
  virtual CExpression* Clone() const { return new CInstanceof(*this); }
  virtual CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed,
	           bool isStatement = false);
private:
  CExpression* fBase;
  CJavaTypeSignature fToType;
};

#endif
