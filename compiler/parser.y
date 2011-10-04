%{
// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: parser.y,v 1.18 1997/11/10 00:48:06 geppetto Exp $
#include <map>
#include <stream.h>
#include <cstdlib>
#include <deque>
#include "unicode_string.h"
#include "Compiler.h"
#include "parser_decls.h"
#include "JavaAccessFlags.h"
#include "JavaTypeSignature.h"
#include "JavaFieldSignature.h"
#include "Expression.h"
#include "VariableDeclaration.h"
#include "Statement.h"
#include "IntermediateClass.h"
#include "BinaryExpression.h"
#include "LocalVariableRecord.h"

CCompiler* gCurrentCompiler = 0;
const unicode_char kSymbolDivider = (unicode_char)'/';

CExpression* InterpretSymbolExpression(const unicode_string& symbol);

#ifndef NDEBUG
#ifndef YYDEBUG
#define YYDEBUG 1
#endif
#endif

%}

%union {
  unicode_string* text;
  deque<unicode_string>* stringList;
  unsigned long longNumber;   // also used for a single character, etc.
  unsigned long long doubleLong;
  float floatNumber;
  double doubleFloat;
  CJavaAccessFlags* modifiers;
  CJavaTypeSignature* typeSignature;
  CVariableDeclaration* fieldSignature;
  deque<CVariableDeclaration>* fieldList;
  CExpression* expression;
  ExpressionList* expressionList;
  deque<CVariableDeclaration*>* variableList;
  CStatement* statement;
  CCompoundStatement* compoundStatement;
  StatementList* statementList;
  deque<CCatchClause*>* catchList;
  CCatchClause* catchClause;
}

%token ERROR   /* used by the lexer. */

%token DOTNEW

%token ABSTRACT BOOLEAN BREAK BYTE CASE CATCH CHAR CLASS CONTINUE DEFAULT 
%token DO DOUBLE ELSE EXTENDS FALSE_TOKEN FINAL FINALLY FLOAT FOR IF
%token IMPLEMENTS IMPORT INSTANCEOF INT INTERFACE LONG NATIVE NULL_TOKEN
%token PACKAGE PRIVATE PROTECTED PUBLIC RETURN SHORT STATIC SUPER SWITCH
%token SYNCHRONIZED THIS THROW THROWS TRANSIENT VOLATILE TRUE_TOKEN TRY
%token VOID WHILE 

%token SHIFT_RIGHT_EQUALS FILL_SHIFT_RIGHT_EQUALS SHIFT_LEFT_EQUALS ADD_EQUALS
%token SUB_EQUALS MUL_EQUALS DIV_EQUALS MOD_EQUALS AND_EQUALS XOR_EQUALS
%token OR_EQUALS OR AND EQUAL_COMPARE NOT_EQUAL LTEQ GTEQ INSTANCEOF
%token BITSHIFT_RIGHT FILL_SHIFT_RIGHT SHIFT_LEFT INCR DECR NEW

%token <longNumber> INT_LITERAL CHARACTER_LITERAL
%token <doubleLong> LONG_LITERAL 
%token <floatNumber> FLOAT_LITERAL
%token <doubleFloat> DOUBLE_LITERAL
%token <text> SYMBOL STRING_LITERAL

%type <text> Name Identifier QualifiedName SimpleName
%type <text> ClassOrInterfaceType ClassType InterfaceType
%type <text> Super OptSuper
%type <stringList> InterfaceTypeList ClassTypeList Interfaces OptInterfaces
%type <stringList> OptExtendsInterfaces ExtendsInterfaces
%type <expression> Literal PrimaryNoNewArray
%type <typeSignature> ReferenceType Type PrimitiveType
%type <typeSignature> IntegralType FloatingPointType NumericType ArrayType
%type <modifiers> OptModifiers Modifiers Modifier
%type <expression> Expression VariableInitializer ArrayInitializer
%type <expression> OptVariableInitializers
%type <expressionList> VariableInitializers ArgumentList OptArgumentList
%type <compoundStatement> MethodBody Block
%type <fieldList> OptFormalParameterList FormalParameterList
%type <fieldSignature> FormalParameter
%type <stringList> OptThrows Throws
%type <statementList> ConstructorBody OptBlockStatements BlockStatements
%type <statement> ExplicitConstructorInvocation BlockStatement
%type <statement> LocalVariableDeclarationStatement Statement
%type <statement> StatementWithoutTrailingSubstatement LabeledStatement
%type <statement> IfThenStatement IfThenElseStatement WhileStatement
%type <statement> ForStatement StatementNoShortIf LabeledStatementNoShortIf
%type <statement> IfThenElseStatementNoShortIf WhileStatementNoShortIf
%type <statement> ForStatementNoShortIf EmptyStatement ExpressionStatement
%type <statement> SwitchStatement DoStatement BreakStatement
%type <statement> ContinueStatement ReturnStatement SynchronizedStatement
%type <statement> ThrowStatement TryStatement
%type <expression> StatementExpression Assignment PreIncrementExpression
%type <expression> PreDecrementExpression PostIncrementExpression
%type <expression> PostDecrementExpression MethodInvocation
%type <expression> ClassInstanceCreationExpression ConstantExpression
%type <compoundStatement> SwitchBlock Finally
%type <statementList> OptSwitchBlockStatements SwitchBlockStatements
%type <statement> SwitchBlockStatement
%type <statement> OptForInit ForInit
%type <expressionList> StatementExpressionList OptForUpdate ForUpdate
%type <expressionList> DimExprs
%type <expression> OptExpression DimExpr
%type <catchList> Catches OptCatches
%type <catchClause> CatchClause
%type <expression> Primary ArrayCreationExpression FieldAccess ArrayAccess
%type <expression> PostfixExpression UnaryExpression CastExpression
%type <expression> UnaryExpressionNotPlusMinus MultiplicativeExpression
%type <expression> AdditiveExpression ShiftExpression RelationalExpression
%type <expression> EqualityExpression AndExpression ExclusiveOrExpression
%type <expression> InclusiveOrExpression ConditionalAndExpression
%type <expression> ConditionalOrExpression ConditionalExpression
%type <expression> AssignmentExpression LeftHandSide
%type <longNumber> OptDims Dims AssignmentOperator
%type <expression> OptVariableInitializer
%type <variableList> PartialVariable VariableDeclaration
%type <variableList> LocalVariableDeclaration
%type <variableList> FinalPartialVariable FinalVariableDeclaration

%%

Goal:
  CompilationUnit

Literal:
  INT_LITERAL
    { $$ = new COrdinalLiteral((unsigned long)$1); }
  | STRING_LITERAL
    {
      $$ = new CStringLiteral(*($1));
      delete $1;
    }
  | CHARACTER_LITERAL
    { $$ = new COrdinalLiteral((unsigned short)$1); }
  | LONG_LITERAL
    { $$ = new COrdinalLiteral($1); }
  | FLOAT_LITERAL
    { $$ = new CFloatLiteral($1); }
  | DOUBLE_LITERAL
    { $$ = new CFloatLiteral($1); }
  | TRUE_TOKEN
    { $$ = new COrdinalLiteral(true); }
  | FALSE_TOKEN
    { $$ = new COrdinalLiteral(false); }
  | NULL_TOKEN
    { $$ = new CNull(); }
  ;

Type: 
  PrimitiveType
  | ReferenceType
  ;

PrimitiveType:
  BOOLEAN
    { $$ = new CJavaTypeSignature(CJavaTypeSignature::Boolean); }
  | NumericType
  ;

NumericType: 
  IntegralType
  | FloatingPointType
  ;

IntegralType: 
  BYTE
    { $$ = new CJavaTypeSignature(CJavaTypeSignature::Byte); }
  | SHORT
    { $$ = new CJavaTypeSignature(CJavaTypeSignature::Short); }
  | INT
    { $$ = new CJavaTypeSignature(CJavaTypeSignature::Integer); }
  | LONG
    { $$ = new CJavaTypeSignature(CJavaTypeSignature::LongInteger); }
  | CHAR
    { $$ = new CJavaTypeSignature(CJavaTypeSignature::Character); }
  ;

FloatingPointType: 
  FLOAT
    { $$ = new CJavaTypeSignature(CJavaTypeSignature::Float); }
  | DOUBLE
    { $$ = new CJavaTypeSignature(CJavaTypeSignature::Double); }
  ;

ReferenceType:
  ClassOrInterfaceType
    {
      $$ = new CJavaTypeSignature(*($1));
      delete $1;
    }
  | ArrayType
  ;

ClassOrInterfaceType:
  Name
  ;

ClassType:
  ClassOrInterfaceType
  ;

InterfaceType:
  ClassOrInterfaceType
  ;

ArrayType:
  PrimitiveType '[' ']'
    {
      $$ = $1;
      $$->SetArrayBounds(1);
    }
  | Name '[' ']'
    {
      $$ = new CJavaTypeSignature(*($1), 1);
      delete $1;
    }
  | ArrayType '[' ']'
    {
      $$ = $1;
      $$->SetArrayBounds($$->GetArrayBounds() + 1);
    }
  ;

Name:
  SimpleName
  | QualifiedName
  ;

SimpleName:
  Identifier
  ;

QualifiedName:
  Name '.' Identifier
    {
      $$ = $1;
      *($$) += kSymbolDivider;
      *($$) += *($3);
      delete $3;
    }
  ;

Identifier:
  SYMBOL
  ;

CompilationUnit:
  OptPackageDeclaration OptImportDeclarations OptTypeDeclarations
  ;

OptImportDeclarations:
  | OptImportDeclarations ImportDeclaration
  ;

OptTypeDeclarations:
  | OptTypeDeclarations TypeDeclaration
    { yydeprecated = false; }
  ;

OptPackageDeclaration:
    {
      gCurrentCompiler->NoPackage();
    }
  | PACKAGE Name ';'
    {
      gCurrentCompiler->SetPackage(*($2));
      delete $2;
    }
  ;

ImportDeclaration:
  SingleTypeImportDeclaration
  | TypeImportOnDemandDeclaration
  ;

SingleTypeImportDeclaration:
  IMPORT Name ';'
    {
      gCurrentCompiler->AddClassImport(*($2));
      delete $2;
    }
  ;

TypeImportOnDemandDeclaration:
  IMPORT Name '.' '*' ';'
    {
      gCurrentCompiler->AddPackageImport(*($2));
      delete $2;
    }
  ;

TypeDeclaration:
  ClassDeclaration
  | InterfaceDeclaration
  ;

OptModifiers:
    { $$ = new CJavaAccessFlags(); }
  | Modifiers
  ;

Modifiers:
  Modifier
  | Modifiers Modifier
    {
      $$ = $1;
      $$->SetFlags($1->GetJavaFlags() | $2->GetJavaFlags());
      delete $2;
    }
  ;

Modifier:
  PUBLIC
    {
      $$ = new CJavaAccessFlags();
      $$->fPublic = 1;
    }
  | PROTECTED
    {
      $$ = new CJavaAccessFlags();
      $$->fProtected = 1;
    }
  | PRIVATE
    {
      $$ = new CJavaAccessFlags();
      $$->fPrivate = 1;
    }
  | STATIC
    {
      $$ = new CJavaAccessFlags();
      $$->fStatic = 1;
    }
  | ABSTRACT
    {
      $$ = new CJavaAccessFlags();
      $$->fAbstract = 1;
    }
  | FINAL
    {
      $$ = new CJavaAccessFlags();
      $$->fFinal = 1;
    }
  | NATIVE
    {
      $$ = new CJavaAccessFlags();
      $$->fNative = 1;
    }
  | SYNCHRONIZED
    {
      $$ = new CJavaAccessFlags();
      $$->fSynchronized = 1;
    }
  | TRANSIENT
    {
      $$ = new CJavaAccessFlags();
      $$->fTransient = 1;
    }
  | VOLATILE
    {
      $$ = new CJavaAccessFlags();
      $$->fVolatile = 1;
    }
  ;

ClassDeclaration:
  OptModifiers CLASS Identifier OptSuper OptInterfaces
    {
      CJavaAccessFlags usedFlags(*$1);
      usedFlags.fPublic = usedFlags.fFinal =
	usedFlags.fAbstract = usedFlags.fStatic = 0;
      if (usedFlags.Count() > 0) {
	string errorMessage("Invalid modifier use in class declaration: ");
	errorMessage += usedFlags.FlagNames();
	yyerror(errorMessage.c_str());
	YYERROR;
      }
      gCurrentCompiler->StartClass(*($3), $1, $4, $5, yydeprecated);
      delete $3;
    }
  ClassBody
    { gCurrentCompiler->EndClass(); }
  ;

OptSuper:
    { $$ = 0; }
  | Super
  ;

Super:
  EXTENDS ClassType
    { $$ = $2; }
  ;

OptInterfaces:
    { $$ = 0; }
  | Interfaces
  ;

Interfaces:
  IMPLEMENTS InterfaceTypeList
    { $$ = $2; }
  ;

InterfaceTypeList:
  InterfaceType
    {
      $$ = new deque<unicode_string>;
      $$->push_back(*($1));
      delete $1;
    }
  | InterfaceTypeList ',' InterfaceType
    {
      $$ = $1;
      $$->push_back(*($3));
      delete $3;
    }
  ;

ClassBody:
  '{' OptClassBodyDeclarations '}'
  ;

OptClassBodyDeclarations:
  | OptClassBodyDeclarations ClassBodyDeclaration
  ;

ClassBodyDeclaration:
    ClassMemberDeclaration 
       { yydeprecated = false; }
    OptIllegalSemicolon
  | StaticInitializer
      { yydeprecated = false; }
    OptIllegalSemicolon
  | ConstructorDeclaration
      { yydeprecated = false; }
    OptIllegalSemicolon
  ;

OptIllegalSemicolon:
  | ';'
  {
    assert(gCurrentCompiler != 0);
    gCurrentCompiler->ParseWarning(yylineno,
	   "Warning:  illegal use of semicolon in class body.", yytext);
  }
  ;

ClassMemberDeclaration:
  FieldDeclaration
  | MethodDeclaration
  | ClassDeclaration
  | InterfaceDeclaration
  ;

/*
 * I don't use these because they don't properly pass the flow of the
 * type information along the variable list.
 *
 * FieldDeclaration:
 *   OptModifiers Type VariableDeclarators ';'
 *   ;
 * VariableDeclarators:
 *   VariableDeclarator
 *   | VariableDeclarators ',' VariableDeclarator
 *   ;
 * VariableDeclarator:
 *   VariableDeclaratorId
 *   | VariableDeclaratorId '=' VariableInitializer
 *   ;
 * VariableDeclaratorId:
 *   Identifier
 *   | VariableDeclaratorId '[' ']'
 *   ;
 */

/*
 * the next few aren't part of the language spec ... they are needed to
 * make this work:  int x=0, y=x;
 */
FieldDeclaration:
  OptModifiers VariableDeclaration ';'
    {
      CJavaAccessFlags usedFlags(*$1);
      usedFlags.fPublic = usedFlags.fProtected = usedFlags.fPrivate =
          usedFlags.fStatic = usedFlags.fFinal = usedFlags.fTransient =
          usedFlags.fVolatile = 0;
      if (usedFlags.Count() > 0) {
	string errorMessage("Invalid modifier use in field declaration: ");
	errorMessage += usedFlags.FlagNames();
	yyerror(errorMessage.c_str());
	YYERROR;
      }
      gCurrentCompiler->AddField(new CDeclarationStatement($2, $1,
							   yydeprecated));
    }
  ;

VariableDeclaration:
  PartialVariable
    {
      $$ = $1;
      // take off the bogus entry added in PartialVariable
      delete $$->front();  
      $$->pop_front();
    }
  ;

PartialVariable:
  Type Identifier OptDims OptVariableInitializer
    {
      if ($1->GetBaseType() == CJavaTypeSignature::Void) {
	yyerror("Invalid use of 'void' for a variable type.");
	YYERROR;
      }
      $$ = new deque<CVariableDeclaration*>;
      // push a bogus entry at the beginning, just to hold the base type of
      // this variable declaration sequence.
      $$->push_front(new CVariableDeclaration(*($1), unicode_string()));
      $1->SetArrayBounds($1->GetArrayBounds() + $3);
      CArrayInitializer* arrayInitializer= DYNAMIC_CAST(CArrayInitializer, $4);
      if (arrayInitializer != 0) {
	if ($1->GetArrayBounds() == 0) {
	  string errorMessage("Cannot use array initializer with non-array ");
	  errorMessage += ::UnicodeToUTF(*$2);
	  yyerror(errorMessage.c_str());
	  YYERROR;
	} else {
	  arrayInitializer->SetArrayType(*($1));
	}
      }
      
      CVariableDeclaration declaration(*($1), *($2));
      if (gCurrentCompiler->InStatementBlock()) {
	if (gCurrentCompiler->LookupLocalVariable(*$2) != 0) {
	  string errorMessage = ::UnicodeToUTF(*$2);
	  errorMessage += " already declared in this scope.";
	  yyerror(errorMessage.c_str());
	  YYERROR;
	} else {
	  gCurrentCompiler->AddLocalVariable(declaration);
	}
      }
      CExpression* initializer = $4;
      if (initializer != 0) {
	initializer =
	  new CBinaryExpression(::InterpretSymbolExpression(*($2)),
				'=', initializer);
      }
      $$->push_back(new CVariableDeclaration(*($1), *($2), initializer));
      delete $1;
      delete $2;
    }
  | PartialVariable ',' Identifier OptDims OptVariableInitializer
    {
      $$ = $1;
      CJavaTypeSignature thisType = ($$)->front()->GetType();
      thisType.SetArrayBounds(thisType.GetArrayBounds() + $4);
      CArrayInitializer* arrayInitializer= DYNAMIC_CAST(CArrayInitializer, $5);
      if (arrayInitializer != 0) {
	if (thisType.GetArrayBounds() == 0) {
	  string errorMessage("Cannot use array initializer with non-array ");
	  errorMessage += ::UnicodeToUTF(*$3);
	  yyerror(errorMessage.c_str());
	  YYERROR;	  
	} else {
	  arrayInitializer->SetArrayType(thisType);
	}
      }

      CVariableDeclaration declaration(thisType, *($3));
      if (gCurrentCompiler->InStatementBlock()) {
	if (gCurrentCompiler->LookupLocalVariable(*$3) != 0) {
	  string errorMessage = ::UnicodeToUTF(*$3);
	  errorMessage += " already declared in this scope.";
	  yyerror(errorMessage.c_str());
	  YYERROR;
	} else {
	  gCurrentCompiler->AddLocalVariable(declaration);
	}
      }
      CExpression* initializer = $5;
      if (initializer != 0) {
	initializer =
	  new CBinaryExpression(::InterpretSymbolExpression(*($3)),
				'=', initializer);
      }
      $$->push_back(new CVariableDeclaration(thisType, *($3), initializer));
      delete $3;
    }
  ;

FinalPartialVariable:
  Type Identifier OptDims OptVariableInitializer
    {
      if ($1->GetBaseType() == CJavaTypeSignature::Void) {
	yyerror("Invalid use of 'void' for a variable type.");
	YYERROR;
      }
      $$ = new deque<CVariableDeclaration*>;
      // push a bogus entry at the beginning, just to hold the base type of
      // this variable declaration sequence.
      $$->push_front(new CVariableDeclaration(*($1), unicode_string(),
					      0, true));
      $1->SetArrayBounds($1->GetArrayBounds() + $3);
      CArrayInitializer* arrayInitializer= DYNAMIC_CAST(CArrayInitializer, $4);
      if (arrayInitializer != 0) {
	if ($1->GetArrayBounds() == 0) {
	  string errorMessage("Cannot use array initializer with non-array ");
	  errorMessage += ::UnicodeToUTF(*$2);
	  yyerror(errorMessage.c_str());
	  YYERROR;
	} else {
	  arrayInitializer->SetArrayType(*($1));
	}
      }
      
      if (gCurrentCompiler->InStatementBlock()) {
	if (gCurrentCompiler->LookupLocalVariable(*$2) != 0) {
	  string errorMessage = ::UnicodeToUTF(*$2);
	  errorMessage += " already declared in this scope.";
	  yyerror(errorMessage.c_str());
	  YYERROR;
	} else {
	  CVariableDeclaration declaration(*($1), *($2), 0, true);
	  gCurrentCompiler->AddLocalVariable(declaration);
	}
      }
      CExpression* initializer = $4;
      if (initializer != 0) {
	CExpression* expression = ::InterpretSymbolExpression(*($2));
	expression->SetLValue(true);
	initializer = new CBinaryExpression(expression, '=', initializer);
      } else {
	gCurrentCompiler->ParseWarning(yylineno,
	       "Warning:  final variable without initializer.", yytext);
      }
      $$->push_back(new CVariableDeclaration(*($1), *($2), initializer, true));
      delete $1;
      delete $2;
    }
  | FinalPartialVariable ',' Identifier OptDims OptVariableInitializer
    {
      $$ = $1;
      CJavaTypeSignature thisType = ($$)->front()->GetType();
      thisType.SetArrayBounds(thisType.GetArrayBounds() + $4);
      CArrayInitializer* arrayInitializer= DYNAMIC_CAST(CArrayInitializer, $5);
      if (arrayInitializer != 0) {
	if (thisType.GetArrayBounds() == 0) {
	  string errorMessage("Cannot use array initializer with non-array ");
	  errorMessage += ::UnicodeToUTF(*$3);
	  yyerror(errorMessage.c_str());
	  YYERROR;	  
	} else {
	  arrayInitializer->SetArrayType(thisType);
	}
      }

      if (gCurrentCompiler->InStatementBlock()) {
	if (gCurrentCompiler->LookupLocalVariable(*$3) != 0) {
	  string errorMessage = ::UnicodeToUTF(*$3);
	  errorMessage += " already declared in this scope.";
	  yyerror(errorMessage.c_str());
	  YYERROR;
	} else {
	  CVariableDeclaration declaration(thisType, *($3), 0, true);
	  gCurrentCompiler->AddLocalVariable(declaration);
	}
      }
      CExpression* initializer = $5;
      if (initializer != 0) {
	CExpression* expression = ::InterpretSymbolExpression(*($3));
	expression->SetLValue(true);
	initializer = new CBinaryExpression(expression, '=', initializer);
      } else {
	gCurrentCompiler->ParseWarning(yylineno,
	       "Warning:  final variable without initializer.", yytext);
      }
      $$->push_back(new CVariableDeclaration(thisType, *($3),
					     initializer, true));
      delete $3;
    }
  ;

OptVariableInitializer:
    { $$ = 0; }
  | '=' VariableInitializer
    { $$ = $2; }
  ;




VariableInitializer:
  Expression
  | ArrayInitializer
  ;

ArrayInitializer:
  '{' OptVariableInitializers '}'
    { $$ = $2; }
  ;

OptVariableInitializers: OptComma
    { $$ = new CArrayInitializer(); }
  | VariableInitializers OptComma
    { $$ = new CArrayInitializer($1); }
  ;

VariableInitializers:
  VariableInitializer
    {
      $$ = new ExpressionList();
      $$->push_back($1);
    }
  | VariableInitializers ',' VariableInitializer
    {
      $$ = $1;
      $$->push_back($3);
    }
  ;

OptComma :
  | ','
  ;

MethodDeclaration:
  MethodHeader MethodBody
    { gCurrentCompiler->EndFunction($2); }
  ;

MethodHeader:
  /*  OptModifiers Type MethodDeclarator OptThrows */
  OptModifiers Type Identifier '(' OptFormalParameterList ')'
  OptDims OptThrows
    {
      const CIntermediateClass* currentClass =
	gCurrentCompiler->GetCurrentClass();
      if (currentClass->GetShortName() == *($3)) {
	string errorMessage("Invalid return value in constructor.");
	yyerror(errorMessage.c_str());
	YYERROR;
      }
      $2->SetArrayBounds($2->GetArrayBounds() + $7);
      CJavaAccessFlags usedFlags = *($1);
      usedFlags.fPublic = usedFlags.fProtected = usedFlags.fPrivate =
          usedFlags.fStatic = usedFlags.fFinal = usedFlags.fAbstract =
          usedFlags.fNative = usedFlags.fSynchronized = 0;
      if (usedFlags.Count() > 0) {
	string errorMessage("Invalid modifier use in method declaration: ");
	errorMessage += usedFlags.FlagNames();
	yyerror(errorMessage.c_str());
	YYERROR;
      }
      gCurrentCompiler->StartFunction(*($2), *($3), *($1), $5, $8,
				      yydeprecated);
      delete $1;
      delete $2;
      delete $3;
    }
  /* OptModifiers VOID MethodDeclarator OptThrows */
  | OptModifiers VOID Identifier '(' OptFormalParameterList ')'
  OptDims OptThrows
      {
      const CIntermediateClass* currentClass =
	gCurrentCompiler->GetCurrentClass();
      if (currentClass->GetName() == *($3)) {
	string errorMessage("Invalid use of return value with constructor.");
	yyerror(errorMessage.c_str());
	YYERROR;
      }
      if ($7 > 0) {
	yyerror("Invalid return value of void array.");
	YYERROR;
      }
      CJavaAccessFlags usedFlags = *($1);
      usedFlags.fPublic = usedFlags.fProtected = usedFlags.fPrivate =
          usedFlags.fStatic = usedFlags.fFinal = usedFlags.fAbstract =
          usedFlags.fNative = usedFlags.fSynchronized = 0;
      if (usedFlags.Count() > 0) {
	string errorMessage("Invalid modifier use in method declaration: ");
	errorMessage += usedFlags.FlagNames();
	yyerror(errorMessage.c_str());
	YYERROR;
      }
      gCurrentCompiler->StartFunction(CJavaTypeSignature::kVoid,
				      *($3), *($1), $5, $8, yydeprecated);
      delete $1;
      delete $3;
    }
  ;

OptThrows:
    { $$ = 0; }
  | Throws
  ;

OptFormalParameterList:
    { $$ = 0; }
  | FormalParameterList
  ;

FormalParameterList:
  FormalParameter
    {
      $$ = new deque<CVariableDeclaration>(1, *($1));
      delete $1;
    }
  | FormalParameterList ',' FormalParameter
    { 
      $$ = $1;
      for (deque<CVariableDeclaration>::iterator parameterIter = $1->begin();
	   parameterIter != $1->end(); ++parameterIter) {
	if ((*parameterIter).GetName() == (*($3)).GetName()) {
	  string errorString("Invalid re-use of parameter name: ");
	  errorString += ::UnicodeToString((*($3)).GetName());
	  yyerror(errorString.c_str());
	  YYERROR;	  
	}
      }
      $$->push_back(*($3));
      delete $3;
    }
  ;

FormalParameter:
  Type Identifier OptDims
    {
      if ($1->GetBaseType() == CJavaTypeSignature::Void) {
	yyerror("Invalid use of 'void' for a parameter type.");
	YYERROR;
      }
      $1->SetArrayBounds($1->GetArrayBounds() + $3);
      $$ = new CVariableDeclaration(*($1), *($2));
      delete $1;
      delete $2;
    }
  | FINAL Type Identifier OptDims
    {
      if ($2->GetBaseType() == CJavaTypeSignature::Void) {
	yyerror("Invalid use of 'void' for a parameter type.");
	YYERROR;
      }
      $2->SetArrayBounds($2->GetArrayBounds() + $4);
      $$ = new CVariableDeclaration(*($2), *($3), 0, true);
      delete $2;
      delete $3;
    }
  ;

Throws:
  THROWS ClassTypeList
    { $$ = $2; }
  ;

ClassTypeList:
  ClassType
    {
      $$ = new deque<unicode_string>;
      $$->push_back(*($1));
      delete $1;
    }
  | ClassTypeList ',' ClassType
    {
      $$ = $1;
      $$->push_back(*($3));
      delete $3;
    }
  ;

MethodBody:
  Block 
  | ';'
    { $$ = 0; }
  ;

StaticInitializer:
  STATIC Block
    { gCurrentCompiler->AddStaticCode($2); }
  ;

ConstructorDeclaration:
  OptModifiers SimpleName
    {
      const CIntermediateClass* currentClass =
	gCurrentCompiler->GetCurrentClass();
      if (!(currentClass->GetShortName() == *($2))) {
	string errorMessage("No type given for function: ");
	errorMessage += ::UnicodeToString(*($2));
	yyerror(errorMessage.c_str());
	YYERROR;
      }
      delete $2;
    }
  '(' OptFormalParameterList ')' OptThrows
    {
      CJavaAccessFlags usedFlags(*$1);
      usedFlags.fPublic = usedFlags.fProtected = usedFlags.fPrivate = 0;
      if (usedFlags.Count() > 0) {
	string errorMessage("Invalid modifier use in constructor: ");
	errorMessage += usedFlags.FlagNames();
	yyerror(errorMessage.c_str());
	YYERROR;
      }
      gCurrentCompiler->StartConstructor(*$1, $5, $7, yydeprecated);
      delete $1;
    }
  ConstructorBody
    { 
      CCompoundStatement* statement = new CCompoundStatement($9);
      gCurrentCompiler->EndFunction(statement);
    }
  ;

/*
 * ConstructorDeclarator:
 *   SimpleName '(' OptFormalParameterList ')'
 *  ;
 */

ConstructorBody:
  '{' OptBlockStatements '}'
    {
      $$ = $2;
    }
  | '{' ExplicitConstructorInvocation OptBlockStatements '}'
    {
      $$ = $3;
      $$->push_front($2);
    }
  ;

ExplicitConstructorInvocation:
  THIS '(' OptArgumentList ')' ';'
    {
      $$ = new CExplicitConstructorCall(CExplicitConstructorCall::kThis, $3);
    }
  | SUPER '(' OptArgumentList ')' ';'
    {
      $$ = new CExplicitConstructorCall(CExplicitConstructorCall::kSuper, $3);
    }
  ;

OptArgumentList:
    { $$ = 0; }
  | ArgumentList
  ;

InterfaceDeclaration:
  OptModifiers INTERFACE Identifier OptExtendsInterfaces
    {
      CJavaAccessFlags usedFlags(*($1));
      usedFlags.fPublic = usedFlags.fAbstract = 0;
      $1->fAbstract = $1->fInterface = 1;
      if (usedFlags.Count() > 0) {
	string errorMessage("Invalid interface modifier use: ");
	errorMessage += usedFlags.FlagNames();
	yyerror(errorMessage.c_str());
	YYERROR;
      }
      gCurrentCompiler->StartInterface(*($3), $1, $4, yydeprecated);
    }
  InterfaceBody
    { gCurrentCompiler->EndClass(); }
  ;

OptExtendsInterfaces:
    { $$ = 0; }
  | ExtendsInterfaces
  ;

ExtendsInterfaces:
  EXTENDS InterfaceType
    {
      $$ = new deque<unicode_string>;
      $$->push_back(*($2));
      delete $2;
    }
  | ExtendsInterfaces ',' InterfaceType
    {
      $$ = $1;
      $$->push_back(*($3));
      delete $3;
    }
  ;

InterfaceBody:
  '{' OptInterfaceMemberDeclarations '}'
  ;

OptInterfaceMemberDeclarations:
  | InterfaceMemberDeclarations
  ;

InterfaceMemberDeclarations:
  InterfaceMemberDeclaration
  | InterfaceMemberDeclarations InterfaceMemberDeclaration
  ;

InterfaceMemberDeclaration:
  ConstantDeclaration
  | AbstractMethodDeclaration
  ;

ConstantDeclaration:
  FieldDeclaration
  ;

AbstractMethodDeclaration:
  MethodHeader ';'
    { gCurrentCompiler->EndFunction(0); }
  ;

Block:
  '{'
    { gCurrentCompiler->PushLocalScope(); }
  OptBlockStatements '}'
    {
      gCurrentCompiler->PopLocalScope();
      $$ = new CCompoundStatement($3);
    }
  ;

OptBlockStatements:
    { $$ = new StatementList(); }
  | BlockStatements
  ;

BlockStatements:
  BlockStatement
    {
      $$ = new StatementList();
      if ($1 != 0) {
	$$->push_front($1);
      }
    }
  | BlockStatements BlockStatement
    {
      $$ = $1;
      if ($2 != 0) {
	$$->push_back($2);
      }
    }
  ;

BlockStatement:
  LocalVariableDeclarationStatement
  | Statement
  | ClassDeclaration
    { $$ = 0; }
  ;

LocalVariableDeclarationStatement:
  LocalVariableDeclaration ';'
    { $$ = new CDeclarationStatement($1); }
  | FINAL FinalVariableDeclaration ';'
    { $$ = new CDeclarationStatement($2); }
  ;

LocalVariableDeclaration:
  VariableDeclaration
  ;

FinalVariableDeclaration:
  FinalPartialVariable
    {
      $$ = $1;
      // take off the bogus entry added in PartialVariable
      delete $$->front();  
      $$->pop_front();
    }
  ;

Statement:
  StatementWithoutTrailingSubstatement
  | LabeledStatement
  | IfThenStatement
  | IfThenElseStatement
  | WhileStatement
  | ForStatement
  ;

StatementNoShortIf:
  StatementWithoutTrailingSubstatement
  | LabeledStatementNoShortIf
  | IfThenElseStatementNoShortIf
  | WhileStatementNoShortIf
  | ForStatementNoShortIf
  ;

StatementWithoutTrailingSubstatement:
  Block
    { $$ = $1; }
  | EmptyStatement
  | ExpressionStatement
  | SwitchStatement
  | DoStatement
  | BreakStatement
  | ContinueStatement
  | ReturnStatement
  | SynchronizedStatement
  | ThrowStatement
  | TryStatement
  ;

EmptyStatement:
  ';'
    { $$ = 0; }
  ;

LabeledStatement:
  Identifier ':' Statement
    { $$ = new CLabelStatement($1, $3); }
  ;

LabeledStatementNoShortIf:
  Identifier ':' StatementNoShortIf
    { $$ = new CLabelStatement($1, $3); }
  ;

ExpressionStatement:
  StatementExpression ';'
    { $$ = new CExpressionStatement($1); }
  ;

StatementExpression:
  Assignment
  | PreIncrementExpression
  | PreDecrementExpression
  | PostIncrementExpression
  | PostDecrementExpression
  | MethodInvocation
  | ClassInstanceCreationExpression
  ;

IfThenStatement:
  IF '(' Expression ')' Statement
    { $$ = new CIfStatement($3, $5); }
  ;

IfThenElseStatement:
  IF '(' Expression ')' StatementNoShortIf ELSE Statement
    { $$ = new CIfStatement($3, $5, $7); }
  ;

IfThenElseStatementNoShortIf:
  IF '(' Expression ')' StatementNoShortIf ELSE StatementNoShortIf
    { $$ = new CIfStatement($3, $5, $7); }
  ;

SwitchStatement:
  SWITCH '(' Expression ')' SwitchBlock
    { $$ = new CSwitch($3, $5); }
  ;

SwitchBlock:
  '{' OptSwitchBlockStatements '}'
    { $$ = new CCompoundStatement($2); }
  ;

OptSwitchBlockStatements:
    { $$ = new StatementList(); }
  | SwitchBlockStatements
  ;

SwitchBlockStatements:
  SwitchBlockStatement
    {
      $$ = new StatementList();
      if ($1 != 0) {
	$$->push_back($1);
      }
    }
  | SwitchBlockStatements SwitchBlockStatement
    {
      $$ = $1;
      if ($2 != 0) {
	$$->push_back($2);
      }
    }
  ;

SwitchBlockStatement:
  BlockStatement
  | CASE ConstantExpression ':'
    { $$ = new CLabelStatement($2, 0); }
  | DEFAULT ':'
    { $$ = new CLabelStatement(0); }
  ;

/*
 * I'm not using these becuase my old design assumed a less-formed structure
 * based on the old language spec.
 * 
 * SwitchBlock:
 *   '{' OptSwitchBlockStatementGroups '}'
 *   ;
 * OptSwitchBlockStatementGroups: OptSwitchLabels
 *   | SwitchBlockStatementGroups OptSwitchLabels
 *   ;
 * SwitchBlockStatementGroups:
 *   SwitchBlockStatementGroup
 *   | SwitchBlockStatementGroups SwitchBlockStatementGroup
 *   ;
 * SwitchBlockStatementGroup:
 *   SwitchLabels BlockStatements
 *   ;
 * OptSwitchLabels:
 *   | SwitchLabels
 *   ;
 * SwitchLabels:
 *   SwitchLabel
 *   | SwitchLabels SwitchLabel
 *   ;
 * SwitchLabel:
 *   CASE ConstantExpression ':'
 *   | DEFAULT ':'
 *   ;
 */

WhileStatement:
  WHILE '(' Expression ')' Statement
    { $$ = new CWhileStatement($5, $3); }
  ;

WhileStatementNoShortIf:
  WHILE '(' Expression ')' StatementNoShortIf
    { $$ = new CWhileStatement($5, $3); }
  ;

DoStatement:
  DO Statement WHILE '(' Expression ')' ';'
    { $$ = new CDoStatement($2, $5); }
  ;

ForStatement:
  FOR PushForStatementScope
  '(' OptForInit ';' OptExpression ';' OptForUpdate ')' Statement
    {
      $$ = new CForStatement($4, $6, $8, $10);
      gCurrentCompiler->PopLocalScope();
    }
  ;

ForStatementNoShortIf:
  FOR PushForStatementScope
  '(' OptForInit ';' OptExpression ';' OptForUpdate ')' StatementNoShortIf
    {
      $$ = new CForStatement($4, $6, $8, $10);
      gCurrentCompiler->PopLocalScope();
    }
  ;

PushForStatementScope:
    { gCurrentCompiler->PushLocalScope(); }
  ;

OptForInit:
    { $$ = 0; }
  | ForInit
  ;

ForInit:
  StatementExpressionList
    {
      StatementList* statements = new StatementList;
      for (ExpressionList::iterator i = $1->begin(); i != $1->end(); ++i) {
	statements->push_back(new CExpressionStatement(*i));
      }
      delete $1;
      $$ = new CCompoundStatement(statements);
    }
  | LocalVariableDeclaration
    { $$ = new CDeclarationStatement($1); }
  ;

OptExpression:
    { $$ = 0; }
  | Expression
  ;

OptForUpdate:
    { $$ = 0; }
  | ForUpdate
  ;

ForUpdate:
  StatementExpressionList
  ;

StatementExpressionList:
  StatementExpression
    {
      $$ = new ExpressionList;
      $$->push_front($1);
    }
  | StatementExpressionList ',' StatementExpression
    {
      $$ = $1;
      $$->push_back($3);
    }
  ;

BreakStatement:
  BREAK ';'
    { $$ = new CBranchStatement(CBranchStatement::kBreak); }
  | BREAK Identifier ';'
    { $$ = new CBranchStatement(CBranchStatement::kBreak, $2); }
  ;

ContinueStatement:
  CONTINUE ';'
    { $$ = new CBranchStatement(CBranchStatement::kContinue); }
  | CONTINUE Identifier ';'
    { $$ = new CBranchStatement(CBranchStatement::kContinue, $2); }
  ;

ReturnStatement:
  RETURN ';'
    { $$ = new CReturnStatement(); }
  | RETURN Expression ';'
    { $$ = new CReturnStatement($2); }
  ;

ThrowStatement:
  THROW Expression ';'
    { $$ = new CThrowStatement($2); }
  ;

SynchronizedStatement:
  SYNCHRONIZED '(' Expression ')' Block
    { $$ = new CSynchronized($3, $5, gCurrentCompiler->AddUnnamedVariable()); }
  ;

TryStatement:
  TRY Block Catches
    {
      unsigned short handlerVariable, subroutineVariable;
      $$ = new CTryStatement($2, $3, 0, handlerVariable, subroutineVariable);
    }
  | TRY Block OptCatches Finally
    {
      unsigned short handlerVariable, subroutineVariable;
      handlerVariable = gCurrentCompiler->AddUnnamedVariable();
      subroutineVariable = gCurrentCompiler->AddUnnamedVariable();
      $$ = new CTryStatement($2, $3, $4, handlerVariable, subroutineVariable);
    }
  ;

OptCatches:
    { $$ = 0; }
  | Catches
  ;

Catches:
  CatchClause
    {
      $$ = new deque<CCatchClause*>;
      $$->push_back($1);
    }
  | Catches CatchClause
    {
      $$ = $1;
      $$->push_back($2);
    }
  ;

CatchClause:
  CATCH 
    { gCurrentCompiler->PushLocalScope(); }
  '(' FormalParameter ')'
    { 
      if (gCurrentCompiler->LookupLocalVariable($4->GetName()) != 0) {
	string errorMessage = ::UnicodeToUTF($4->GetName());
	errorMessage += " already declared in this scope.";
	yyerror(errorMessage.c_str());
	YYERROR;
      } else {
	$<longNumber>$ = gCurrentCompiler->AddLocalVariable(*($4));
      }
    }
  Block
    {
      gCurrentCompiler->PopLocalScope();
      $$ = new CCatchClause($<longNumber>6, $4->GetType(), $7);
    }
  ;

Finally:
  FINALLY Block
    { $$ = $2; }
  ;

Primary:
  PrimaryNoNewArray
  | ArrayCreationExpression
  ;

PrimaryNoNewArray:
  Literal
  | THIS
    { $$ = new CSpecialExpression(CSpecialExpression::kThis); }
  | '(' Expression ')'
    {
      $$ = $2;
    }
  | ClassInstanceCreationExpression
  | FieldAccess
  | MethodInvocation
  | ArrayAccess
  ;

ClassInstanceCreationExpression:
  NEW ClassType '(' OptArgumentList ')'
    {
      $$ = new CNewObject(*($2), $4);
      delete $2;
    }
  | NEW ClassType '(' OptArgumentList ')' '{'
    {
      $<text>$ =
	new unicode_string(gCurrentCompiler->StartAnonymousClass($2));
    }
    OptClassBodyDeclarations '}'
    {
      gCurrentCompiler->EndClass();
      $$ = new CNewObject(*($<text>7), $4);
      delete $<text>7;
    }
  | Primary DOTNEW ClassType '(' OptArgumentList ')'
    {
      $$ = new CNewObject(*($3), $5, $1);
      delete $3;
    }
  | Primary DOTNEW ClassType '(' OptArgumentList ')' '{'
    {
      $<text>$ =
	new unicode_string(gCurrentCompiler->StartAnonymousClass($3));
    }
    OptClassBodyDeclarations '}'
    {
      gCurrentCompiler->EndClass();
      $$ = new CNewObject(*($<text>8), $5, $1);
      delete $<text>7;
    }
  ;

ArgumentList:
  Expression
    {
      $$ = new ExpressionList;
      $$->push_back($1);
    }
  | ArgumentList ',' Expression
    {
      $$ = $1;
      $$->push_back($3);
    }
  ;

ArrayCreationExpression:
  NEW PrimitiveType DimExprs OptDims
    {
      if ($2->GetBaseType() == CJavaTypeSignature::Void) {
	yyerror("Invalid array creation using 'void' type.");
	YYERROR;
      }
      $2->SetArrayBounds($3->size() + $4);
      $$ = new CNewArray(*($2), $3);
      delete $2;
    }
  | NEW ClassOrInterfaceType DimExprs OptDims
    {
      int arrayDimensions = $3->size() + $4;
      CJavaTypeSignature tempSignature(*($2), arrayDimensions);
      $$ = new CNewArray(tempSignature, $3);
      delete $2;
    }
  ;

DimExprs:
  DimExpr
    {
      $$ = new ExpressionList();
      $$->push_front($1);
    }
  | DimExprs DimExpr
    {
      $$ = $1;
      $$->push_back($2);
    }
  ;

DimExpr:
  '[' Expression ']'
    { $$ = $2; }
  ;

OptDims:
    { $$ = 0; }
  | Dims
  ;

Dims:
  '[' ']'
    { $$ = 1; }
  | Dims '[' ']'
    { $$ = $1 + 1; }
  ;

FieldAccess:
  Primary '.' Identifier
    {
      $$ = new CClassFieldExpression($1, *($3));
      delete $3;
    }
  | SUPER '.' Identifier
    {
      CExpression* super = new CSpecialExpression(CSpecialExpression::kSuper);
      $$ = new CClassFieldExpression(super, *($3));
      delete $3;
    }
  ;

MethodInvocation:
  Name '(' OptArgumentList ')'
    {
      unicode_string::size_type slashPosition = $1->find(kSymbolDivider);
      if (slashPosition != unicode_string::npos) {
	unicode_string prefix(*($1), 0, slashPosition);
	if (gCurrentCompiler->LookupLocalVariable(prefix) != 0 ||
	    gCurrentCompiler->LookupOuterLocalVariable(prefix) != 0) {
	  unicode_string::size_type lastSlash =
	    $1->find_last_of(kSymbolDivider);
	  prefix.assign(*($1), 0, lastSlash);
	  CExpression* baseExpression = ::InterpretSymbolExpression(prefix);
	  unicode_string methodName(*($1), lastSlash + 1);
	  $$ = new CMethodCall(baseExpression, methodName, $3);
	} else {
	  $$ = new CMethodCall(0, *($1), $3);
	}
      } else {
	$$ = new CMethodCall(0, *($1), $3);
      }
      delete $1;
    }
  | Primary '.' Identifier '(' OptArgumentList ')'
    {
      $$ = new CMethodCall($1, *($3), $5);
      delete $3;
    }
  | SUPER '.' Identifier '(' OptArgumentList ')'
    {
      CExpression* super = new CSpecialExpression(CSpecialExpression::kSuper);
      $$ = new CMethodCall(super, *($3), $5);
      delete $3;
    }
  ;

ArrayAccess:
  Name '[' Expression ']'
    {
      CExpression* baseExpression = ::InterpretSymbolExpression(*($1));
      delete $1;
      $$ = new CArrayIndex(baseExpression, $3);
    }
  | PrimaryNoNewArray '[' Expression ']'
    { $$ = new CArrayIndex($1, $3); }
  ;

PostfixExpression:
  Primary
  | Name
    {
      $$ = ::InterpretSymbolExpression(*($1));
      delete $1;
    }
  | PostIncrementExpression
  | PostDecrementExpression
  ;

PostIncrementExpression:
  PostfixExpression INCR
    { $$ = new CUnaryExpression($1, INCR, CUnaryExpression::kPostfix, true); }
  ;

PostDecrementExpression:
  PostfixExpression DECR
    { $$ = new CUnaryExpression($1, DECR, CUnaryExpression::kPostfix, true); }
  ;

UnaryExpression:
  PreIncrementExpression
  | PreDecrementExpression
  | '+' UnaryExpression
    { $$ = new CUnaryExpression($2, '+', CUnaryExpression::kPrefix, false); }
  | '-' UnaryExpression
    { $$ = new CUnaryExpression($2, '-', CUnaryExpression::kPrefix, false); }
  | UnaryExpressionNotPlusMinus
  ;

PreIncrementExpression:
  INCR UnaryExpression
    { $$ = new CUnaryExpression($2, INCR, CUnaryExpression::kPrefix, true); }
  ;

PreDecrementExpression:
  DECR UnaryExpression
    { $$ = new CUnaryExpression($2, DECR, CUnaryExpression::kPrefix, true); }
  ;

UnaryExpressionNotPlusMinus:
  PostfixExpression
  | '~' UnaryExpression
    { $$ = new CUnaryExpression($2, '~', CUnaryExpression::kPrefix, false); }
  | '!' UnaryExpression
    { $$ = new CUnaryExpression($2, '!', CUnaryExpression::kPrefix, false); }
  | CastExpression
  ;

CastExpression:
  '(' PrimitiveType OptDims ')' UnaryExpression
    {
      $2->SetArrayBounds($3);
      $$ = new CCastExpression(*($2), $5);
      delete $2;
    }
  | '(' Expression ')' UnaryExpressionNotPlusMinus
    {
      CClassFieldExpression* castTo = DYNAMIC_CAST(CClassFieldExpression, $2);
      if (castTo == 0) {
        delete $2;
        delete $4;
	yyerror("Invalid cast expression.");
	YYERROR;
      } else {
        CJavaTypeSignature type(castTo->GetFieldString());
        $$ = new CCastExpression(type, $4);
        delete $2;
      }
    }
  | '(' Name Dims ')' UnaryExpressionNotPlusMinus
    {
      CJavaTypeSignature type(*($2), $3);
      $$ = new CCastExpression(type, $5);
      delete $2;
    }
  ;

MultiplicativeExpression:
  UnaryExpression
  | MultiplicativeExpression '*' UnaryExpression
    { $$ = new CBinaryExpression($1, '*', $3); }
  | MultiplicativeExpression '/' UnaryExpression
    { $$ = new CBinaryExpression($1, '/', $3); }
  | MultiplicativeExpression '%' UnaryExpression
    { $$ = new CBinaryExpression($1, '%', $3); }
  ;

AdditiveExpression:
  MultiplicativeExpression
  | AdditiveExpression '+' MultiplicativeExpression
    { $$ = new CBinaryExpression($1, '+', $3); }
  | AdditiveExpression '-' MultiplicativeExpression
    { $$ = new CBinaryExpression($1, '-', $3); }
  ;

ShiftExpression:
  AdditiveExpression
  | ShiftExpression SHIFT_LEFT AdditiveExpression
    { $$ = new CBinaryExpression($1, SHIFT_LEFT, $3); }
  | ShiftExpression BITSHIFT_RIGHT AdditiveExpression
    { $$ = new CBinaryExpression($1, BITSHIFT_RIGHT, $3); }
  | ShiftExpression FILL_SHIFT_RIGHT AdditiveExpression
    { $$ = new CBinaryExpression($1, FILL_SHIFT_RIGHT, $3); }
  ;

RelationalExpression:
  ShiftExpression
  | RelationalExpression '<' ShiftExpression
    { $$ = new CBinaryExpression($1, '<', $3); }
  | RelationalExpression '>' ShiftExpression
    { $$ = new CBinaryExpression($1, '>', $3); }
  | RelationalExpression LTEQ ShiftExpression
    { $$ = new CBinaryExpression($1, LTEQ, $3); }
  | RelationalExpression GTEQ ShiftExpression
    { $$ = new CBinaryExpression($1, GTEQ, $3); }
  | RelationalExpression INSTANCEOF ReferenceType
    {
      $$ = new CInstanceof($1, *($3));
      delete $3;
    }
  ;

EqualityExpression:
  RelationalExpression
  | EqualityExpression EQUAL_COMPARE RelationalExpression
    { $$ = new CBinaryExpression($1, EQUAL_COMPARE, $3); }
  | EqualityExpression NOT_EQUAL RelationalExpression
    { $$ = new CBinaryExpression($1, NOT_EQUAL, $3); }
  ;

AndExpression:
  EqualityExpression
  | AndExpression '&' EqualityExpression
    { $$ = new CBinaryExpression($1, '&', $3); }
  ;

ExclusiveOrExpression:
  AndExpression
  | ExclusiveOrExpression '^' AndExpression
    { $$ = new CBinaryExpression($1, '^', $3); }
  ;

InclusiveOrExpression:
  ExclusiveOrExpression
  | InclusiveOrExpression '|' ExclusiveOrExpression
    { $$ = new CBinaryExpression($1, '|', $3); }
  ;

ConditionalAndExpression:
  InclusiveOrExpression
  | ConditionalAndExpression AND InclusiveOrExpression
    { $$ = new CBinaryExpression($1, AND, $3); }
  ;

ConditionalOrExpression:
  ConditionalAndExpression
  | ConditionalOrExpression OR ConditionalAndExpression
    { $$ = new CBinaryExpression($1, OR, $3); }
  ;

ConditionalExpression:
  ConditionalOrExpression
  | ConditionalOrExpression '?' Expression ':' ConditionalExpression
    { $$ = new CTrinaryExpression($1, $3, $5); }
  ;

AssignmentExpression:
  ConditionalExpression
  | Assignment
  ;

Assignment:
  LeftHandSide AssignmentOperator AssignmentExpression
    { $$ = new CBinaryExpression($1, $2, $3); }
  ;

LeftHandSide:
  Name
    {
      $$ = ::InterpretSymbolExpression(*($1));
      delete $1;
    }
  | FieldAccess
  | ArrayAccess
  ;

AssignmentOperator:
  '='
    { $$ = '='; }
  | MUL_EQUALS
    { $$ = MUL_EQUALS; }
  | DIV_EQUALS
    { $$ = DIV_EQUALS; }
  | MOD_EQUALS
    { $$ = MOD_EQUALS; }
  | ADD_EQUALS
    { $$ = ADD_EQUALS; }
  | SUB_EQUALS
    { $$ = SUB_EQUALS; }
  | SHIFT_LEFT_EQUALS
    { $$ = SHIFT_LEFT_EQUALS; }
  | SHIFT_RIGHT_EQUALS
    { $$ = SHIFT_RIGHT_EQUALS; }
  | FILL_SHIFT_RIGHT_EQUALS
    { $$ = FILL_SHIFT_RIGHT_EQUALS; }
  | AND_EQUALS
    { $$ = AND_EQUALS; }
  | XOR_EQUALS
    { $$ = XOR_EQUALS; }
  | OR_EQUALS
    { $$ = OR_EQUALS; }
  ;

Expression:
  AssignmentExpression
  ;

ConstantExpression:
  Expression
  ;

%%

void
yyerror(const char* errorMessage)
{
  assert(gCurrentCompiler != 0);
  gCurrentCompiler->ParseError(yylineno, errorMessage, yytext);
}

void
InitializeParser(const string& parseString, CCompiler* compilerAlias)
{
  #ifndef NDEBUG
  if (::getenv("YYDEBUG") != 0) {
    yydebug = 1;
  }
  #endif
  gCurrentCompiler = compilerAlias;
  assert(gCurrentCompiler != 0);
  InitializeLexer(parseString);
}

void
FinishParser()
{
  FinishLexer();
  gCurrentCompiler = 0;
  #ifndef NDEBUG
  yydebug = 0;
  #endif
}

//
//  Function name : InterpretSymbolExpression
//  Description : Used to parse a qualified symbol into a base variable and
//    some sequence of field accesses.  So "x" is parsed as the expression
//    representing the variable 'x' but "x.y.z" is parsed as the variable
//    x dereferenced to get field y, dereferenced to get field z.
//
CExpression*
InterpretSymbolExpression(const unicode_string& symbol)
{
  unicode_string::size_type slashPosition = symbol.find(kSymbolDivider);
  unicode_string baseVariable = symbol;
  CExpression* baseExpression = 0; 
  if (slashPosition < symbol.size()) {
    baseVariable.assign(symbol, 0, slashPosition);
  }
  const CLocalVariableRecord* match =
    gCurrentCompiler->LookupLocalVariable(baseVariable);
  if (match != 0) {
    CVariableDeclaration declaration = match->GetDeclaration();
    baseExpression =
      new CLocalVariableExpression(declaration.GetSignature(),
				   match->GetVariableIndex(),
				   declaration.IsFinal());
  } else {
    const COuterLocalExpression* outerLocal =
      gCurrentCompiler->LookupOuterLocalVariable(baseVariable);
    if (outerLocal != 0) {
      baseExpression = new COuterLocalExpression(*outerLocal);
    }
  }
  if (baseExpression != 0) {
    if (slashPosition < symbol.size()) {
      do {
	unicode_string::size_type previousPosition = slashPosition + 1;
	slashPosition = symbol.find(kSymbolDivider, previousPosition);
	if (slashPosition >= symbol.size()) {
	  slashPosition = symbol.size();
	}
	unicode_string nextField(symbol, previousPosition,
				 slashPosition - previousPosition);
	baseExpression = new CClassFieldExpression(baseExpression, nextField);
      } while (slashPosition < symbol.size());
    }
  } else {
    baseExpression = new CClassFieldExpression(symbol);
  }
  return baseExpression;
}
