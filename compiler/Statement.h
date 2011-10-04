// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: Statement.h,v 1.10 1997/08/21 16:30:11 geppetto Exp $
#ifndef _Statement_h
#define _Statement_h
#pragma interface

#include "JavaFieldSignature.h"
#include "dynamic_cast.h"
#include "parser_decls.h"
#include "NonlocalBranch.h"
#include <list>
#include <vector>
class CCompiler;
class CExpression;
class CCodeSequence;
class CJavaClassFile;
class CJavaMethodInfo;
class CVariableDeclaration;
class CTryStatement;
class CCompileError;

//
//  Class name : CStatement
//  Description : A CStatement object is used as a node in the abstract
//    syntax tree for a Java class.  It represents a syntactic statement
//    in a piece of Java code, meaning a piece of a sequencial execution that
//    does not evaluate to any value that is used by the program.
//    This is intended as a base class only (that's why the constructor is
//    protected).  It is used to unify all of the different types of statements
//    under one type-safe umbrella, since a CStatement* value can be safely
//    dynamic casted down to the correct child value.
//
class CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
public:
  virtual ~CStatement();
  CCompileError* GenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
  unsigned long GetLineNumber() const { return fLineNumber; }
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed) = 0;
  CStatement();
  CCompileError* MakeError(const unicode_string& message);
  CCompileError* MakeError(const string& message);
  unsigned long fLineNumber;
};

//
//  Class name : CExpressionStatement
//  Description : This is a simple statement built out of one expression
//    followed by a semicolon.
//
class CExpressionStatement : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
public:
  CExpressionStatement(CExpression* adoptExpression);
  virtual ~CExpressionStatement();
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  CExpression* fExpression;
};


//
//  Class name : CCompoundStatement
//  Description : A statement made up of a sequence of sub-statements.
//
class CCompoundStatement : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
  friend class CSwitch;
public:
  CCompoundStatement(StatementList* adoptStatements);
  virtual ~CCompoundStatement();
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  StatementList* fChildren;
};

//
//  Class name : CDeclarationStatement
//  Description : This type of statement represents the declaration of some
//    number of variables, along with their optional initializers.
//
class CDeclarationStatement : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
public:
  CDeclarationStatement(deque<CVariableDeclaration*>* adoptDeclarations,
			CJavaAccessFlags* adoptModifiers = 0,
			bool deprecated = false);
  virtual ~CDeclarationStatement();
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  deque<CVariableDeclaration*>* fDeclarations;
  CJavaAccessFlags* fModifiers;
  bool fDeprecated;
};

//
//  Class name : CIfStatement
//  Description : This represents an IF-THEN or IF-THEN-ELSE statement.
//
class CIfStatement : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
public:
  CIfStatement(CExpression* adoptCondition, CStatement* adoptThenClause,
	       CStatement* adoptElseClause = 0);
  virtual ~CIfStatement();
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  CExpression* fCondition;
  CStatement* fThenClause;
  CStatement* fElseClause;
};

//
//  Class name : CWhileStatement
//  Description : This type of statement is used to model a while(){}
//    statement.
//
class CWhileStatement : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
public:
  CWhileStatement(CStatement* adoptLoop, CExpression* adoptCondition);
  virtual ~CWhileStatement();
  void RegisterContinueBranch(const CNonlocalBranch& instruction);
  void RegisterBreakBranch(const CNonlocalBranch& instruction);
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  CExpression* fCondition;
  CStatement* fLoopStatement;
  list<CNonlocalBranch> fNestedBreaks;
  list<CNonlocalBranch> fNestedContinues;
};

//
//  Class name : CDoStatement
//  Description : This type of statement is used to model a do{...}while(...)
//    statement.
//
class CDoStatement : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
public:
  CDoStatement(CStatement* adoptLoop, CExpression* adoptCondition);
  virtual ~CDoStatement();
  void RegisterContinueBranch(const CNonlocalBranch& instruction);
  void RegisterBreakBranch(const CNonlocalBranch& instruction);
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  CStatement* fLoopStatement;
  CExpression* fCondition;
  list<CNonlocalBranch> fNestedBreaks;
  list<CNonlocalBranch> fNestedContinues;
};

//
//  Class name : CExitStatement
//  Description : This type of statement represents a non-sequential exit from
//     the current block.  This includes 'return' and 'throw' statements.
//
class CReturnStatement : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
public:
  CReturnStatement(CExpression* adoptReturnValue = 0);
  virtual ~CReturnStatement();
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  CExpression* fValue;
};

//
//  Class name : CBranchStatement
//  Description : This type of statement is used to represent 'break' and
//    'continue' statements in Java.
//
class CBranchStatement : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
public:
  typedef enum { kBreak, kContinue } Type;
  CBranchStatement(Type type, unicode_string* adoptLabel = 0);
  virtual ~CBranchStatement();
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  Type fBranchType;
  unicode_string* fToLabel;
};


//
//  Class name : CForStatement
//  Description : This type of statment is used to represent a 'for' loop in
//    Java.
//
class CForStatement : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
public:
  CForStatement(CStatement* adoptInitializer, CExpression* adoptConditional,
		ExpressionList* adoptIncrementor, CStatement* adoptBody);
  virtual ~CForStatement();
  void RegisterContinueBranch(const CNonlocalBranch& instruction);
  void RegisterBreakBranch(const CNonlocalBranch& instruction);
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  CStatement* fInitializer;
  CExpression* fConditional;
  ExpressionList* fIncrementor;
  CStatement* fBody;
  list<CNonlocalBranch> fNestedBreaks;
  list<CNonlocalBranch> fNestedContinues;
};

//
//  Class name : CThrowStatement
//  Description : This type of statement represents a non-sequential exit from
//     the current block.
//
class CThrowStatement : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
public:
  CThrowStatement(CExpression* adoptThrowValue);
  virtual ~CThrowStatement();
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  CExpression* fValue;
};

//
//  Class name : CSynchronized
//  Description : This models the 'synchronized' concept in Java, which marks
//    entry to a block of code to be monitored for only one entry thread.
//
class CSynchronized : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
public:
  CSynchronized(CExpression* adoptCondition, CStatement* adoptBlock,
		unsigned short synchronizeVariable);
  virtual ~CSynchronized();
  void EmitMonitorExitCode(CCodeSequence& code);
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  CExpression* fCondition;
  CStatement* fBlock;
  unsigned short fSynchronizeVariable;
};

//
//  Class name : CLabelStatement
//  Description : This wrapper is used to mark another statement for
//     non-sequential jumping.  This includes labeled statements reached
//     by 'break' and 'continue' as well as 'case' and 'default' statements.
//
class CLabelStatement : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
  friend class CSwitch;
public:
  typedef enum { kLabel, kCase, kDefault } Type;
  CLabelStatement(unicode_string* label, CStatement* adoptStatement);
  CLabelStatement(CStatement *adoptDefault);
  CLabelStatement(CExpression* caseExpression, CStatement* adoptCase);
  virtual ~CLabelStatement();
  const unicode_string* GetLabel() const;
  void RegisterBreakBranch(const CNonlocalBranch& instruction);
  CStatement* GetChildStatement() { return fStatement; }
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  unicode_string* fLabel;
  CExpression* fCaseExpression;
  CStatement* fStatement;
  Type fType;
  list<CNonlocalBranch> fNestedBreaks;
};

//
//  Class name : CCatchClause
//  Description : This is a simple little class to encapsulate a catch clause
//    in a try statement.
//
class CCatchClause {
 public:
  CCatchClause(unsigned short variableIndex,
	       const CJavaTypeSignature& catchType,
	       CCompoundStatement* adoptBlock);
  ~CCatchClause();

  unsigned short GetLocalVariableIndex() const { return fExceptionVariable; }
 private:
  friend class CCompiler;
  friend CTryStatement;

  unsigned short fExceptionVariable;
  CJavaTypeSignature fCatchType;
  CCompoundStatement* fBlock;
};


//
//  Class name : CTryStatement
//  Description : The throw statement is used to represent a
//    throw-catch-finally structure in Java.  This is done by keeping the
//    throw block, a list of all of the catch clauses and the finally block.
//
class CTryStatement : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
public:
  CTryStatement(CCompoundStatement* adoptTryBlock,
		deque<CCatchClause*>* catchClauseList,
		CCompoundStatement* adoptFinally = 0,
		unsigned short finallyHandlerVariable = 0,
		unsigned short finallySubroutineVariable = 0);
  virtual ~CTryStatement();
  void EmitFinallyCall(CCodeSequence& code);
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  CCompoundStatement* fTryBlock;
  deque<CCatchClause*>* fCatchList;
  CCompoundStatement* fFinally;
  unsigned short fFinallyHandlerVariable;
  unsigned short fFinallySubroutineVariable;
  unsigned long fFinallySubroutineLocation;
};

//
//  Class name : CSwitch
//  Description : This type of statement models a switch statement in Java.
//
class CSwitch : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
public:
  CSwitch(CExpression* adoptSwitch, CCompoundStatement* block);
  virtual ~CSwitch();
  void RegisterBreakBranch(const CNonlocalBranch& instruction);
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  CCompileError* ScanStatementForLabels(CStatement& statement,
			vector<unsigned long>& keys, bool& foundDefault,
			long& highKey, long& lowKey,
			CCompileContext& context);
  void TableswitchLabel(CLabelStatement& label, CCodeSequence& code,
			unsigned long keyBase, long lowKey,
			unsigned long& defaultLocation);
  void LookupswitchLabel(CLabelStatement& label, CCodeSequence& code,
			 unsigned long keyBase, unsigned long& endKey,
			 unsigned long& defaultLocation);

  CExpression* fSwitchExpression;
  CCompoundStatement* fBlock;
  list<CNonlocalBranch> fNestedBreaks;
};

//
//  Class name : CExplicitConstructorCall
//  Description : An explicit constructor call is used as the first statement
//    in a constructor body.
//
class CExplicitConstructorCall : public CStatement {
  DynamicCastDeclarations;
  friend class CCompiler;
public:
  typedef enum { kThis, kSuper } Type;
  CExplicitConstructorCall(Type type, ExpressionList* adoptArguments);
  virtual ~CExplicitConstructorCall();
  Type GetType() const { return fType; }
protected:
  virtual CCompileError* HandleGenerateCode(CCodeSequence& code,
		   CCompileContext& context, unsigned short& stackUsed);
private:
  CExpression* fExpression;
  Type fType;
};

#endif
