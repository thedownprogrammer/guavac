// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: parser_decls.h,v 1.3 1997/06/13 18:19:46 geppetto Exp $
#ifndef _parser_decls_h
#define _parser_decls_h

#include "string.h"
#include "JavaFieldSignature.h"
#include "VariableDeclaration.h"
#include <deque>
class CJavaAccessFlags;
class CJavaTypeSignature;
class CExpression;
class CStatement;
class CCompoundStatement;
class CCompiler;
class CCatchClause;

// These are global declarations needed for the lexer and parser files.

void InitializeParser(const string& inString, CCompiler* compilerAlias);
void FinishParser();

void InitializeLexer(const string& tokenizeString);
void FinishLexer();

extern char* yytext;
extern int yylineno;
extern bool yydeprecated;
int yylex();
int yyparse();
void yyerror(const char*);

typedef deque<CExpression*> ExpressionList;
typedef deque<CStatement*> StatementList;

#endif
