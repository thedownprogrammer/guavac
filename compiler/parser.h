typedef union {
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
} YYSTYPE;
#define	ERROR	258
#define	DOTNEW	259
#define	ABSTRACT	260
#define	BOOLEAN	261
#define	BREAK	262
#define	BYTE	263
#define	CASE	264
#define	CATCH	265
#define	CHAR	266
#define	CLASS	267
#define	CONTINUE	268
#define	DEFAULT	269
#define	DO	270
#define	DOUBLE	271
#define	ELSE	272
#define	EXTENDS	273
#define	FALSE_TOKEN	274
#define	FINAL	275
#define	FINALLY	276
#define	FLOAT	277
#define	FOR	278
#define	IF	279
#define	IMPLEMENTS	280
#define	IMPORT	281
#define	INSTANCEOF	282
#define	INT	283
#define	INTERFACE	284
#define	LONG	285
#define	NATIVE	286
#define	NULL_TOKEN	287
#define	PACKAGE	288
#define	PRIVATE	289
#define	PROTECTED	290
#define	PUBLIC	291
#define	RETURN	292
#define	SHORT	293
#define	STATIC	294
#define	SUPER	295
#define	SWITCH	296
#define	SYNCHRONIZED	297
#define	THIS	298
#define	THROW	299
#define	THROWS	300
#define	TRANSIENT	301
#define	VOLATILE	302
#define	TRUE_TOKEN	303
#define	TRY	304
#define	VOID	305
#define	WHILE	306
#define	SHIFT_RIGHT_EQUALS	307
#define	FILL_SHIFT_RIGHT_EQUALS	308
#define	SHIFT_LEFT_EQUALS	309
#define	ADD_EQUALS	310
#define	SUB_EQUALS	311
#define	MUL_EQUALS	312
#define	DIV_EQUALS	313
#define	MOD_EQUALS	314
#define	AND_EQUALS	315
#define	XOR_EQUALS	316
#define	OR_EQUALS	317
#define	OR	318
#define	AND	319
#define	EQUAL_COMPARE	320
#define	NOT_EQUAL	321
#define	LTEQ	322
#define	GTEQ	323
#define	BITSHIFT_RIGHT	324
#define	FILL_SHIFT_RIGHT	325
#define	SHIFT_LEFT	326
#define	INCR	327
#define	DECR	328
#define	NEW	329
#define	INT_LITERAL	330
#define	CHARACTER_LITERAL	331
#define	LONG_LITERAL	332
#define	FLOAT_LITERAL	333
#define	DOUBLE_LITERAL	334
#define	SYMBOL	335
#define	STRING_LITERAL	336


extern YYSTYPE yylval;
