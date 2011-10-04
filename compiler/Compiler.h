// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: Compiler.h,v 1.19 1997/11/10 00:48:06 geppetto Exp $
#ifndef _Compiler_h
#define _Compiler_h
#pragma interface

#include "JavaFieldSignature.h"
#include "JavaMethodSignature.h"
#include "unicode_string.h"
#include "parser_decls.h"
#include "JavaDirectory.h"
#include <string>
#include <deque>
#include <vector>
#include <map>
#include <list>
class CJavaMethodInfo;
class CJavaClassFile;
class CJavaAccessFlags;
class CIntermediateClass;
class CIntermediateFunction;
class CCompoundStatement;
class CDeclarationStatement;
class COuterLocalExpression;
class CStatement;
class CCompileError;
class CJavaFieldInfo;
class CLocalVariableRecord;
class ostream;


//
//  Class name : CCompiler
//  Description : This class is used to encapsulate the operations used in
//    compiling a Java source file.  This manages all of the table-driven
//    parser code produced by flex and bison.
//    To use this interface, call the public static member function
//    'CompileValue' which takes in a string and parses it into a list of
//    CJavaClassFiles, if possible.
//
class CCompiler {
public:
  typedef vector<CJavaClassFile*> ClassList;
  static bool CompileFile(const string& fileName,
			  const deque<string>& classPath,
			  ClassList& resultClasses,
			  deque<string>& dependencies);

  const CJavaClassFile* ImportClass(const unicode_string& className,
				    bool aliasBase = true);
  bool ImportPackage(const unicode_string& packageName);
  void SetPackage(const unicode_string& packageName);
  void NoPackage();

  void AddClassImport(const unicode_string& className);
  void AddPackageImport(const unicode_string& package);
  void StartClass(const unicode_string& className,
		  CJavaAccessFlags* adoptModifiers = 0,
		  unicode_string* adoptExtends = 0,
		  deque<unicode_string>* adoptInterfaces = 0,
                  bool deprecated = false);
  void StartInterface(const unicode_string& className,
		      CJavaAccessFlags* adoptModifiers = 0,
		      deque<unicode_string>* adoptInterfaceExtends = 0,
		      bool deprecated = false);
  unicode_string StartAnonymousClass(unicode_string* adoptParentName);
			   
  void EndClass();
  void StartFunction(const CJavaTypeSignature& returnType,
		     const unicode_string& functionName,
		     const CJavaAccessFlags& modifiers,
		     deque<CVariableDeclaration>* adoptParameters = 0,
		     deque<unicode_string>* adoptThrows = 0,
		     bool deprecated = false);
  void StartConstructor(const CJavaAccessFlags& modifiers,
			deque<CVariableDeclaration>* adoptParameters = 0,
			deque<unicode_string>* adoptThrows = 0,
			bool deprecated = false);
  void EndFunction(CCompoundStatement* adoptStatementBlock = 0);
  bool InStatementBlock() const;
  void PushLocalScope();
  void PopLocalScope();
  unsigned short AddLocalVariable(const CVariableDeclaration& signature);
  typedef unsigned short LocalVariableIndex;
  const CLocalVariableRecord*
    LookupLocalVariable(const unicode_string& name) const;
  const COuterLocalExpression*
    LookupOuterLocalVariable(const unicode_string& name) const;
  unsigned short AddUnnamedVariable(unsigned long width = 1);
  void AddField(CDeclarationStatement* adoptDeclaration);
  void AddStaticCode(CCompoundStatement* adoptStatement);
  void ParseError(unsigned long lineNumber, const string& errorMessage,
		  const string& input);
  void ParseWarning(unsigned long lineNumber, const string& errorMessage,
		    const string& input);

  CIntermediateClass* GetCurrentClass();
  const CIntermediateClass* GetCurrentClass() const;

  const CJavaClassFile* LookupClass(const unicode_string& name,
		    const CIntermediateClass* classContext = 0,
		    const CIntermediateFunction* methodContext = 0) const;
  const CJavaClassFile* LookupClass(const unicode_string& name,
				    const CCompileContext& context) const;

  bool ValidClass(const unicode_string& name) const;
  bool ValidParent(const CJavaClassFile& child,
		   const CJavaClassFile& parent) const;
  bool ValidInterface(const CJavaClassFile& child,
		      const CJavaClassFile& interface) const;
  bool DescendsFrom(const CJavaClassFile& child,
		    const CJavaClassFile& ancestor) const;
  bool AssignableSubtype(const CJavaTypeSignature& childType,
			 const CJavaTypeSignature& ancestorType) const;
  bool CastableType(const CJavaTypeSignature& currentType,
		    const CJavaTypeSignature& toType) const;
  bool SameType(const CJavaTypeSignature& first,
		const CJavaTypeSignature& second) const;
  bool SameType(const CJavaMethodSignature& first,
		const CJavaMethodSignature& second) const;
  bool ImplicitCastTo(const CJavaTypeSignature& from,
		      const CJavaTypeSignature& to) const;
  bool SameField(const CJavaFieldSignature& first,
		 const CJavaFieldSignature& second) const;
  CCompileError* CheckType(const CJavaTypeSignature& type) const;
  CJavaTypeSignature FixType(const CJavaTypeSignature& oldType) const;
  unicode_string NameClassConstant(const CJavaTypeSignature& type) const;
  const CJavaClassFile* FindField(const unicode_string& field,
				  const CJavaClassFile& fromClass,
				  CJavaFieldInfo& setFieldInfo) const;
  CCompileError* CheckValidOverride(const CJavaClassFile& parentClass,
				    const CIntermediateFunction& method);
  CCompileError* FindConstantField(CExpression*& result,
      const unicode_string& className, const CJavaFieldSignature& field,
      const CCompileContext& context, unsigned long lineNumber) const;
  CCompileError* FindIntermediateConstantField(CExpression*& result,
				      const CIntermediateClass& intermediate,
				      const CJavaFieldInfo& fieldInfo) const;
  static CExpression* CreateLiteralFromConstant(unsigned short index,
		const CJavaClassFile& onClass, const CJavaTypeSignature& type);
  static unsigned short CreateConstantFromLiteral(const CExpression* literal,
						  CJavaClassFile& onClass);
  pair<const CJavaClassFile*, const CJavaMethodInfo*>*
    MatchMethod(const unicode_string& methodName,
  		const CJavaClassFile& fromClass,
		const deque<CJavaTypeSignature>& arguments,
		string*& errorString);
  pair<const CJavaClassFile*, const CJavaMethodInfo*>*
    MatchConstructor(const CJavaClassFile& fromClass,
		     const deque<CJavaTypeSignature>& arguments,
		     ExpressionList::const_iterator& visibleBegin,
		     ExpressionList::const_iterator& visibleEnd,
		     string*& errorString);
  const CJavaMethodInfo*
    MatchClassMethod(const CJavaClassFile& onClass,
		     const unicode_string& name,
		     const deque<CJavaTypeSignature>& arguments,
		     string*& errorString) const;
  pair<const CJavaClassFile*, const CJavaMethodInfo*>*
    ExactMatchMethod(const CJavaMethodSignature& method,
		     const CJavaClassFile& fromClass);
  const CJavaMethodInfo*
    ExactMatchClassMethod(const CJavaMethodSignature& method,
			  const CJavaClassFile& onClass) const;
  bool MoreSpecific(const CJavaMethodSignature& method1,
		    const CJavaMethodSignature& method2) const;

  typedef list<CJavaMethodSignature> MethodList;
  unsigned long UnimplementedMethods(const CJavaClassFile& onClass,
				     MethodList& methods);
  unsigned long UndeclaredInterfaceMethods(const CJavaClassFile& onClass,
					   MethodList& methods);

  bool IsThrowable(const CJavaTypeSignature& type) const;
  bool InClassInitializers() const { return fInClassInitializers; }

  typedef vector<unsigned long> FinallyHandlerStack;
  void PushFinallyHandler(unsigned long instruction);
  unsigned long PopFinallyHandler();
  FinallyHandlerStack::iterator GetHandlerBegin();
  FinallyHandlerStack::iterator GetHandlerEnd();
  
  void PushStatementContext(CStatement* statement);
  void PopStatementContext();
  StatementList::iterator GetContextBegin();
  StatementList::iterator GetContextEnd();

  void GenerateSyntheticConstructorArguments(const unicode_string& className,
					     ExpressionList& into) const;

  void WarnDeprecatedField(const CJavaFieldInfo& field,
			   const CJavaClassFile& fieldClass,
			   const CCompileContext& context,
			   unsigned long line = 0) const;
  void WarnDeprecatedMethod(const CJavaMethodInfo& method,
			   const CJavaClassFile& methodClass,
			   const CCompileContext& context,
			   unsigned long line = 0) const;
  void WarnDeprecatedClass(const CJavaClassFile& usedClass) const;

  static const unsigned long kVersionID;
  static const unicode_string kConstructorName;
  static const unicode_string kStaticName;
  static const unicode_string kObjectName;
  static const unicode_string kThrowableName;
  static const unicode_string kErrorName;
  static const unicode_string kRuntimeExceptionName;

  static const unicode_string kSyntheticDivider;
  static const unicode_string kOuterThisName;
  static const unicode_string kSyntheticFieldPrefix;

  static const string kDefaultImport;
protected:
  CCompiler();
  CCompiler(const deque<string>& classPath);
  ~CCompiler();

  bool CheckJavaLangObject();
  bool ParsedFile(const string& fileName) const;
  bool ParseFile(const string& fileName);
  bool CheckImports(const string& fileName);
  typedef deque<CIntermediateClass*> IntermediateList;
  bool PartialCompileFile(const string& fileName,
			  IntermediateList& resultClasses);

  typedef list<CJavaDirectory> ClassPathList;
  CJavaClassFile* ImportOneClassPath(const unicode_string& className,
				     const CJavaDirectory& classPath);

  void PrepareIntermediateClass(CIntermediateClass& intermediate,
				const unicode_string& fileName);
  void RemoveIntermediateClass(const CIntermediateClass* intermediate);
  bool PrepareClassDeclarations(CIntermediateClass& intermediate);
  void AddSyntheticConstructorParams(CIntermediateClass& inClass,
				     CIntermediateFunction& intermediate);
  bool GenerateClass(CIntermediateClass& intermediate);
  CCompileError* GenerateCode(CIntermediateClass& intermediate,
			      CIntermediateFunction& intermediateMethod,
			      CJavaMethodInfo& realMethod);
  bool GenerateFieldConstants(CIntermediateClass& intermediate);
  CCompileError* GenerateMethod(CIntermediateClass& intermediateClass,
				CIntermediateFunction& intermediateMethod);
  void GenerateSyntheticCode(CCompileContext& context,
			     CCodeSequence& code);
  void FixAnonymousSyntheticConstructor(CIntermediateClass& intermediateClass,
					CIntermediateFunction& constructor,
					const CJavaMethodInfo& parentInfo);

  const CIntermediateClass*
                LookupIntermediateClass(const unicode_string& name) const;
  const CJavaClassFile* LookupInnerClass(const unicode_string& name,
			 const CIntermediateClass* classContext,
			 const CIntermediateFunction* methodContext) const;

  void PrintCompileError(const string& fileName, const string& message,
			 unsigned long line = 0) const;
  void PrintCompileError(const string& fileName, const unicode_string& message,
			 unsigned long line = 0) const;
			 

  string fFileName;
  deque<string> fParsedFiles;
  ClassPathList fClassPath;
  ClassPathList fImportedPackages;
  ClassList fImportedClasses;
  typedef map<unicode_string, const CJavaClassFile*, less<unicode_string> >
     ImportTable;
  ImportTable fImportAliases;
  unicode_string fPackageName;
  IntermediateList fIntermediateClasses;
  bool fParseError;
  deque<string> fDependencies;
  deque<unicode_string> fClassImports;
  vector<unsigned long> fClassImportLocations;
  deque<unicode_string> fPackageImports;
  vector<unsigned long> fPackageImportLocations;
  bool fInClassInitializers;
  StatementList fStatementContexts;
  FinallyHandlerStack fFinallyHandlers;

  deque<CIntermediateClass*> fParsingClasses;

};

#endif
