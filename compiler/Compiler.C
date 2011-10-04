// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: Compiler.C,v 1.24 1997/11/10 00:48:06 geppetto Exp $
#pragma implementation
#include "unicode_string.h"
#include "Compiler.h"
#include "parser_decls.h"
#include "FilePath.h"
#include "JavaClassFile.h"
#include <iostream.h>
#include <fstream.h>
#include "IntermediateClass.h"
#include "IntermediateFunction.h"
#include "JavaTypeSignature.h"
#include "parser_decls.h"
#include "Statement.h"
#include "JavaAttribute.h"
#include "JavaExceptionsTable.h"
#include "JavaCodeAttribute.h"
#include "CompileError.h"
#include "Expression.h"
#include "CompileContext.h"
#include "CodeSequence.h"
#include <cstdio>
#include <climits>
#include <algorithm>
extern "C" {
  #include <unistd.h>
}

const unicode_string CCompiler::kConstructorName = ::StringToUnicode("<init>");
const unicode_string CCompiler::kStaticName = ::StringToUnicode("<clinit>");
const string CCompiler::kDefaultImport("java/lang");
const unicode_string CCompiler::kObjectName =
                                    ::StringToUnicode("java/lang/Object");
const unicode_string CCompiler::kThrowableName =
                                    ::StringToUnicode("java/lang/Throwable");
const unicode_string CCompiler::kErrorName =
                                    ::StringToUnicode("java/lang/Error");
const unicode_string CCompiler::kRuntimeExceptionName =
                               ::StringToUnicode("java/lang/RuntimeException");
const unicode_string CCompiler::kSyntheticDivider = ::StringToUnicode("$");
const unicode_string CCompiler::kOuterThisName = ::StringToUnicode("this$0");
const unicode_string CCompiler::kSyntheticFieldPrefix = 
                                   ::StringToUnicode("val$");
const unsigned long CCompiler::kVersionID = 0x3002d;  // beta API

//
//  Method name : CCompiler
//  Description : Default constructor.  Assumes that all package class names
//    will be relative to the current working directory.  ( ./ )
//
CCompiler::CCompiler()
  : fClassPath(),
    fPackageName(),
    fParseError(false)
{
}

//
//  Method name : CCompiler
//  Description : Constructor.  Takes a list containing all of the root
//    directories to be searched by this compiler.  For every package name
//    used in the source, each of these directory trees is searched in the
//    provided order.
//    Each of the provided directories are checked for validity (make sure
//    they are readable directories) before use.
//    If the provided list is empty, then it will just search the current
//    working directory.  (like the default constructor)
//
CCompiler::CCompiler(const deque<string>& classPath)
  : fClassPath(),
    fPackageName(),
    fParseError(false)
{
  for (deque<string>::const_iterator i = classPath.begin();
       i < classPath.end(); i++) {
    CJavaDirectory oneDirectory(*i);
    if (oneDirectory.IsValid()) {
      fClassPath.push_back(oneDirectory);
    }
  }
  if (fClassPath.size() < 1) {
    fClassPath.push_back(CJavaDirectory("."));
  }
}

//
//  Method name : ~CCompiler
//  Description : Destructor
//
CCompiler::~CCompiler()
{
  for (ClassList::iterator i = fImportedClasses.begin();
       i != fImportedClasses.end(); i++) {
    delete *i;
  }
  IntermediateList::iterator classes = fIntermediateClasses.begin();
  for (; classes != fIntermediateClasses.end(); classes++) {
    delete *classes;
  }
  classes = fParsingClasses.begin();
  for (; classes != fParsingClasses.end(); classes++) {
    delete (*classes)->fCurrentFunction;
  }
}

//
//  Method name : CompileFile
//  Description : This static function can be used to compile a Java file.
//
bool
CCompiler::CompileFile(const string& fileName,
		       const deque<string>& classPath,
		       CCompiler::ClassList& resultClasses,
		       deque<string> &dependencies)
{
  CCompiler compiler(classPath);
  if (!compiler.CheckJavaLangObject()) {
    cerr << "Could not find " << UnicodeToString(kObjectName) <<
      " in the class path." << endl;
    return false;
  } else {
    compiler.fDependencies.push_back(fileName);
    IntermediateList intermediates;
    bool success = compiler.PartialCompileFile(fileName, intermediates);
    if (success) { 
      unicode_string unicodeFilename = ::StringToUnicode(fileName);
      IntermediateList::iterator begin = intermediates.begin();
      IntermediateList::iterator end = intermediates.end();
      IntermediateList::iterator classIterator;
      for (classIterator = begin; classIterator != end; ++classIterator) {
	if (! compiler.GenerateClass(*(*classIterator))) {
	  success = false;
	  break;
	}
      }
      for (classIterator = begin; classIterator != end; ++classIterator) {
	if ((*classIterator)->fRealClass != 0) {
	  resultClasses.push_back((*classIterator)->fRealClass);
	  (*classIterator)->fRealClass = 0;
	}
      }
      dependencies = compiler.fDependencies;
    }
    return success;
  }
}

//
//  Method name : PartialCompileFile
//  Description : This method takes the provided Java source file name and
//    parses it for all of its interface information.  This does not do any
//    compilation of the internals of method bodies ... this must be done
//    by a later pass.  The second parameter (resultClasses) is filled with
//    the classes that are found in this file.
//
bool
CCompiler::PartialCompileFile(const string& fileName,
			      CCompiler::IntermediateList& resultClasses)
{
  unsigned long previousClassCount = fIntermediateClasses.size();
  bool success = !ParsedFile(fileName);
  if (success) {
    fParsedFiles.push_back(fileName);
    if (success = ParseFile(fileName)) {
      unicode_string unicodeFilename = ::StringToUnicode(fFileName);
      IntermediateList::iterator begin =
	fIntermediateClasses.begin() + previousClassCount;
      IntermediateList::iterator end = fIntermediateClasses.end();
      IntermediateList::iterator classIterator;
      for (classIterator = begin; classIterator != end; ++classIterator) {
	PrepareIntermediateClass(*(*classIterator), unicodeFilename);
      }
      for (classIterator = begin; classIterator != end; ++classIterator) {
	if (!PrepareClassDeclarations(*(*classIterator))) {
	  success = false;
	  break;
	}
	resultClasses.push_back(*classIterator);
      }
    }
  }
  return success;
}

//
//  Method name : ParsedFile
//  Description : Returns true if this file has already been parsed by this
//    compiler.
//
bool
CCompiler::ParsedFile(const string& fileName) const
{
  CFilePath filePath(fileName);
  for (deque<string>::const_iterator file = fParsedFiles.begin();
       !(file == fParsedFiles.end()); ++file) {
    CFilePath parsedFile(*file);
    if (parsedFile == filePath) {
      return true;
    }
  }
  return false;
}

//
//  Method name : ParseFile
//  Description : This method is used during compilation to parse one input
//    file.  If this operation is successful, it return true.
//
bool
CCompiler::ParseFile(const string& fileName)
{
  FILE* infile = fopen(fileName.c_str(), "r");
  if (infile == 0) {
    cerr << "Could not open input file: " << fileName << endl;
    return false;
  }
  string textToParse;
  int inChar;
  while ((inChar = fgetc(infile)) != EOF) {
    textToParse += (char)inChar;
  }
  fclose(infile);
  string::size_type start = fileName.find_last_of('/');
  start = (start == string::npos) ? 0 : start + 1;
  fFileName.assign(fileName, start);
  if (!ImportPackage(::StringToUnicode(kDefaultImport))) {
    cerr << "Cannot find required package " << kDefaultImport
	 << " in class path." << endl;
  }
  InitializeParser(textToParse, this);
  int parsed = ::yyparse();
  yylineno = 0;
  bool success = parsed == 0 && !fParseError;
  ::FinishParser();
  return success && CheckImports(fileName);
}

//
//  Method name : AddClassImport
//  Description : This method is called during parsing to indicate a class
//    that should be imported after parsing is complete.
//
void
CCompiler::AddClassImport(const unicode_string& className)
{
  fClassImports.push_back(className);
  fClassImportLocations.push_back(yylineno);
}

//
//  Method name : AddPackageImport
//  Description : This method is called during parsing to indicate a package
//    that should be imported after parsing is complete.
//
void
CCompiler::AddPackageImport(const unicode_string& package)
{
  fPackageImports.push_back(package);
  fPackageImportLocations.push_back(yylineno);
}

//
//  Method name : CheckImports
//  Description : This method is used internally to check whether the imports
//    that were found in a java file are valid.
//
bool
CCompiler::CheckImports(const string& fileName)
{
  bool valid = true;
  int index = 0;
  for (deque<unicode_string>::iterator i = fClassImports.begin();
       !(i == fClassImports.end()); ++i, ++index) {
    if (ImportClass(*i) == 0) {
      PrintCompileError(fileName, "Invalid class: " + ::UnicodeToUTF(*i),
			fClassImportLocations[index]);
      valid = false;
    }
  }
  index = 0;
  for (deque<unicode_string>::iterator i = fPackageImports.begin();
       !(i == fPackageImports.end()); ++i, ++index) {
    if (ImportPackage(*i) == 0) {
      PrintCompileError(fileName, "Invalid package: " + ::UnicodeToUTF(*i),
			fPackageImportLocations[index]);
      valid = false;
    }
  }
  return valid;
}

//
//  Method name : ImportClass
//  Description : This looks for the class with the provided qualified name
//    by searching the class path given to this compiler as an argument.  If
//    the desired class is found, it is loaded in and stored for later
//    reference and a pointer to it is returned.  If it is not found, then
//    0 is returned.
//    The 'aliasBase' argument is used to specify whether the base class
//    name should be visible to the rest of the program.  For example, the
//    qualified class sun.net.www.URLConnection would have a base name
//    'URLConnection', which should be globally visible if someone used an
//    import statement, but not visible if they used a fully qualified class
//    name as a type.
//
const CJavaClassFile*
CCompiler::ImportClass(const unicode_string& className, bool aliasBase)
{
  const CJavaClassFile* classFile = 0;
  string classString = ::UnicodeToUTF(className);
  unicode_string::size_type lastSlash = className.find_last_of('/');

  bool isSimpleClass = lastSlash == unicode_string::npos;
  if (isSimpleClass) {
    for (ClassPathList::const_iterator i = fImportedPackages.begin();
	 classFile == 0 && !(i == fImportedPackages.end()); ++i) {
      classFile = ImportOneClassPath(className, *i);
      if (classFile != 0) {
	unicode_string fullName = classFile->GetClassName();
	// this assignment just reduces template instantiations...
	const ImportTable* aliases = &fImportAliases;
	ImportTable::const_iterator entry = aliases->find(fullName);
	if (entry != aliases->end()) {
	  classFile = (*entry).second;
	} else {
	  fImportAliases.insert(ImportTable::value_type(fullName, classFile));
	}
	if (!(fullName == className)) {
	  fImportAliases.insert(ImportTable::value_type(className,
							classFile));
	}
	break;
      }
    }
  }
  ImportTable::iterator exists = fImportAliases.find(className);
  if (classFile == 0 && !(exists == fImportAliases.end())) {
    classFile = (*exists).second;
  }
  for (ClassPathList::const_iterator i = fClassPath.begin();
       classFile == 0 && !(i == fClassPath.end()); ++i) {
    classFile = ImportOneClassPath(className, *i);
    if (classFile != 0) {
      fImportAliases.insert(ImportTable::value_type(className, classFile));
      CJavaDirectory thisClassPath(*i,
				 ::UnicodeToUTF(classFile->GetPackageName()));
      if (!isSimpleClass) {
	bool addAlias = aliasBase;
	if (!addAlias) {
	  addAlias = true;
	  for (ClassPathList::const_iterator package =
		 fImportedPackages.begin();
	       addAlias && !(package == fImportedPackages.end()); ++package) {
	    if (*package == thisClassPath) {
	      addAlias = false;
	    }
	  }
	}
	if (addAlias) {
	  unicode_string shortName(className, lastSlash + 1);
	  if (fImportAliases.find(shortName) == fImportAliases.end()) {
	    fImportAliases.insert(ImportTable::value_type(shortName,
							  classFile));
	  }
	}
      }
    }
  }
  return classFile;
}

//
//  Method name : ImportOneClassPath
//  Description : This is a helper method used by ImportClass.  It looks for
//    the Java class with the provided name relative to the provided java
//    directory.  If it is successful, the class is added to the import list
//    on this compiler and the class is returned.  Otherwise, it returns 0.
//
CJavaClassFile*
CCompiler::ImportOneClassPath(const unicode_string& className,
			      const CJavaDirectory& classPath)
{
  CJavaClassFile* classFile = classPath.LoadClassFile(className);
  string utfClassName = ::UnicodeToUTF(className);
  if (classFile == 0 && !classPath.InZipFile()) {
    string sourceName = classPath.GetRealDirectory() + "/" +
      utfClassName + ".java";
    CFilePath sourcePath(sourceName);
    if (sourcePath.IsFile() && sourcePath.IsReadable()) {
      // cout << "==== loading from source: " << sourceName
      //      << "  classFile: (" << classFile << ")" << endl;
      IntermediateList resultClasses;
      if (PartialCompileFile(sourceName, resultClasses)) {
	for (IntermediateList::iterator i = resultClasses.begin();
	     !(i == resultClasses.end()); ++i) {
	  if (className == (*i)->GetShortName() &&
	      (*i)->fAccessFlags.fPublic) {
	    classFile = (*i)->fRealClass;
	    (*i)->fRealClass = 0;
	    RemoveIntermediateClass(*i);
	    delete (*i);
	    break;
	  }
	}
      }
    }
  }
  if (classFile != 0) {
    string actualName = ::UnicodeToUTF(classFile->GetClassName());
    string requiredName = utfClassName;
    string packageName = classPath.GetPackage();
    if (packageName.length() > 1) {
      requiredName = packageName + requiredName;
    }
    if (!classPath.InZipFile()) {
      string depend =
	classPath.GetRealDirectory() + utfClassName + ".java";
      if (access(depend.c_str(), F_OK) == 0) {
	fDependencies.push_back(depend);
      }
    }
    if (!(actualName == requiredName)) {
      cerr << "Invalid class " << actualName << " found in " <<
	classPath.GetRealDirectory() << endl;
      delete classFile;
      classFile = 0;
    } else {
      fImportedClasses.push_back(classFile);
      WarnDeprecatedClass(*classFile);
    }
  }
  return classFile;
}

//
//  Method name : ImportPackage
//  Description : This looks for the package with the provided qualified
//    name and imports all of the classes directly in that package level
//    using ImportClass.  If the package doesn't exist or can't be accessed,
//    false is returned, otherwise true is returned.  (This implies that
//    true will be returned even when some or all of the class files in
//    the package aren't loadable.)
//
bool
CCompiler::ImportPackage(const unicode_string& packageName)
{
  string packageString = ::UnicodeToUTF(packageName);
  bool found = false;
  for (ClassPathList::const_iterator i = fClassPath.begin();
       !(i == fClassPath.end()); i++) {
    CJavaDirectory relativePath(*i, packageString);
    if (relativePath.IsValid()) {
      fImportedPackages.push_back(relativePath);
      found = true;
    }
  }
  return found;
}

//
//  Method name : SetPackage
//  Description : Specifies what the package is for the compilation unit that
//    is currently being compiled.
//
void
CCompiler::SetPackage(const unicode_string& packageName)
{
  fPackageName = packageName;
  ImportPackage(packageName);
}

//
//  Method name : NoPackage
//  Description : Specifies that the current compilation unit has no package
//    specified.
//
void
CCompiler::NoPackage()
{
  SetPackage(unicode_string());
}

//
//  Method name : StartClass
//  Description : When the parser hits the beginning of a class declaration,
//    it uses this call to tell the compiler object the important class
//    header information and whatnot.  This keeps track of a current class
//    of evaluation that all future intra-class operations will be applied to.
//
void
CCompiler::StartClass(const unicode_string& className,
		      CJavaAccessFlags* adoptModifiers,
		      unicode_string* adoptExtends,
		      deque<unicode_string>* adoptInterfaces,
		      bool deprecated)
{
  CIntermediateClass* current = GetCurrentClass();
  unicode_string fullName;
  if (current != 0) {
    if (current->fCurrentFunction == 0) {
      fullName = current->fName + kSyntheticDivider + className;
    } else {
      char numString[16];
      ::sprintf(numString, "%i", current->NextSyntheticIndex());
      fullName = current->fName + kSyntheticDivider +
	::StringToUnicode(numString);
      if (className.length() > 0) {
	fullName += kSyntheticDivider + className;
      }
    }
  } else if (fPackageName.size() > 0) {
    fullName = fPackageName;
    fullName += (unicode_char)'/';
    fullName += className;
  } else {
    fullName = className;
  }
  CIntermediateClass* intermediate =
    new CIntermediateClass(fFileName, fullName, className, adoptModifiers,
			   adoptExtends, adoptInterfaces, deprecated);
  intermediate->fInsideClass = current;
  intermediate->fIsInner =
    (current != 0 && !intermediate->fAccessFlags.fStatic);
  CJavaClassFile* realClass = new CJavaClassFile;
  intermediate->fRealClass = realClass;
  realClass->fVersion = kVersionID;
  unicode_string sourceName = ::UTFToUnicode(fFileName);
  realClass->fSourceFile = new CJavaSourceFileAttribute(sourceName);
  realClass->fAccessFlags = intermediate->fAccessFlags;
  realClass->fThisClassName = fullName;
  realClass->fDeprecated = deprecated;
  realClass->fInterfaces = intermediate->fInterfaces;
  fParsingClasses.push_back(intermediate);

  if (current != 0) {
    current->fRealClass->AddInnerClass(fullName , className,
				       intermediate->fAccessFlags);
    realClass->AddInnerClass(fullName, className, intermediate->fAccessFlags,
			     current->fRealClass->GetClassName());
    if (current->fCurrentFunction == 0) {
      current->fInnerClasses.push_back(intermediate);
    } else {
      current->fCurrentFunction->fInnerClasses.push_back(intermediate);
    }
    if (intermediate->fIsInner) {
      CJavaAccessFlags* thisFlags = new CJavaAccessFlags;
      thisFlags->fPrivate = thisFlags->fFinal = 1;
      deque<CVariableDeclaration*>* declarations =
	new deque<CVariableDeclaration*>;
      CJavaTypeSignature type(current->fName);
      declarations->push_back(new CVariableDeclaration(type, kOuterThisName));
      AddField(new CDeclarationStatement(declarations, thisFlags));
    }  
  } else if (intermediate->fAccessFlags.fPublic) {
    unicode_string baseFile(sourceName, 0, sourceName.find_last_of('.'));
    if (!(baseFile == intermediate->GetShortName())) {
      unicode_string errorMessage =
	::UTFToUnicode("Warning: ") + intermediate->fName +
	::UTFToUnicode(" may not be declared public in a file named ") +
	sourceName;
      PrintCompileError(fFileName, errorMessage, yylineno);
    }
  }
  fIntermediateClasses.push_back(intermediate);
}


//
//  Method name : StartInterface
//  Description : This function signals the beginning of an interface
//    declaration in the file that is currently being parsed.  The compiler
//    object sets things up so that future fields will be accepted and
//    understood.
//
void
CCompiler::StartInterface(const unicode_string& className,
			  CJavaAccessFlags* adoptModifiers,
			  deque<unicode_string>* adoptInterfaceExtends,
			  bool deprecated)
{
  StartClass(className, adoptModifiers, 0, adoptInterfaceExtends, deprecated);
}

//
//  Method name : StartAnonymousClass
//  Description : This method is used to indicate the beginning of an
//    unnamed inner class.  The provided parent name could be either a
//    class or an interface, so this gets pretty ugly in a hurry.
//    This method returns the synthetic name of the new class.
//
unicode_string
CCompiler::StartAnonymousClass(unicode_string* adoptParentName)
{
  CJavaAccessFlags* flags = new CJavaAccessFlags();
  flags->fPrivate = 1;
  StartClass(unicode_string(), flags, adoptParentName, 0, false);
  CIntermediateClass* currentClass = GetCurrentClass();
  currentClass->fIsAnonymous = true;
  return currentClass->fName;
}

//
//  Method name : EndClass
//  Description : This method is used by the parser to tell the compiler
//    that the current class scope has ended.  This allows the compiler
//    to clean up and prepare for the next class or interface in the file.
//
void
CCompiler::EndClass()
{
  CIntermediateClass* currentClass = GetCurrentClass();
  assert(currentClass->fCurrentFunction == 0);
  if (!currentClass->fAnyConstructors &&
      currentClass->fAccessFlags.fInterface == 0) {
    CJavaAccessFlags flags;
    flags.fPublic = true;
    StartConstructor(flags, 0);
    EndFunction(new CCompoundStatement(0));
  }
  if (currentClass->fStaticInitializer != 0 ||
      currentClass->fStaticDeclarations.size() > 0) {
    static const deque<CJavaTypeSignature> noArguments;
    static const CJavaTypeSignature staticReturnType(CJavaTypeSignature::Void);
    static const CJavaMethodSignature staticSignature(staticReturnType,
						   kStaticName, noArguments);
    CJavaAccessFlags modifiers;
    modifiers.fStatic = 1;
    CIntermediateFunction* staticFunction = 
      new CIntermediateFunction(staticSignature, modifiers, 0, yylineno);
    if (currentClass->fStaticInitializer != 0) {
      staticFunction->fBlock = currentClass->fStaticInitializer;
      currentClass->fStaticInitializer = 0;
    } else {
      staticFunction->fBlock = new CCompoundStatement(new StatementList);
    }
    staticFunction->fMaxLocalVariables = currentClass->fStaticLocalVariables;
    currentClass->fFunctions.push_back(staticFunction);
  }
  fParsingClasses.pop_back();
}


//
//  Method name : StartConstructor
//  Description : This method signifies the beginning of a constructor
//    function, which is specified slightly differently than a normal
//    function, which has return types and whatnot.
//
void
CCompiler::StartConstructor(const CJavaAccessFlags& modifiers,
			    deque<CVariableDeclaration>* adoptParameters,
			    deque<unicode_string>* adoptThrows,
			    bool deprecated)
{
  static CJavaTypeSignature type(CJavaTypeSignature::Void);
  CIntermediateClass* currentClass = GetCurrentClass();
  currentClass->fAnyConstructors = true;
  if (currentClass->fIsInner) {
    CJavaTypeSignature type(currentClass->fInsideClass->fName);
    CVariableDeclaration declaration(type, kOuterThisName);
    if (adoptParameters == 0) {
      adoptParameters = new deque<CVariableDeclaration>;
    }
    adoptParameters->push_front(declaration);
  }
  StartFunction(type, kConstructorName, modifiers, adoptParameters,
		adoptThrows, deprecated);
}

//
//  Method name : StartFunction
//  Description : Used by the parser to signal the beginning of a new method
//    in a class.  All of the provided information is grabbed out of the
//    function header before the body is parsed, and the compiler has a
//    chance here to set up the appropriate scopes and other information.
//
void
CCompiler::StartFunction(const CJavaTypeSignature& returnType,
			 const unicode_string& functionName,
			 const CJavaAccessFlags& modifiers,
			 deque<CVariableDeclaration>* adoptParameters,
			 deque<unicode_string>* adoptThrows,
			 bool deprecated)
{
  assert(!fParsingClasses.empty());
  CIntermediateClass* currentClass = GetCurrentClass();
  assert(currentClass->fCurrentFunction == 0);
  assert(currentClass->fLocalVariables.size() == 0);
  assert(currentClass->fVariableScopes.size() == 0);
  assert(currentClass->fCurrentLocalVariable == 0);
  CJavaAccessFlags newModifiers = modifiers;
  if (modifiers.fStatic == 0) {
    currentClass->fCurrentLocalVariable = 1;
  }

  if (currentClass->fAccessFlags.fInterface != 0) {
    newModifiers.fAbstract = 1;
    newModifiers.fPublic = 1;
  }
  PushLocalScope();
  deque<CJavaTypeSignature> parameterTypes;
  if (adoptParameters != 0) {
    for (deque<CVariableDeclaration>::iterator i = adoptParameters->begin();
	 i != adoptParameters->end(); ++i) {
      AddLocalVariable(*i);
      parameterTypes.push_back((*i).GetType());
    }
    delete adoptParameters;
  }
  CJavaMethodSignature signature(returnType, functionName, parameterTypes);
  currentClass->fCurrentFunction =
    new CIntermediateFunction(signature, newModifiers,
			      adoptThrows, yylineno, deprecated);
  currentClass->fCurrentFunction->fRealParametersSize =
    currentClass->fCurrentLocalVariable;
}


//
//  Method name : EndFunction
//  Description : This is used by the parser to signal the end of a Java
//    function.  The compiler uses this to clean up any global state and
//    close out the last scope.
//
void
CCompiler::EndFunction(CCompoundStatement* statementBlock)
{
  CIntermediateClass* currentClass = GetCurrentClass();
  assert(currentClass->fCurrentFunction != 0);
  assert(!fParsingClasses.empty());
  PopLocalScope();
  // assert(fVariableScopes.size() == 0);
  currentClass->fVariableScopes.erase(currentClass->fVariableScopes.begin(),
				      currentClass->fVariableScopes.end());
  // assert(fLocalVariables.size() == 0);
  currentClass->fLocalVariables.erase(currentClass->fLocalVariables.begin(),
				      currentClass->fLocalVariables.end());
  currentClass->fCurrentFunction->fMaxLocalVariables =
    currentClass->fCurrentLocalVariable;
  currentClass->fCurrentFunction->fBlock = statementBlock;
  currentClass->fCurrentFunction->fEndLineNumber = yylineno;
  currentClass->fFunctions.push_back(currentClass->fCurrentFunction);
  currentClass->fCurrentFunction = 0;
  currentClass->fCurrentLocalVariable = 0;
}


//
//  Method name : PushLocalScope
//  Description : This function is used to signify a new local scope in an
//    active method.  This allows new variables to be declared that will be
//    removed from visibility after the scope exits.
//
void
CCompiler::PushLocalScope()
{
  GetCurrentClass()->fVariableScopes.push_front(deque<unicode_string>());
}


//
//  Method name : PopLocalScope
//  Description : This is the converse of PushLocalScope ... it tells the
//    compiler that the parser is leaving a local variable scope so it
//    should remove all local variables that were in the current scope.
//
void
CCompiler::PopLocalScope()
{
  CIntermediateClass* currentClass = GetCurrentClass();
  assert(currentClass->fVariableScopes.size() > 0);
  deque<unicode_string>::iterator end =
    currentClass->fVariableScopes.front().end();
  deque<unicode_string>::iterator i =
    currentClass->fVariableScopes.front().begin();
  for (; i != end; i++) {
    CIntermediateClass::LocalVariableTable::iterator variable =
      currentClass->fLocalVariables.find(*i);
    assert(variable != currentClass->fLocalVariables.end());
    currentClass->fLocalVariables.erase(variable);
  }
  currentClass->fVariableScopes.pop_front();
}

//
//  Method name : AddLocalVariable
//  Description : Adds a new local variable to the current scope with the
//    provided name.  The compiler assigns it the next available variable
//    index.  (Doesn't bother re-using out-of-scope ones since Sun's doesn't
//    seem to do this, either.)
//    Returns the index of the new variable.
//
unsigned short
CCompiler::AddLocalVariable(const CVariableDeclaration& declaration)
{
  unicode_string name = declaration.GetName();
  CIntermediateClass* currentClass = GetCurrentClass();
  (currentClass->fVariableScopes.front()).push_front(name);
  unsigned short index = currentClass->fCurrentLocalVariable;
  currentClass->fLocalVariables.insert(
    CIntermediateClass::LocalVariableTable::value_type(name,
			       CLocalVariableRecord(declaration, index)));
  currentClass->fCurrentLocalVariable += declaration.GetType().GetWidth();
  return index;
}

//
//  Method name : AddUnnamedVariable
//  Description : This method is used to reserve space for a local variable
//    entry which is never referenced by name.  This is needed for subroutine
//    handling used by the 'finally' part of a 'try' statement, which stores
//    an entry in a local variable index.
//    Returns the index of the new local variable.
//
unsigned short
CCompiler::AddUnnamedVariable(unsigned long width)
{
  CIntermediateClass* currentClass = GetCurrentClass();
  unsigned short index = currentClass->fCurrentLocalVariable;
  currentClass->fCurrentLocalVariable += width;
  return index;
}

//
//  Method name : LookupLocalVariable
//  Description : Goes into the local variable table and checks to see if
//    the variable with the provided name has been declared in the current
//    scope.  If it has, a pair encapsulating the local variable index for
//    code generation and the signature of the variable is returned.
//    If not, 0 is returned.
//
const CLocalVariableRecord*
CCompiler::LookupLocalVariable(const unicode_string& name) const
{
  const CIntermediateClass* currentClass = GetCurrentClass();
  CIntermediateClass::LocalVariableTable::const_iterator match =
    currentClass->fLocalVariables.find(name);
  if (match != currentClass->fLocalVariables.end()) {
    return &((*match).second);
  } else {
    return 0;
  }
}

//
//  Method name : LookupOuterLocalVariable
//  Description : If the current class is an inner class within the block
//    of another class, this will try to find a named local variable in
//    that block.  If this is unsuccessful for any reason, it returns 0.
//
const COuterLocalExpression*
CCompiler::LookupOuterLocalVariable(const unicode_string& name) const
{
  const CIntermediateClass* currentClass = GetCurrentClass();
  const CIntermediateClass* inside = currentClass->fInsideClass;
  if (inside != 0 && inside->fCurrentFunction != 0) {
    for (list<COuterLocalExpression*>::const_iterator i =
	   currentClass->fSyntheticLocals.begin();
	 i != currentClass->fSyntheticLocals.end(); ++i) {
      if (name == (*i)->GetLabel()) {
	return *i;
      }
    }
    CIntermediateClass::LocalVariableTable::const_iterator match =
      inside->fLocalVariables.find(name);
    if (match != inside->fLocalVariables.end()) {
      CVariableDeclaration declaration = (*match).second.GetDeclaration();
      COuterLocalExpression* result =
	new COuterLocalExpression(declaration,
				  (*match).second.GetVariableIndex());
      // casting away 'const' to cache.
      ((CIntermediateClass*)currentClass)->fSyntheticLocals.push_back(result);

      CJavaAccessFlags* flags = new CJavaAccessFlags;
      flags->fPrivate = flags->fFinal = 1;
      deque<CVariableDeclaration*>* declarations =
	new deque<CVariableDeclaration*>;
      declarations->push_back(
         new CVariableDeclaration(declaration.GetType(),
			  kSyntheticFieldPrefix + declaration.GetName()));
      // casting away 'const' to cache.
      ((CCompiler*)this)->AddField(
		   new CDeclarationStatement(declarations, flags));
      return result;
    }
  }
  return 0;
}

//
//  Method name : AddField
//  Description : This method is used to add a data field to the current
//    class or interface.  The declaration is examined to see whether or not
//    it is static to determine where the code goes.
//
void
CCompiler::AddField(CDeclarationStatement* declaration)
{
  assert(!fParsingClasses.empty());
  assert(declaration != 0);
  assert(declaration->fDeclarations != 0);
  assert(declaration->fModifiers != 0);
  if (GetCurrentClass()->fAccessFlags.fInterface != 0) {
    declaration->fModifiers->fStatic = 1;
    declaration->fModifiers->fFinal = 1;
  }
  if (declaration->fModifiers->fStatic != 0) {
    GetCurrentClass()->fStaticDeclarations.push_back(declaration);
  } else {
    GetCurrentClass()->fNonStaticDeclarations.push_back(declaration);
  }
}

//
//  Method name : AddStaticCode
//  Description : This adds some static code to the current class, which will
//    be added to the <clinit> pseudo-method and executed at class load time.
//
void
CCompiler::AddStaticCode(CCompoundStatement* statement)
{
  assert(statement != 0);
  assert(!fParsingClasses.empty());
  CIntermediateClass* currentClass = GetCurrentClass();
  if (statement->fChildren->size() > 0) {
    if (currentClass->fStaticInitializer != 0) {
      currentClass->fStaticInitializer->fChildren->push_back(statement);
      currentClass->fStaticLocalVariables +=
	currentClass->fCurrentLocalVariable;
    } else {
      currentClass->fStaticInitializer = statement;
      currentClass->fStaticLocalVariables =
	currentClass->fCurrentLocalVariable;
    }
  } else {
    delete statement;
  }
  currentClass->fCurrentLocalVariable = 0;
}

//
//  Method name : PrepareIntermediateClass
//  Description : After parsing is complete, this call is used by the compiler
//    to prepare the intermediate class so that it can be queried for class
//    signature information like any imported class.  The compiler must be
//    able to do this before the intermediate class is completely resolved
//    to allow two classes in the same file to safely refernce each other.
//
void
CCompiler::PrepareIntermediateClass(CIntermediateClass& intermediate,
				    const unicode_string& fileName)
{
  assert(intermediate.fRealClass != 0);
  if (intermediate.fIsAnonymous) {
    const CJavaClassFile* extends =
      LookupClass(*intermediate.fExtends, &intermediate);
    if (extends != 0 && extends->GetAccessFlags().fInterface) {
      intermediate.fInterfaces.push_back(*intermediate.fExtends);
      intermediate.fExtends = 0;
    }
  }
  if (intermediate.fExtends != 0) {
    intermediate.fRealClass->fSuperclassName = *intermediate.fExtends;
  } else if (!(intermediate.fName == kObjectName)) {
    intermediate.fRealClass->fSuperclassName = kObjectName;
  }
  intermediate.fRealClass->fInterfaces = intermediate.fInterfaces;
}

//
//  Method name : PrepareClassDeclarations
//  Description : This method is used to set up the fields and methods
//    declared on one class being compiled.  If the class was declared to
//    have a field with type like 'String,' for example, this is fixed to
//    be 'java.lang.String' before insertion.
//    If no problems occur during this preparation, this method returns true.
//
bool
CCompiler::PrepareClassDeclarations(CIntermediateClass& intermediate)
{
  bool result = true;
  CJavaClassFile* realClass = intermediate.fRealClass;
  assert(realClass != 0);
  for (StatementList::iterator i = intermediate.fStaticDeclarations.begin();
       !(i == intermediate.fStaticDeclarations.end()); ++i) {
    CDeclarationStatement* declaration =
      DYNAMIC_CAST(CDeclarationStatement, *i);
    assert(declaration != 0 && declaration->fDeclarations != 0 &&
	   declaration->fModifiers != 0);
    deque<CVariableDeclaration*>::iterator field =
      declaration->fDeclarations->begin();
    for (; !(field == declaration->fDeclarations->end()); ++field) {
      CJavaFieldSignature oldSignature = (*field)->GetSignature();
      unicode_string fieldName = oldSignature.GetFieldName();
      CJavaFieldInfo existingFieldInfo;
      const CJavaClassFile* fieldAlreadyOn =
	FindField(fieldName, *realClass, existingFieldInfo);
      if (fieldAlreadyOn != 0 && fieldAlreadyOn != realClass) {
	CJavaAccessFlags modifiers = existingFieldInfo.GetModifiers();
	if (fieldAlreadyOn == realClass) {
	  string message = "Field name '" + ::UnicodeToString(fieldName) +
	    "' already in use.";
	  PrintCompileError(intermediate.fSourceFileName, message,
			    declaration->GetLineNumber());
	  result = false;
	}
      }
      if (result) {
	CJavaFieldSignature newSignature(FixType(oldSignature.GetType()),
					 fieldName);
	CJavaFieldInfo* fieldInfo =
	  new CJavaFieldInfo(*declaration->fModifiers, newSignature, 0,
			     declaration->fDeprecated);
	realClass->fFields.insert(
			  FieldTable::value_type(newSignature, fieldInfo));
      }
    }
  }
  for (StatementList::iterator i = intermediate.fNonStaticDeclarations.begin();
       !(i == intermediate.fNonStaticDeclarations.end()); ++i) {
    CDeclarationStatement* declaration =
      DYNAMIC_CAST(CDeclarationStatement, *i);
    assert(declaration != 0 && declaration->fDeclarations != 0 &&
	   declaration->fModifiers != 0);
    deque<CVariableDeclaration*>::iterator field =
      declaration->fDeclarations->begin();
    for (; !(field == declaration->fDeclarations->end()); ++field) {
      CJavaFieldSignature oldSignature = (*field)->GetSignature();
      unicode_string fieldName = oldSignature.GetFieldName();
      CJavaFieldInfo existingFieldInfo;
      const CJavaClassFile* fieldAlreadyOn =
	FindField(fieldName, *realClass, existingFieldInfo);
      if (fieldAlreadyOn == realClass) {
	string message = "Field name '" + ::UnicodeToString(fieldName) +
	  "' already in use.";
	PrintCompileError(intermediate.fSourceFileName, message,
			  declaration->GetLineNumber());
	result = false;
      }
      if (result) {
	CJavaFieldSignature newSignature(FixType(oldSignature.GetType()),
					 fieldName);
	CJavaFieldInfo* fieldInfo =
	  new CJavaFieldInfo(*declaration->fModifiers, newSignature, 0,
			     declaration->fDeprecated);
	realClass->fFields.insert(
			  FieldTable::value_type(newSignature, fieldInfo));
      }
    }
  }
  deque<CIntermediateFunction*>::iterator functions =
    intermediate.fFunctions.begin();
  for (; functions != intermediate.fFunctions.end(); ++functions) {
    unicode_string methodName = (*functions)->fSignature.GetName();
    if (methodName == kConstructorName) {
      AddSyntheticConstructorParams(intermediate, *(*functions));
    }
    CJavaTypeSignature returns = FixType((*functions)->fSignature.GetType());
    deque<CJavaTypeSignature> arguments;
    deque<CJavaTypeSignature>::const_iterator arg =
      (*functions)->fSignature.ParametersBegin();
    deque<CJavaTypeSignature>::const_iterator end =
      (*functions)->fSignature.ParametersEnd();
    for (; arg != end; ++arg) {
      arguments.push_back(FixType(*arg));
    }
    CJavaMethodSignature signature(returns, methodName, arguments);
    CJavaMethodInfo* info = new CJavaMethodInfo;
    info->fSignature = signature;
    info->fDeprecated = (*functions)->fDeprecated;
    if ((*functions)->fThrows.size() > 0) {
      info->fExceptions = new CJavaExceptionsTable();
      for (deque<unicode_string>::const_iterator throws =
	     (*functions)->fThrows.begin();
	   !(throws == (*functions)->fThrows.end()); ++throws) {
	unicode_string throwClassName;
	FixType(*throws).GetBaseClassName(throwClassName);
	info->fExceptions->AddException(throwClassName);
      }
    }
    info->fAccessFlags = (*functions)->fAccessFlags;
    realClass->InsertMethod(info);
    (*functions)->fMethodInfoAlias = info;
  }
  if (realClass->fSuperclassName.length() > 0) {
    const CJavaClassFile* parent = LookupClass(realClass->fSuperclassName);
    if (parent == 0) {
      cerr << "Invalid class name used as parent of "
	   << ::UnicodeToString(intermediate.fName) << ": "
	   << ::UnicodeToString(realClass->fSuperclassName) << endl;
      result = false;
    } else if (! ValidParent(*realClass, *parent)) {
      cerr << ::UnicodeToString(intermediate.fName) << " cannot extend "
	   << ::UnicodeToString(realClass->fSuperclassName) << endl;
      result = false;
    } else {
      if (!(parent->GetClassName() == realClass->fSuperclassName)) {
	realClass->fSuperclassName = parent->GetClassName();
      }
      if (realClass->GetAccessFlags().fInterface == 0) {
	if (realClass->GetAccessFlags().fAbstract != 0) {
	  MethodList methodList;
	  if (UndeclaredInterfaceMethods(*realClass, methodList) != 0) {
	    for (MethodList::const_iterator i = methodList.begin();
		 !(i == methodList.end()); ++i) {
	      CJavaMethodInfo* info = new CJavaMethodInfo;
	      info->fSignature = *i;
	      CJavaAccessFlags flags;
	      flags.fPublic = 1;
	      flags.fAbstract = 1;
	      info->fAccessFlags = flags;
	      realClass->InsertMethod(info);
	    }
	  }
	} else {
	  MethodList methodList;
	  if (UnimplementedMethods(*realClass, methodList) != 0) {
	    cerr << ::UnicodeToString(intermediate.fName)
		 << " must be declared abstract or implement:" << endl;
	    for (MethodList::const_iterator i = methodList.begin();
		 !(i == methodList.end()); ++i) {
	      cerr << "   ";
	      (*i).Disassemble(cerr);
	      cerr << endl;
	    }
	    result = false;
	  }
	}
      }
    }
  }
  for (deque<unicode_string>::iterator i = realClass->fInterfaces.begin();
       i != realClass->fInterfaces.end(); ++i) {
    const CJavaClassFile* interface = LookupClass(*i);
    if (interface == 0) {
      cerr << "Invalid class name used as interface for "
	<< ::UnicodeToString(intermediate.fName) << ": "
	  << ::UnicodeToString(*i) << endl;
      result = false;
    } else if (! ValidInterface(*realClass, *interface)) {
      cerr << ::UnicodeToString(*i) << " cannot serve as an interface for "
	<< ::UnicodeToString(intermediate.fName) << endl;
      result = false;
    } else {
      if (!(*i == interface->GetClassName())) {
	*i = interface->GetClassName();
      }
    }
  }
  return result;
}

//
//  Method name : AddSyntheticConstructorParams
//  Description : This is needed for inner classes that are passed a set of
//    local variables during construction.  This method inserts the proper
//    pairs into the constructor.
//
void
CCompiler::AddSyntheticConstructorParams(CIntermediateClass& inClass,
					 CIntermediateFunction& intermediate)
{
  list<COuterLocalExpression*>::const_iterator synthetics =
    inClass.fSyntheticLocals.begin();
  list<COuterLocalExpression*>::const_iterator end =
    inClass.fSyntheticLocals.end();
  if (!(synthetics == end)) {
    deque<CJavaTypeSignature> parameters;
    intermediate.fSignature.CopyParameters(parameters);
    for (; !(synthetics == end); ++synthetics) {
      unsigned long index = intermediate.fRealParametersSize + 
	intermediate.fSyntheticParametersSize;
      CVariableDeclaration declaration = (*synthetics)->GetLocalDeclaration();
      CJavaTypeSignature type = FixType(declaration.GetType());
      parameters.push_back(type);

      unsigned short size = type.GetWidth();
      intermediate.fSyntheticParametersSize += size;
      intermediate.fMaxLocalVariables += size;
    }
    intermediate.fSignature = 
      CJavaMethodSignature(intermediate.fSignature.GetType(),
			   intermediate.fSignature.GetName(), parameters);
  }
}

//
//  Method name : GenerateClass
//  Description : This is the cannonical 'second-pass' of the compiler.  It
//    takes one of the intermediate classes that the parser produced and
//    does the necessary checks and transformations on it to produce a valid
//    class file.  If this is successful, the class file is returned to
//    the user.  Otherwise, 0 is returned to signify an error.
//
bool
CCompiler::GenerateClass(CIntermediateClass& intermediate)
{
  unicode_string fileName = ::UTFToUnicode(intermediate.fSourceFileName);
  CJavaClassFile* realClass = intermediate.fRealClass;
  assert(realClass != 0);
  bool result = true;
  FieldTable::iterator endFields = intermediate.fRealClass->fFields.end();
  FieldTable::iterator field = intermediate.fRealClass->fFields.begin();
  for (; field != endFields; ++field) {
    CCompileError* error =
      CheckType((*field).second->GetSignature().GetType());
    if (error != 0) {
      PrintCompileError(intermediate.fSourceFileName, error->GetMessage());
      delete error;
      result = false;
    }
  }
  if (result) {
    result = GenerateFieldConstants(intermediate);
  }
  if (result) {
    deque<CIntermediateFunction*>::iterator function =
      intermediate.fFunctions.begin();
    for (; function != intermediate.fFunctions.end(); ++function) {
      CCompileError* error = 0;
      if (!((*function)->fSignature.GetName() == kConstructorName)) {
	const CJavaClassFile* parentClass =
	  LookupClass(intermediate.fRealClass->GetSuperclassName());
	if (parentClass != 0) {
	  error = CheckValidOverride(*parentClass, *(*function));
	}
	for (CJavaClassFile::InterfaceList::const_iterator interfaces =
	       intermediate.fRealClass->GetInterfaceBegin();
	     error == 0 &&
	       interfaces != intermediate.fRealClass->GetInterfaceEnd();
	     ++interfaces) {
	  parentClass = LookupClass(*interfaces);
	  if (parentClass != 0) {
	    error = CheckValidOverride(*parentClass, *(*function));
	  }
	}
      }
      if (error == 0) {
	error = GenerateMethod(intermediate, *(*function));
      }
      if (error != 0) {
	string message("In ");
	if ((*function)->fSignature.GetName() == kConstructorName) {
	  message += "a constructor for";
	} else if ((*function)->fSignature.GetName() == kStaticName) {
	  message += "static initialization for";
	} else {
	  message += ::UnicodeToString((*function)->fSignature.Disassemble());
	}
	message += " in class " + ::UnicodeToString(intermediate.fName) +
	  ": " + ::UnicodeToString(error->GetMessage());
	if (error->GetLine() == CCompileError::kNoLineNumber) {
	  PrintCompileError(intermediate.fSourceFileName, message);
	} else {
	  PrintCompileError(intermediate.fSourceFileName, message,
			    error->GetLine());
	}
	result = false;
	delete error;
      }
    }
  }
  return result;
}

//
//  Method name : GenerateFieldConstants
//  Description : This method is used to figure out if any of the fields on
//    the provided intermediate class have a constant value that should be
//    pre-calculated in their class file.  This method iterates through all
//    of the field declarations and tries to find final fields for constant
//    insertion.
//    This method returns true if there were no errors encountered during
//    constant generation.
//
bool
CCompiler::GenerateFieldConstants(CIntermediateClass& intermediateClass)
{
  bool success = true;
  assert(intermediateClass.fRealClass != 0);
  fInClassInitializers = true;
  FieldTable::iterator endFields = intermediateClass.fRealClass->fFields.end();
  FieldTable::iterator field = intermediateClass.fRealClass->fFields.begin();
  for (; field != endFields; ++field) {
    const CJavaFieldInfo* fieldInfo = (*field).second;
    assert(fieldInfo != 0);
    if (fieldInfo->GetModifiers().fFinal != 0) {
      CExpression* result = 0;
      CCompileError* error =
	FindIntermediateConstantField(result, intermediateClass, *fieldInfo);
      if (error != 0) {
	PrintCompileError(intermediateClass.fSourceFileName,
			  "Invalid field initialization: " +
			  ::UnicodeToString(error->GetMessage()),
			  error->GetLine());
	delete error;
	success = false;
      } else if (result != 0) {
	assert(result->IsLiteral());
	JavaConstantIndex constant =
	  CreateConstantFromLiteral(result, *intermediateClass.fRealClass);
	CJavaFieldInfo* newFieldInfo = new CJavaFieldInfo(*fieldInfo);
	newFieldInfo->SetConstantIndex(constant);
	delete fieldInfo;
	(*field).second = newFieldInfo;
	delete result;
      }
    }
  }
  fInClassInitializers = false;
  return success;
}

//
//  Method name : GenerateMethod
//  Description : This method is used to generate the final method
//    representation for a java method.  This generates all of the
//    needed information for a CJavaMethodInfo and inserts it into the provided
//    class.  If this operation is unsuccessful for any reason, an error
//    is returned, otherwise, 0 is returned.
//
CCompileError*
CCompiler::GenerateMethod(CIntermediateClass& intermediateClass,
			  CIntermediateFunction& intermediateMethod)
{
  CCompileError* error = 0;
  MethodTable::iterator methodPosition =
    intermediateClass.fRealClass->fMethods.begin();
  for (; !(methodPosition == intermediateClass.fRealClass->fMethods.end());
       ++methodPosition) {
    if (SameType(intermediateMethod.fSignature, (*methodPosition).first)) {
      break;
    }
  }
  assert(methodPosition != intermediateClass.fRealClass->fMethods.end());
  if ((*methodPosition).second != intermediateMethod.fMethodInfoAlias) {
    string errorString = "Duplicate method signature: ";
    unicode_string disassembled =
      intermediateMethod.fSignature.Disassemble();
    errorString += ::UnicodeToString(disassembled);
    error = new CCompileError(errorString,
			      intermediateMethod.fStartLineNumber);
  }
  CJavaMethodInfo* method = intermediateMethod.fMethodInfoAlias;
  const CJavaClassFile* throwable = 0;
  if (error == 0) {
    for (deque<unicode_string>::iterator i= intermediateMethod.fThrows.begin();
	 i != intermediateMethod.fThrows.end(); ++i) {
      const CJavaClassFile* thrown = LookupClass(*i, &intermediateClass);
      if (thrown == 0) {
	method = 0;
	string errorString = "Non-existant type ";
	errorString += ::UnicodeToString(*i);
	errorString += " thrown by ";
	unicode_string disassembled =
	  intermediateMethod.fSignature.Disassemble();
	errorString += ::UnicodeToString(disassembled);
	error = new CCompileError(errorString,
				  intermediateMethod.fStartLineNumber);
	break;
      } else {
	if (! (thrown == throwable || DescendsFrom(*thrown, *throwable))) {
	  method = 0;
	  string errorString = "Non-Throwable type ";
	  errorString += ::UnicodeToString(*i);
	  errorString += " thrown by ";
	  errorString +=
	    ::UnicodeToString(intermediateMethod.fSignature.Disassemble());
	  error = new CCompileError(errorString,
				    intermediateMethod.fStartLineNumber);
	} else if (!(thrown->GetClassName() == *i)) {
	  *i = thrown->GetClassName();
	}
      }
    }
  }
  if (error == 0) {
    error = CheckType(method->fSignature.GetType());
    deque<CJavaTypeSignature>::const_iterator arg =
      method->fSignature.ParametersBegin();
    deque<CJavaTypeSignature>::const_iterator end =
      method->fSignature.ParametersEnd();
    for (; error == 0 && arg != end; ++arg) {
      error = CheckType(*arg);
    }
  }
  if (error == 0) {
    error = GenerateCode(intermediateClass, intermediateMethod, *method);
    // remove empty static initializer...
    bool staticMethod = intermediateMethod.fSignature.GetName() == kStaticName;
    if (staticMethod &&	method->fCodeAttribute == 0) {
      intermediateClass.fRealClass->fMethods.erase(methodPosition);
    } else if (error == 0 &&
	       intermediateClass.fAccessFlags.fInterface != 0) {
      if (intermediateMethod.fAccessFlags.fNative != 0 ||
	  intermediateMethod.fAccessFlags.fSynchronized != 0 ||
	  intermediateMethod.fAccessFlags.fFinal != 0 ||
	  intermediateMethod.fAccessFlags.fPrivate != 0 ||
	  intermediateMethod.fAccessFlags.fProtected != 0 ||
	  (intermediateMethod.fAccessFlags.fStatic != 0 && !staticMethod)) {
	error = new CCompileError("Interface methods can't be native, static, "
			  "synchronized, final, private, or protected",
				    intermediateMethod.fStartLineNumber);
      } else if (method->GetCode() != 0 &&
		 !(method->fSignature.GetName() == kStaticName)) {
	error = new CCompileError("Invalid code in interface",
				    intermediateMethod.fStartLineNumber);
      }
    }
  }
  return error;
}

//
//  Method name : GenerateCode
//  Description : This method is used to generate the CJavaCodeAttribute
//    information for a method.  This is where the actual compilation work
//    starts as a method is transformed from a tree of statements and
//    expressions into a sequence of bytecodes.  The current intermediate
//    class and method are half-way points in the compilation process and
//    the 'method' parameter is the value that is actually modified to take
//    on the new code attribute.
//    If this operation is successful, 0 is returned, otherwise an error
//    is created and returned to the caller.
//
CCompileError*
CCompiler::GenerateCode(CIntermediateClass& intermediate,
			CIntermediateFunction& intermediateMethod,
			CJavaMethodInfo& method)
{
  CCompileError* error = 0;
  bool isInterface = intermediate.fAccessFlags.fInterface != 0;
  CCompoundStatement* body = intermediateMethod.fBlock;
  bool staticInitializer = method.fSignature.GetName() == kStaticName;
  CJavaAccessFlags methodFlags = method.fAccessFlags;
  if (methodFlags.fAbstract != 0) {
    if (body != 0) {
      error = new CCompileError("Invalid method body on abstract method",
				intermediateMethod.fStartLineNumber);
    }
  } else if (methodFlags.fNative != 0) {
    if (body != 0) {
      error = new CCompileError("Invalid method body on native method",
				intermediateMethod.fStartLineNumber);
    }
  } else {
    if (body == 0) {
      error = new CCompileError("Invalid empty method body",
				intermediateMethod.fStartLineNumber);
    } else {
      CCodeSequence code;
      CJavaCodeAttribute* codeAttribute = new CJavaCodeAttribute;
      method.fCodeAttribute = codeAttribute;
      codeAttribute->fMaxLocals = intermediateMethod.fMaxLocalVariables;
      CCompileContext context(this, &intermediate, intermediate.fRealClass,
			      &intermediateMethod, &method,
			      intermediateMethod.fMaxLocalVariables);
      for (deque<unicode_string>::const_iterator i =
	     intermediateMethod.fThrows.begin();
	   !(i == intermediateMethod.fThrows.end()); ++i) {
	context.PushThrowable(CJavaTypeSignature(*i));
      }
      deque<CJavaTypeSignature>::const_iterator parameters = 
	method.GetSignature().ParametersBegin();
      deque<CJavaTypeSignature>::const_iterator parametersEnd = 
	method.GetSignature().ParametersEnd();
      unsigned long variableIndex = 0;
      if (method.GetModifiers().fStatic == 0) {
	context.InitializeVariable(variableIndex++);
      }
      for (; parameters != parametersEnd; ++parameters) {
	context.InitializeVariable(variableIndex);
	variableIndex += (*parameters).GetWidth();
      }
      CExplicitConstructorCall* constructor = 0;
      unsigned short maxStack = 0;
      if (method.fSignature.GetName() == kConstructorName) {
	bool callsThisConstructor = false;
	if (body->fChildren->empty() ||
	    DYNAMIC_CAST(CExplicitConstructorCall,
			 body->fChildren->front()) == 0) {
	  if (!(intermediate.GetName() == kObjectName)) {
	    constructor = new CExplicitConstructorCall(
				      CExplicitConstructorCall::kSuper, 0);
	  }
	} else {
	  constructor =
	    DYNAMIC_CAST(CExplicitConstructorCall, body->fChildren->front());
	  callsThisConstructor = 
	    (constructor->GetType() == CExplicitConstructorCall::kThis);
	  body->fChildren->pop_front();
	}
	if (constructor != 0) {
	  error =
	    constructor->GenerateCode(code, context, codeAttribute->fMaxStack);
	  maxStack = codeAttribute->fMaxStack;
	} else {
	  maxStack = 0;
	}
	if (!callsThisConstructor) {
	  fInClassInitializers = true;
	  if (intermediate.fIsInner) {
	    GenerateSyntheticCode(context, code);
	    maxStack = ::max((unsigned short)2, maxStack);
	  }
	  if (intermediate.fNonStaticDeclarations.size() > 0) {
	    for (StatementList::iterator i =
		   intermediate.fNonStaticDeclarations.begin();
		 error == 0 &&
		   !(i == intermediate.fNonStaticDeclarations.end()); ++i) {
	      unsigned short stack;
	      error = (*i)->GenerateCode(code, context, stack);
	      maxStack = ::max(stack, maxStack);
	    }
	  }
	  fInClassInitializers = false;
	}
      }
      if (error == 0) {
	if (staticInitializer) {
	  fInClassInitializers = true;
	  for (StatementList::iterator i =
		 intermediate.fStaticDeclarations.begin();
	       error == 0 && !(i == intermediate.fStaticDeclarations.end());
	       ++i) {
	    unsigned short stack;
	    error = (*i)->GenerateCode(code, context, stack);
	    maxStack = ::max(stack, maxStack);
	  }
	}
	if (error == 0) {
	  error =
	    body->GenerateCode(code, context, codeAttribute->fMaxStack);
	}
	fInClassInitializers = false;
	codeAttribute->fMaxStack = ::max(codeAttribute->fMaxStack, maxStack);
	if (constructor != 0) {
	  body->fChildren->push_front(constructor);
	}
	if (error == 0) {
	  bool needsReturn = body->fChildren->empty();
	  if (!needsReturn && context.IsReachable()) {
	    CStatement* lastStatement = body->fChildren->back();
	    CReturnStatement* returnStatement =
	      DYNAMIC_CAST(CReturnStatement, lastStatement);
	    needsReturn = returnStatement == 0;
	  }
	  if (needsReturn) {
	    if (method.fSignature.GetType() == CJavaTypeSignature::kVoid) {
	      code.Append(CJavaCodeAttribute::op_return);
	    } else {
	      string errorString("Return statement needed at the end of ");
	      errorString +=
		::UnicodeToString(method.GetSignature().Disassemble());
	      error = new CCompileError(errorString,
				intermediateMethod.fEndLineNumber);
	    }
	  }
	}
      }
      if (error == 0) {
	code.PeepholeOptimize();
	code.Finalize(*codeAttribute);
	if (staticInitializer) {
	  if (code.size() == 1) {
	    delete codeAttribute;
	    method.fCodeAttribute = 0;
// 	  } else if (intermediate.fAccessFlags.fInterface != 0) {
// 	    error =
// 	      new CCompileError("Invalid static initializers in interface");
	  }
	}
      }
      if (error != 0) {
	delete codeAttribute;
	method.fCodeAttribute = 0;
      }
    }
  }
  return error;
}

//
//  Method name : ValidClass
//  Description : This function is used to determine whether a given class
//    name represents a valid object.  This checks imported classes, classes
//    in this same file, and globally-named files.
//
bool
CCompiler::ValidClass(const unicode_string& name) const
{
  return LookupClass(name) != 0;
}

//
//  Method name : LookupIntermediateClass
//  Description : This checks the list of classes in the same file to see if
//     any of them have the given class name.  If any do, a pointer to that
//     class is returned, otherwise 0 is returned.
//
const CIntermediateClass*
CCompiler::LookupIntermediateClass(const unicode_string& name) const
{
  bool baseName = name.find((unicode_char)'/') == unicode_string::npos;
  IntermediateList::const_iterator i = fIntermediateClasses.begin();
  for (; i != fIntermediateClasses.end(); ++i) {
    if ((*i)->fName == name) {
      return *i;
    } else if (baseName) {
      unicode_string::size_type lastSlash = (*i)->fName.find_last_of('/');
      unicode_string afterSlash((*i)->fName, lastSlash + 1);
      if (lastSlash != unicode_string::npos && afterSlash == name) {
	return *i;
      }
    }
  }
  return 0;
}

//
//  Method name : RemoveIntermediateClass
//  Description : If the provided intermediate class is in the list of
//    currently-parsing intermediate classes, it is removed.
//
void
CCompiler::RemoveIntermediateClass(const CIntermediateClass* intermediate)
{
  IntermediateList::iterator i = fIntermediateClasses.begin();
  for (; i != fIntermediateClasses.end(); ++i) {
    if (*i == intermediate) {
      fIntermediateClasses.erase(i);
      break;
    }
  }
}

//
//  Method name : LookupClass
//  Description : This can be used to see if a given class name is visible
//    inside of this file.  This includes globally-qualified class names that
//    include their package and simple names that are visible thanks to some
//    previous import statement or visible because they are in this file.
//    If one is found, that entry is returned, otherwise 0 is returned.
//    NOTE:  Since this can potentially return a half-built entry from this
//      same file, you should use this class for its interface only ... the
//      method internals and constants may not yet be valid.
//
const CJavaClassFile*
CCompiler::LookupClass(const unicode_string& name,
		       const CIntermediateClass* classContext,
		       const CIntermediateFunction* methodContext) const
{
  if (name.length() == 0) {
    return 0;
  }
  const CIntermediateClass* intermediate = LookupIntermediateClass(name);
  if (intermediate != 0) {
    assert(intermediate->fRealClass != 0);
    return intermediate->fRealClass;
  }
  if (name.find((unicode_char)'/') == unicode_string::npos) {
    const CJavaClassFile* innerClass =
      LookupInnerClass(name, classContext, methodContext);
    if (innerClass != 0) {
      return innerClass;
    }
  }
  ImportTable::const_iterator entry = fImportAliases.find(name);
  if (entry != fImportAliases.end()) {
    return (*entry).second;
  }
  // Casting away 'const' to allow caching of the result.
  return ((CCompiler*)this)->ImportClass(name, false);
}

//
//  Method name : LookupClass
//  Description : Tries to find the given class name, visible from the
//    provided compile context.  This is a convenience method for the
//    other LookupClss(), above.
//
const CJavaClassFile*
CCompiler::LookupClass(const unicode_string& name,
		       const CCompileContext& context) const
{
  return LookupClass(name, context.GetIntermediateClass(),
		     context.GetIntermediateMethod());
}

//
//  Method name : LookupInnerClass
//  Description : This is used internally by the compiler to try to find
//    a class name that is only available as an inner class from the provided
//    class, possibly inner within the provided method.
//
const CJavaClassFile*
CCompiler::LookupInnerClass(const unicode_string& name,
		       const CIntermediateClass* classContext,
		       const CIntermediateFunction* methodContext) const
{
  if (methodContext != 0) {
    for (deque<CIntermediateClass*>::const_iterator i =
	   methodContext->fInnerClasses.begin();
	 !(i == methodContext->fInnerClasses.end()); ++i) {
      if ((*i)->GetShortName() == name) {
	return (*i)->fRealClass;
      }
    }
  }
  if (classContext != 0) {
    for (deque<CIntermediateClass*>::const_iterator i =
	   classContext->fInnerClasses.begin();
	 !(i == classContext->fInnerClasses.end()); ++i) {
      if ((*i)->GetShortName() == name) {
	return (*i)->fRealClass;
      }
    }
    const CJavaClassFile* parent =
      LookupClass(classContext->fRealClass->fSuperclassName);
    unicode_string match;
    while (parent != 0) {
      if (parent->FindInnerClass(name, match)) {
	return LookupClass(match);
      }
      parent = LookupClass(parent->fSuperclassName);
    }
  }
  return 0;
}

//
//  Method name : ValidParent
//  Description : Returns true if this 'parent' object can legally be
//    extended by 'child.'
//
bool
CCompiler::ValidParent(const CJavaClassFile& child,
		       const CJavaClassFile& parent) const
{
  CJavaAccessFlags childAccess = child.GetAccessFlags();
  CJavaAccessFlags parentAccess = parent.GetAccessFlags();
  bool result = (parentAccess.fPublic != 0) ||
    (parent.GetPackageName() == child.GetPackageName());
  result &= parentAccess.fFinal == 0;
  result &= parentAccess.fInterface == 0;
  result &= childAccess.fInterface == 0 ||
    parent.GetClassName() == kObjectName;
  // XXXX some day, do a circularity check here...
  return result;
}

//
//  Method name : ValidInterface
//  Description : Returns true if the 'interface' object can legally be
//    extended by 'child.'
//
bool
CCompiler::ValidInterface(const CJavaClassFile& child,
			  const CJavaClassFile& interface) const
{
  CJavaAccessFlags childAccess = child.GetAccessFlags();
  CJavaAccessFlags interfaceFlags = interface.GetAccessFlags();
  bool result = interfaceFlags.fInterface != 0;
  result &= (interfaceFlags.fPublic != 0) ||
    (interface.GetPackageName() == child.GetPackageName());
  return result;
}

//
//  Method name : DescendsFrom
//  Description : Returns true if the provided child descends from the
//     ancestor object, as an extension class or interface.
//     Executes a depth-first search up the inheritance tree, classes
//     first, then interfaces.
//
bool
CCompiler::DescendsFrom(const CJavaClassFile& child,
			const CJavaClassFile& ancestor) const
{
  // XXXX doesn't currently deal with circularity ... assumes well-formed tree.
  const CJavaClassFile* parent;
  bool result = false;
  if (child.fAccessFlags.fInterface == 0) {
    parent = LookupClass(child.fSuperclassName);
    result = &ancestor == parent ||
      (parent != 0 && DescendsFrom(*parent, ancestor));
  } else {
    result = ancestor.GetClassName() == kObjectName;
  }
  if (!result) {
    for (deque<unicode_string>::const_iterator i = child.fInterfaces.begin();
	 !result && i != child.fInterfaces.end(); ++i) {
      parent = LookupClass(*i);
      result = &ancestor == parent ||
	(parent != 0 && DescendsFrom(*parent, ancestor));
    }
  }
  return result;
}

//
//  Method name : AssignableSubtype
//  Description : This function returns true if the childType can legally
//    be assigned to a variable of type ancestorType, but is childType !=
//    ancestorType.  Only works with classes and arrays ... not primitive
//    types.
//
bool
CCompiler::AssignableSubtype(const CJavaTypeSignature& childType,
			     const CJavaTypeSignature& ancestorType) const
{
  bool result = false;
  if (childType.GetArrayBounds() > 0) {
    if (ancestorType.GetArrayBounds() == childType.GetArrayBounds()) {
      CJavaTypeSignature baseChildType(childType);
      baseChildType.SetArrayBounds(0);
      CJavaTypeSignature baseAncestorType(ancestorType);
      baseAncestorType.SetArrayBounds(0);
      result = AssignableSubtype(baseChildType, baseAncestorType);
    } else {
      static const CJavaTypeSignature kObjectType(kObjectName);
      result = SameType(kObjectType, ancestorType);
    }
  } else {
    result = childType == CJavaTypeSignature::kNullType;
    if (!result) {
      unicode_string childName, ancestorName;
      result = childType.GetBaseClassName(childName) &&
	ancestorType.GetBaseClassName(ancestorName);
      if (result) {
	const CJavaClassFile* childClass = LookupClass(childName);
	const CJavaClassFile* ancestorClass = LookupClass(ancestorName);
	result = (childClass != 0) && (ancestorClass != 0) &&
	  DescendsFrom(*childClass, *ancestorClass);
      }
    }
  }
  return result;
}

//
//  Method name : CastableType
//  Description : This function returns true if the currentType can legally
//    be casted to an instance of type toType.
//    Only works with classes and arrays ... not primitive types.
//
bool
CCompiler::CastableType(const CJavaTypeSignature& currentType,
			const CJavaTypeSignature& toType) const
{
  unicode_string currentClassName;
  bool result = currentType.GetBaseClassName(currentClassName);
  if (result) {
    unicode_string toClassName;
    if (toType.GetBaseClassName(toClassName)) {
      const CJavaClassFile* toClassFile = LookupClass(toClassName);
      const CJavaClassFile* currentClassFile = LookupClass(currentClassName);
      result = (toClassFile != 0) && (currentClassFile != 0) &&
	(currentType == CJavaTypeSignature::kNullType ||
	 toClassFile->GetAccessFlags().fInterface != 0 ||
	 currentClassFile->GetAccessFlags().fInterface != 0 ||
	 AssignableSubtype(currentType, toType) ||
	 AssignableSubtype(toType, currentType) ||
	 SameType(toType, currentType));
    } else {
      result = toType.GetArrayBounds() > 0 && currentClassName == kObjectName;
    }
  }
  return result;
}

//
//  Method name : FindField
//  Description : This method is used to try to find a field with a given name
//    by starting with 'fromClass' and working up the inheritance tree until
//    it runs out of parents (and returns 0) or find a class that implements
//    the desired field.  If this is found, the third parameter is set to
//    the type signature of the desired field.
//
const CJavaClassFile*
CCompiler::FindField(const unicode_string& field,
		     const CJavaClassFile& fromClass,
		     CJavaFieldInfo& setFieldInfo) const
{
  const CJavaClassFile* found = 0;
  const CJavaFieldInfo* fieldInfo = fromClass.LookupField(field);
  if (fieldInfo != 0) {
    setFieldInfo = *fieldInfo;
    found = &fromClass;
  } else {
    const CJavaClassFile* parent = 0;
    if (fromClass.fAccessFlags.fInterface == 0) {
      parent = LookupClass(fromClass.fSuperclassName);
      if (parent != 0) {
	found = FindField(field, *parent, setFieldInfo);
      }
    }
    deque<unicode_string>::const_iterator i = fromClass.fInterfaces.begin();
    for (; found == 0 && i != fromClass.fInterfaces.end(); ++i) {
      parent = LookupClass(*i);
      if (parent != 0) {
	found = FindField(field, *parent, setFieldInfo);
      }
    }
  }
  return found;
}

//
//  Method name : MatchMethod
//  Description : This is used to search for a matching method with the given
//    name that can be called with arguments of the provided type.  The
//    search starts at 'fromClass' and works its way up the inheritance tree
//    until a match is found (in which case a pair is created and returned to
//    the user to indicate the matched class and method info) or no match can
//    be found. (in which case 0 is returned.)
//
pair<const CJavaClassFile*, const CJavaMethodInfo*>*
CCompiler::MatchMethod(const unicode_string& methodName,
		       const CJavaClassFile& fromClass,
		       const deque<CJavaTypeSignature>& arguments,
		       string*& errorString)
{
  pair<const CJavaClassFile*, const CJavaMethodInfo*>* match = 0;
  const CJavaMethodInfo* methodInfo =
    MatchClassMethod(fromClass, methodName, arguments, errorString);
  if (methodInfo != 0) {
    match = new pair<const CJavaClassFile*, const CJavaMethodInfo*>(
						    &fromClass, methodInfo);
  } else if (!(methodName == kConstructorName)) {
    const CJavaClassFile* parent = LookupClass(fromClass.fSuperclassName);
    if (parent != 0) {
      match = MatchMethod(methodName, *parent, arguments, errorString);
    }
    if (match == 0) {
      deque<unicode_string>::const_iterator i = fromClass.fInterfaces.begin();
      for (; match == 0 && i != fromClass.fInterfaces.end(); ++i) {
	parent = LookupClass(*i);
	if (parent != 0) {
	  match = MatchMethod(methodName, *parent, arguments, errorString);
	}
      }
    }
  }
  return match;
}

//
//  Method name : MatchConstructor
//  Description : This is used to search for a constructor that can be
//    called with arguments of the provided type.  The search starts
//    at the value 'fromClass' and works its way up the inheritance tree
//    until a match is found (in which case a pair is created and returned to
//    the user to indicate the matched class and method info) or no match can
//    be found. (in which case 0 is returned.)
//
pair<const CJavaClassFile*, const CJavaMethodInfo*>*
CCompiler::MatchConstructor(const CJavaClassFile& fromClass,
			    const deque<CJavaTypeSignature>& arguments,
			    ExpressionList::const_iterator& visibleBegin,
			    ExpressionList::const_iterator& visibleEnd,
			    string*& errorString)
{
  pair<const CJavaClassFile*, const CJavaMethodInfo*>* match =
    MatchMethod(kConstructorName, fromClass, arguments, errorString);
  if (match == 0 && errorString == 0) {
    CIntermediateClass* intermediate =
      (CIntermediateClass*)LookupIntermediateClass(fromClass.GetClassName());
    if (intermediate != 0 && intermediate->fIsAnonymous) {
      const CJavaClassFile* parent = LookupClass(fromClass.fSuperclassName);
      if (parent != 0) {
	deque<CJavaTypeSignature> parentArguments;
	for (ExpressionList::const_iterator i = visibleBegin;
	     !(i == visibleEnd); ++i) {
	  parentArguments.push_back((*i)->GetType());
	}
	pair<const CJavaClassFile*, const CJavaMethodInfo*>* parentMatch =
	  MatchMethod(kConstructorName, *parent, parentArguments, errorString);
	if (parentMatch != 0) {
	  const CJavaMethodInfo* parentInfo = parentMatch->second;
	  delete parentMatch;
	  for (deque<CIntermediateFunction*>::iterator methods =
		 intermediate->fFunctions.begin();
	       !(methods == intermediate->fFunctions.end()); ++methods) {
	    if ((*methods)->fSignature.GetName() == kConstructorName) {
	      FixAnonymousSyntheticConstructor(*intermediate, *(*methods),
					       *parentInfo);
	      return new pair<const CJavaClassFile*, const CJavaMethodInfo*>(
				    &fromClass, (*methods)->fMethodInfoAlias);
	    }
	  }
	}
      }
    }
  }
  return match;
}

//
//  Method name : FixAnonymousSyntheticConstructor
//  Description : This method takes the provided synthetic constructor and
//    adds code and parameters so that it will correctly call the superclass
//    constructor.  This is only needed for anonymous classes, which need
//    to synthesize this information based on the types of the 'new'
//    arguments.
//
void
CCompiler::FixAnonymousSyntheticConstructor(CIntermediateClass& intermediate,
					    CIntermediateFunction& constructor,
					    const CJavaMethodInfo& parentInfo)
{
  CJavaMethodSignature parentSignature = parentInfo.GetSignature();
  if (parentSignature.ParameterCount() > 0) {
    CJavaMethodSignature oldSignature = constructor.fSignature;
    ExpressionList* arguments = new ExpressionList();
    deque<CJavaTypeSignature>::const_iterator oldParameters =
      oldSignature.ParametersBegin();
    deque<CJavaTypeSignature>::const_iterator oldEnd =
      oldSignature.ParametersEnd();
    deque<CJavaTypeSignature> newParameters;
    newParameters.push_back(*oldParameters);
    ++oldParameters;

    deque<CJavaTypeSignature>::const_iterator parentParameters =
      parentSignature.ParametersBegin();
    deque<CJavaTypeSignature>::const_iterator parentEnd =
      parentSignature.ParametersEnd();
    for (; !(parentParameters == parentEnd); ++parentParameters) {
      newParameters.push_back(*parentParameters);
      CJavaFieldSignature signature(*parentParameters, unicode_string());
      CLocalVariableExpression* superArgument =
	new CLocalVariableExpression(signature,
				     constructor.fRealParametersSize, false);
      arguments->push_back(superArgument);
      unsigned short width = (*parentParameters).GetWidth();
      constructor.fRealParametersSize += width;
      constructor.fMaxLocalVariables += width;
    }

    for (; !(oldParameters == oldEnd); ++oldParameters) {
      newParameters.push_back(*oldParameters);
    }

    CJavaMethodSignature newSignature(oldSignature.GetType(),
				      oldSignature.GetName(), newParameters);
    constructor.fSignature = newSignature;
    if (constructor.fMethodInfoAlias != 0) {
      constructor.fMethodInfoAlias->fSignature = newSignature;
    }

    intermediate.fRealClass->fMethods.erase(oldSignature);
    intermediate.fRealClass->fMethods.insert(
		     MethodTable::value_type(newSignature,
					     constructor.fMethodInfoAlias));

    CExplicitConstructorCall* explicitConstructor =
      new CExplicitConstructorCall(CExplicitConstructorCall::kSuper,
				   arguments);
    constructor.fBlock->fChildren->push_front(explicitConstructor);
  }
}

//
//  Method name : MatchClassMethod
//  Description : If the provided class contains a method with the given name
//    and number of arguments, and each provided argument can be assigned to
//    its corresponding argument in the signature, then the matching method
//    info structure is returned.  Otherwise, 0 is returned.
//    Currently implemented as a linear search.
//    Sorry for the naming confusion with MatchMethod.  The difference is that
//    MatchMethod is called to try to search a class and all of its parents
//    whereas MatchClassMethod is used by MatchMethod to just search one
//    class.
//
const CJavaMethodInfo*
CCompiler::MatchClassMethod(const CJavaClassFile& onClass,
			    const unicode_string& name,
			    const deque<CJavaTypeSignature>& arguments,
			    string*& errorString) const
{
  const CJavaMethodInfo* match = 0;
  for (MethodTable::const_iterator i = onClass.fMethods.begin();
       i != onClass.fMethods.end(); ++i) {
    CJavaMethodSignature signature = (*i).first;
    if (signature.GetName() == name &&
	arguments.size() == signature.ParameterCount()) {
      deque<CJavaTypeSignature>::const_iterator parameter =
	signature.ParametersBegin();
      deque<CJavaTypeSignature>::const_iterator parametersEnd =
	signature.ParametersEnd();
      deque<CJavaTypeSignature>::const_iterator argument = arguments.begin();
      bool valid = true;
      for (; valid && parameter != parametersEnd; ++parameter, ++argument) {
	if (SameType(*parameter, *argument)) {
	  valid = true;
	} else {
	  valid = ImplicitCastTo(*argument, *parameter);
	}
      }
      if (valid) {
	if (match == 0 || MoreSpecific((*i).first, (*match).GetSignature())) {
	  match = (*i).second;
	} else if (!MoreSpecific((*match).GetSignature(), (*i).first)) {
	  errorString = new string("Ambiguous method call");
	  match = 0;
	}
      }
    }
  }
  return match;
}

//
//  Method name : ExactMatchMethod
//  Description : This is used to search for a matching method with the given
//    type signature. The search starts at 'fromClass' and works its way up
//    the inheritance tree until a match is found (in which case a pair
//    is created and returned to the caller to indicate the matched class and
///   method info) or no match can be found. (in which case 0 is returned.)
//
pair<const CJavaClassFile*, const CJavaMethodInfo*>*
CCompiler::ExactMatchMethod(const CJavaMethodSignature& method,
			    const CJavaClassFile& fromClass)
{
  pair<const CJavaClassFile*, const CJavaMethodInfo*>* match = 0;
  const CJavaMethodInfo* methodInfo = ExactMatchClassMethod(method, fromClass);
  if (methodInfo != 0) {
    match = new pair<const CJavaClassFile*, const CJavaMethodInfo*>(
						    &fromClass, methodInfo);
  } else {
    const CJavaClassFile* parent = LookupClass(fromClass.fSuperclassName);
    if (parent != 0) {
      match = ExactMatchMethod(method, *parent);
    }
    if (match == 0) {
      deque<unicode_string>::const_iterator i = fromClass.fInterfaces.begin();
      for (; match == 0 && i != fromClass.fInterfaces.end(); ++i) {
	parent = LookupClass(*i);
	if (parent != 0) {
	  match = ExactMatchMethod(method, *parent);
	}
      }
    }
  }
  return match;
}

//
//  Method name : ExactMatchClassMethod
//  Description : If the provided class contains a method with the given
//    signature, but possibly a different return type, the matching method
//    info structure is returned.  Otherwise, 0 is returned.
//    Currently implemented as a linear search.
//
const CJavaMethodInfo*
CCompiler::ExactMatchClassMethod(const CJavaMethodSignature& method,
				 const CJavaClassFile& onClass) const
{
  const CJavaMethodInfo* match = 0;
  for (MethodTable::const_iterator i = onClass.fMethods.begin();
       match == 0 && i != onClass.fMethods.end(); ++i) {
    if (method.EqualsIgnoreReturn((*i).first)) {
      match = (*i).second;
    }
  }
  return match;
}

//
//  Method name : MoreSpecific
//  Description : Returns true if the first method signature is more
//    specific than the second.
//
bool
CCompiler::MoreSpecific(const CJavaMethodSignature& first,
			const CJavaMethodSignature& second) const
{
  bool specific = true;
  deque<CJavaTypeSignature>::const_iterator firstParameters =
    first.ParametersBegin();
  deque<CJavaTypeSignature>::const_iterator firstEnd =
    first.ParametersEnd();
  deque<CJavaTypeSignature>::const_iterator secondParameters =
    second.ParametersBegin();
  deque<CJavaTypeSignature>::const_iterator secondEnd =
    second.ParametersEnd();
  for (; specific && firstParameters != firstEnd;
       ++firstParameters, ++secondParameters) {
    if (!ImplicitCastTo(*firstParameters, *secondParameters)) {
      specific = false;
    }
  }
  return specific;
}

//
//  Method name : SameType
//  Description : Returns true if the provided types are identical in the eyes
//    of the compiler.  This should be used whenever there's a chance that
//    both types are class names.  The problem is, because of importing, 
//    'String' and 'java.lang.String' may both be valid names for the same
//    type, and only the compiler knows this.
//
bool
CCompiler::SameType(const CJavaTypeSignature& first,
		    const CJavaTypeSignature& second) const
{
  unicode_string firstName, secondName;
  return (first == second) ||
    (first.GetArrayBounds() == second.GetArrayBounds() &&
     first.GetBaseType() == second.GetBaseType() &&
     (!first.GetBaseClassName(firstName) ||
      !second.GetBaseClassName(secondName) ||
      LookupClass(firstName) == LookupClass(secondName)));      
}

//
//  Method name : ImplicitCastTo
//  Description : Returns true if implicit type conversion is allowed from
//    a value of type 'from' to a value of time 'to.'
//
bool
CCompiler::ImplicitCastTo(const CJavaTypeSignature& from,
			  const CJavaTypeSignature& to) const
{
  bool allowed = SameType(from, to) ||
    (from.IsReference() && to.IsReference() && AssignableSubtype(from, to));
  if (!allowed) {
    if (from.IsNumeric() && to.IsNumeric()) {
      CJavaTypeSignature toType = to.GetBaseType();
      switch (from.GetBaseType()) {
      case CJavaTypeSignature::Byte:
	if (toType == CJavaTypeSignature::Short) {
	  return true;
	}
      case CJavaTypeSignature::Short:
      case CJavaTypeSignature::Character:
	if (toType == CJavaTypeSignature::Integer) {
	  return true;
	}
      case CJavaTypeSignature::Integer:
	if (toType == CJavaTypeSignature::LongInteger) {
	  return true;
	}
      case CJavaTypeSignature::LongInteger:
	if (toType == CJavaTypeSignature::Float) {
	  return true;
	}
      case CJavaTypeSignature::Float:
	if (toType == CJavaTypeSignature::Double) {
	  return true;
	}
      default:
	break;
      }
    }
  }
  return allowed;
}

//
//  Method name : SameType
//  Description : Returns true if the provided method signatures are identical
//    in the eyes of the compiler.  This is needed because two signatures
//    may be lexically different but semantically identical.  For example:
//         void foo(Object o)
//         void foo(java.lang.Object o)
//
bool
CCompiler::SameType(const CJavaMethodSignature& first,
		    const CJavaMethodSignature& second) const
{
  bool result = first.GetName() == second.GetName() &&
    SameType(first.GetType(), second.GetType());
  if (result) {
    deque<CJavaTypeSignature>::const_iterator firstParameter =
      first.ParametersBegin();
    deque<CJavaTypeSignature>::const_iterator secondParameter =
      second.ParametersBegin();
    for (; result &&
	   firstParameter != first.ParametersEnd() &&
  	   secondParameter != second.ParametersEnd();
	 ++firstParameter, ++secondParameter) {
      result = SameType(*firstParameter, *secondParameter);
    }
    if (result) {
      result = firstParameter == first.ParametersEnd() &&
	secondParameter == second.ParametersEnd();
    }
  }
  return result;
}

//
//  Method name : NameClassConstant
//  Description : Java really, really screwed up the class constant format in
//    their bytecode.  If you are talking about a base (non-array) class type,
//    then you enter a string like "java/lang/String" into the constant pool,
//    but if you're talking about an array type (which is sorta a class type),
//    then you would put in a type signature like "[I" or "[LFoo;"
//    So, because this is annoying and inconsistent, I added a function here to
//    do the translation, and in the process it changes underqualified names
//    like 'String' to fully qualified ones like 'java/lang/String'.
//    It also assumes that any class names used in this type really do exist,
//    so you'd better check them using LookupClass before calling this.
//
unicode_string
CCompiler::NameClassConstant(const CJavaTypeSignature& type) const
{
  unicode_string result;
  unicode_string className;
  assert(type.IsReference());
  if (type.GetArrayBounds() == 0) {
    type.GetBaseClassName(className);
    const CJavaClassFile* classFile = LookupClass(className);
    assert(classFile != 0);
    result = classFile->GetClassName();
  } else {
    result = ::UTFToUnicode(type.Compile());
  }
  return result;
}

//
//  Method name : CheckType
//  Description : This method takes the provided type signature and verifies
//    that it actually denotes a usable type.  If it does, 0 is returned,
//    otherwise a compile error is returned that explains why the type is not
//    usable.  This indicates that a class name is used that cannot be found.
//
CCompileError*
CCompiler::CheckType(const CJavaTypeSignature& type) const
{
  CCompileError* error = 0;
  if (type.IsReference()) {
    unicode_string className;
    if (type.GetBaseClassName(className)) {
      if (LookupClass(className) == 0) {
	string errorString("Class ");
	errorString += ::UnicodeToString(className);
	errorString += " could not be found.";
	error = new CCompileError(errorString);
      }
    }
  }
  return error;
}

//
//  Method name : FixType
//  Description : If this type signature uses any class names that should be
//    fully qualified (i.e. 'String' instead of 'java/lang/String'), this
//    function will create a valid one and return it to the caller. Otherwise,
//    the original type will be returned.
//
CJavaTypeSignature
CCompiler::FixType(const CJavaTypeSignature& oldType) const
{
  unicode_string className;
  const CJavaClassFile* classFile;
  if (oldType.GetBaseClassName(className) &&
      (classFile = LookupClass(className)) != 0) {
    return CJavaTypeSignature(classFile->GetClassName(),
			      oldType.GetArrayBounds());
  } else {
    return oldType;
  }
}


//
//  Method name : PushFinallyHandler
//  Description : This method is used to indicate the beginning of a scope
//    goverened by a 'finally' clause which must be invoked regardless how
//    control flow leaves the body of a 'try' statement.
//    The instruction that is provided indicates the code instruction offset
//    from the beginning of the code for the current method.  This instruction
//    should be invoked as a subroutine to execute the appropriate 'finally'
//    code.
//
void
CCompiler::PushFinallyHandler(unsigned long instruction)
{
  fFinallyHandlers.push_back(instruction);
}

//
//  Method name : PopFinallyHandler
//  Description : This is the inverse of PushFinallyHandler.  It removes the
//    last-pushed finally instruction index from the list.
//
unsigned long
CCompiler::PopFinallyHandler()
{
  unsigned long back = fFinallyHandlers.back();
  fFinallyHandlers.pop_back();
  return back;
}

//
//  Method name : GetHandlerBegin
//  Description : This is used to get an iterator pointing to the first (most
//    recently pushed) handler instruction in the stack.  This iterator can
//    be used to cycle through all of the handlers.
//
CCompiler::FinallyHandlerStack::iterator
CCompiler::GetHandlerBegin()
{
  return fFinallyHandlers.begin();
}

//
//  Method name : GetHandlerEnd
//  Description : This method is used to get an iterator pointing past the
//    last (least recently pushed) handler instruction in the stack.
//
CCompiler::FinallyHandlerStack::iterator
CCompiler::GetHandlerEnd()
{
  return fFinallyHandlers.end();
}

//
//  Method name : PushStatementContext
//  Description : This method is used to declare that one statement is part
//    of the context of subsequent statements.  This is needed to implement
//    non-local 'break' and 'continue', which need to know where they are
//    located to properly implement their branches.
//
void
CCompiler::PushStatementContext(CStatement* statement)
{
  fStatementContexts.push_front(statement);
}

//
//  Method name : PopStatementContext
//  Description : This method is the converse of PushStatementContext ... it
//    removes the last-pushed statement from the context of future statements.
//
void
CCompiler::PopStatementContext()
{
  fStatementContexts.pop_front();
}

//
//  Method name : GetContextBegin
//  Description : This method allows some part of the compilation process
//    to get an iterator to the first statement in the enclosing context.
//
StatementList::iterator
CCompiler::GetContextBegin()
{
  return fStatementContexts.begin();
}

//
//  Method name : GetContextEnd
//  Description : This method allows some part of the compilation process
//    to get an iterator past the last statement in the enclosing context.
//
StatementList::iterator
CCompiler::GetContextEnd()
{
  return fStatementContexts.end();
}

//
//  Method name : IsThrowable
//  Description : Returns true if the provided type is a valid type for use
//    in a 'throw' statement.
//
bool
CCompiler::IsThrowable(const CJavaTypeSignature& type) const
{
  static const CJavaTypeSignature throwableType(kThrowableName);
  return type.IsReference() && (SameType(type, throwableType) ||
				AssignableSubtype(type, throwableType));
}

//
//  Method name : ParseError
//  Description : This call tells the compiler that the parser hit some
//    error on the provided line number and input.
//
void
CCompiler::ParseError(unsigned long lineNumber, const string& errorMessage,
		      const string& input)
{
  PrintCompileError(fFileName, errorMessage, lineNumber);
  cerr << "  on input: " << input << endl;
  fParseError = true;
}

//
//  Method name : ParseWarning
//  Description : This call tells the compiler that the parser hit some
//    error that should be reported as a non-fatal warning.
//
void
CCompiler::ParseWarning(unsigned long lineNumber, const string& errorMessage,
			const string& input)
{
  PrintCompileError(fFileName, errorMessage, lineNumber);
}

//
//  Method name : FindConstantField
//  Description : This method asks the compiler to try to find a field on the
//    provided class name with the given field signature.  If it can find an
//    appropriate field with a constant value, a new literal expression is
//    created and returne in the 'result' parameter, otherwise 'result' will
//    be set to 0.  If any errors are found during compilation, they are
//    returned, otherwise the function returns 0.
//    This is a little bit of a weird function because of the potential
//    ordering difficulties in trying to evaluate final fields.  For
//    example, the following is legal, and all three fields should have an
//    associated constant value of 5:
//      class t1 { final static int i = t3.i; }
//      class t2 { final static int i = 5; }
//      class t3 { final static int i = t2.i; }
//    This poses problems, since there is no guarantee that a field on a class
//    in the same file as this one has been evaluated yet.  So... this needs
//    to both check for fully-evaluated classes and half-finished ones.  In
//    the process, it may have to finish type evaluation and constant folding
//    on the other class, which may produce the compile error I am returning.
//
CCompileError*
CCompiler::FindConstantField(CExpression*& result,
   const unicode_string& className, const CJavaFieldSignature& fieldSignature,
   const CCompileContext& context, unsigned long lineNumber) const
{
  CCompileError* error = 0;
  result = 0;
  const CJavaClassFile* classFile = 0;
  const CIntermediateClass* intermediate = LookupIntermediateClass(className);
  if (intermediate != 0) {
    classFile = intermediate->fRealClass;
    assert(classFile != 0);
    const CJavaFieldInfo* fieldInfo = classFile->LookupField(fieldSignature);
    if (fieldInfo != 0 && fieldInfo->GetModifiers().fFinal != 0) {
      error =
	FindIntermediateConstantField(result, *intermediate, *fieldInfo);
      classFile = 0;
    }
  } else {
    classFile = LookupClass(className, context);
    if (classFile != 0) {
      const CJavaFieldInfo* fieldInfo = classFile->LookupField(fieldSignature);
      if (fieldInfo != 0 && fieldInfo->GetModifiers().fFinal != 0) {
	if (fieldInfo->IsConstant()) {
	  JavaConstantIndex index = fieldInfo->GetConstantIndex();
	  result = CreateLiteralFromConstant(index, *classFile,
					     fieldSignature.GetType());
	  WarnDeprecatedField(*fieldInfo, *classFile, context, lineNumber);
	}
	classFile = 0;
      }
    }
  }
  if (classFile != 0 && error == 0 && result == 0) {
    error = FindConstantField(result, classFile->fSuperclassName,
			      fieldSignature, context, lineNumber);
    deque<unicode_string>::const_iterator i = classFile->fInterfaces.begin();
    for (; error == 0 && result == 0 && i != classFile->fInterfaces.end();
	 ++i) {
      error = FindConstantField(result, *i, fieldSignature,
				context, lineNumber);
    }
  }
  return error;
}

//
//  Method name : FindIntermediateConstantField
//  Description : This method is used to check an intermediate class to see
//    if it provides a specified field with a constant value.
//
CCompileError*
CCompiler::FindIntermediateConstantField(CExpression*& result,
				       const CIntermediateClass& intermediate,
				       const CJavaFieldInfo& fieldInfo) const
{
  CCompileError* error = 0;
  const CJavaClassFile* classFile = intermediate.fRealClass;
  const StatementList* declarations =
    fieldInfo.GetModifiers().fStatic != 0 ?
    &intermediate.fStaticDeclarations :
    &intermediate.fNonStaticDeclarations;
  bool found = false;
  for (StatementList::const_iterator i = declarations->begin();
       !found && !(i == declarations->end()); ++i) {
    CDeclarationStatement* declaration =
      DYNAMIC_CAST(CDeclarationStatement, *i);
    assert(declaration != 0);
    deque<CVariableDeclaration*>::iterator field =
      declaration->fDeclarations->begin();
    for (; !found && !(field == declaration->fDeclarations->end());
	 ++field) {
      if (SameField((*field)->GetSignature(), fieldInfo.GetSignature())) {
	bool wasInClass = fInClassInitializers;
	CJavaMethodInfo dummyInfo;
	// casting away const
	CCompiler* nonConstThis = (CCompiler*)this;
	nonConstThis->fInClassInitializers = true;
	CCompileContext context(nonConstThis, &intermediate,
			(CJavaClassFile*)classFile, 0, &dummyInfo, 0);
	error = (*field)->GetConstantValue(result, context);
	found = true;
	nonConstThis->fInClassInitializers = wasInClass;
      }
    }
  }
  return error;
}

//
//  Method name : SameField
//  Description : This method returns true if the two field signatures match.
//
bool
CCompiler::SameField(const CJavaFieldSignature& first,
		     const CJavaFieldSignature& second) const
{
  return first.GetFieldName() == second.GetFieldName() &&
    SameType(first.GetType(), second.GetType());
}

//
//  Method name : CreateLiteralFromConstant
//  Description : This little static method is used to lookup an index in a
//    classes constant pool and then attempt to interpret that as a literal
//    expression value.  If this is possible, a new literal expression is
//    created and returned to the caller, otherwise 0 is returned.
//
CExpression*
CCompiler::CreateLiteralFromConstant(unsigned short index,
		const CJavaClassFile& onClass, const CJavaTypeSignature& type)
{
  CExpression* result = 0;
  const CJavaConstant* constant = onClass.LookupConstant(index);
  if (constant != 0) {
    const CJavaStringConstant* stringConstant =
      DYNAMIC_CAST(CJavaStringConstant, constant);
    if (stringConstant != 0) {
      constant = onClass.LookupConstant(stringConstant->GetStringIndex());
      const CJavaAscizConstant* ascizConstant =
	DYNAMIC_CAST(CJavaAscizConstant, constant);
      if (ascizConstant != 0) {
	result = new CStringLiteral(ascizConstant->GetUnicodeString());
      }
    } else {
      const CJavaIntegerConstant* integerConstant =
	DYNAMIC_CAST(CJavaIntegerConstant, constant);
      if (integerConstant != 0) {
	result = new COrdinalLiteral(integerConstant->GetInteger(), type);
      } else {
	const CJavaFloatConstant* floatConstant =
	  DYNAMIC_CAST(CJavaFloatConstant, constant);
	if (floatConstant != 0) {
	  result = new CFloatLiteral(floatConstant->GetFloat());
	} else {
	  const CJavaDoubleConstant* doubleConstant =
	    DYNAMIC_CAST(CJavaDoubleConstant, constant);
	  if (doubleConstant != 0) {
	    result = new CFloatLiteral(doubleConstant->GetDouble());
	  } else {
	    const CJavaLongConstant* longConstant =
	      DYNAMIC_CAST(CJavaLongConstant, constant);
	    if (longConstant != 0) {
	      result = new COrdinalLiteral(longConstant->GetLong());
	    }
	  }
	}
      }
    }
  }
  return result;
}

//
//  Method name : CreateConstantFromLiteral
//  Description : Given an expression that is assumed to be a literal value of
//    some type, this method creates an entry in the constant pool of the
//    provided class that can be used to mark a constant value. The index
//    into the constant pool is returned. 
//
unsigned short
CCompiler::CreateConstantFromLiteral(const CExpression* literal,
				     CJavaClassFile& onClass)
{
  unsigned short index;
  const COrdinalLiteral* intLiteral = DYNAMIC_CAST(COrdinalLiteral, literal);
  if (intLiteral != 0) {
    index = intLiteral->AddToConstantPool(onClass);
  } else {
    const CStringLiteral* stringLiteral =
      DYNAMIC_CAST(CStringLiteral, literal);
    if (stringLiteral != 0) {
      index = stringLiteral->AddToConstantPool(onClass);
    } else {
      const CFloatLiteral* floatLiteral = DYNAMIC_CAST(CFloatLiteral, literal);
      if (floatLiteral != 0) {
	index = floatLiteral->AddToConstantPool(onClass);
      } else {
	assert(0);
      }
    }
  }
  return index;
}

//
//  Method name : PrintCompileError
//  Description : Dumps out the provided message and line number in a standard
//    error format, to standard error.
//    If 'line' is 0, this assumes that no line number information is
//    available.
//
void
CCompiler::PrintCompileError(const string& fileName,
			     const unicode_string& message,
			     unsigned long line) const
{
  PrintCompileError(fileName, ::UnicodeToString(message), line);
}

//
//  Method name : PrintCompileError
//  Description : Dumps out the provided message and line number in a standard
//    error format, to standard error.
//    If 'line' is 0, this assumes that no line number information is
//    available.
//
void
CCompiler::PrintCompileError(const string& fileName, const string& message,
			     unsigned long line) const
{
  cerr << fileName << ":";
  if (line != 0) {
    cerr << line << ": ";
  }
  cerr << message << endl;
}

//
//  Method name : CheckValidOverride
//  Description : Checks to see if a method with the provided signature can
//    be used in a subclass of 'parentClass.'  If this is not allowed for some
//    reason, an informative error is returned, otherwise this method returns 0
//
CCompileError*
CCompiler::CheckValidOverride(const CJavaClassFile& parentClass,
			      const CIntermediateFunction& method)
{
  CCompileError* error = 0;
  pair<const CJavaClassFile*, const CJavaMethodInfo*>* match =
    ExactMatchMethod(method.fSignature, parentClass);
  if (match != 0) {
    assert(match->first != 0 && match->second != 0);
    const CJavaMethodInfo* otherMethod = match->second;
    CJavaAccessFlags parentModifiers = otherMethod->GetModifiers();
    if (parentModifiers.fFinal != 0) {
      unicode_string message =
	::StringToUnicode("Invalid override of final method ");
      message += method.fSignature.Disassemble();
      message += ::StringToUnicode(" on class ");
      message += match->first->GetClassName();
      error = new CCompileError(message, method.fStartLineNumber);
    } else if (!SameType(method.fSignature.GetType(),
			 otherMethod->GetSignature().GetType())) {
      string message("Cannot override method ");
      message += ::UnicodeToUTF(otherMethod->GetSignature().Disassemble());
      message += " with different return type.";
      error = new CCompileError(message, method.fStartLineNumber);
    } else if (method.fAccessFlags.MorePrivateThan(parentModifiers)) {
      string message("Cannot override method to be more private: ");
      message += ::UnicodeToUTF(otherMethod->GetSignature().Disassemble());
      error = new CCompileError(message, method.fStartLineNumber);
    } // XXXX else if ... check for throwing classes that parent does not ...
    delete match;
  }
  return error;
}

//
//  Method name : UnimplementedMethods
//  Description : This call is used to find out how many methods are visible,
//    but not defined, on this class.  This includes all abstract methods
//    inherited from parents.  A list of all method signatures that are not
//    implemented is given as the second argument and is filled over the course
//    of this call to contain all of the unimplemented method signatures.  This
//    list may contain duplicates if an undefined method signature is
//    inherited in multiple ways.
//
unsigned long
CCompiler::UnimplementedMethods(const CJavaClassFile& onClass,
				CCompiler::MethodList& methods)
{
  MethodList unimplementedParents;
  const CJavaClassFile* parent = LookupClass(onClass.fSuperclassName);
  if (parent != 0) {
    UnimplementedMethods(*parent, unimplementedParents);
  }
  for (MethodList::const_iterator i = unimplementedParents.begin();
       !(i == unimplementedParents.end()); ++i) {
    pair<const CJavaClassFile*, const CJavaMethodInfo*>* matchedMethod =
      ExactMatchMethod(*i, onClass);
    if (matchedMethod == 0 ||
	matchedMethod->second->GetModifiers().fAbstract != 0) {
      methods.push_back(*i);
    }
    delete matchedMethod;
  }
  UndeclaredInterfaceMethods(onClass, methods);
  for (MethodTable::const_iterator method = onClass.fMethods.begin();
       !(method == onClass.fMethods.end()); ++method) {
    if ((*method).second->GetModifiers().fAbstract != 0) {
      methods.push_back((*method).first);
    }
  }
  return methods.size();
}

//
//  Method name : UnimplementedInterfaceMethods
//  Description : This call is used to find out how many methods are visible,
//    but not defined, on this class, that it has inherited from its
//    interfaces.  A list of non-implemented methods is added to the second
//    argument.  This list may contain duplicates.
//
unsigned long
CCompiler::UndeclaredInterfaceMethods(const CJavaClassFile& onClass,
				      CCompiler::MethodList& methods)
{
  MethodList unimplementedParents;
  for (deque<unicode_string>::const_iterator i = onClass.fInterfaces.begin();
       i != onClass.fInterfaces.end(); ++i) {
    const CJavaClassFile* interface = LookupClass(*i);
    if (interface != 0) {
      UnimplementedMethods(*interface, unimplementedParents);
    }
  }
  for (MethodList::const_iterator i = unimplementedParents.begin();
       !(i == unimplementedParents.end()); ++i) {
    pair<const CJavaClassFile*, const CJavaMethodInfo*>* matchedMethod =
      ExactMatchMethod(*i, onClass);
    if (matchedMethod == 0 ||
	(matchedMethod->second->GetModifiers().fAbstract != 0 &&
	 matchedMethod->first != &onClass)){
      methods.push_back(*i);
    }
    delete matchedMethod;
  }
  return methods.size();
}

//
//  Method name : CheckJavaLangObject
//  Description : Checks to see if this compiler can find java.lang.Object
//    in the current class path.  Returs true if it can.
//
bool
CCompiler::CheckJavaLangObject()
{
  return fImportAliases.find(kObjectName) != fImportAliases.end() ||
    ImportClass(kObjectName) != 0;
}

//
//  Method name : WarnDeprecatedField
//  Description : If the provided field is deprecated, this will emit
//    a warning message to the user.
//    If line is '0', this assumes that no line number information
//    is available.
//
void
CCompiler::WarnDeprecatedField(const CJavaFieldInfo& field,
			       const CJavaClassFile& fieldClass,
			       const CCompileContext& context,
			       unsigned long line) const
{
  if (field.IsDeprecated()) {
    string fileName =
      ::UnicodeToString(context.GetClass().fSourceFile->GetFileName());
    string message = "Warning: field " +
      ::UnicodeToString(fieldClass.GetClassName()) + "." +
      ::UnicodeToString(field.GetSignature().GetFieldName()) +
      " has been deprecated.";
    PrintCompileError(fileName, message, line);
  }
}

//
//  Method name : WarnDeprecatedMethod
//  Description : If the provided method is deprecated, this will emit
//    a warning message to the user.
//    If line is '0', this assumes that no line number information
//    is available.
//
void
CCompiler::WarnDeprecatedMethod(const CJavaMethodInfo& method,
			       const CJavaClassFile& methodClass,
			       const CCompileContext& context,
			       unsigned long line) const
{
  if (method.IsDeprecated()) {
    string fileName =
      ::UnicodeToString(context.GetClass().fSourceFile->GetFileName());
    string message = "Warning: method " +
      ::UnicodeToString(method.GetSignature().Disassemble()) + " on class " +
      ::UnicodeToString(methodClass.GetClassName()) + " has been deprecated.";
    PrintCompileError(fileName, message, line);
  }
}

//
//  Method name : WarnDeprecatedClass
//  Description : If the provided class is deprecated, this will emit a
//    warning message to the user.
//    If line is '0', this assumes that no line number information
//    is available.
//
void
CCompiler::WarnDeprecatedClass(const CJavaClassFile& usedClass) const
{
  if (usedClass.IsDeprecated()) {
    cerr << "Warning: class " +
      ::UnicodeToString(usedClass.GetClassName()) + " has been deprecated.";
  }
}

//
//  Method name : GetCurrentClass
//  Description : Returns the class that is currently being parsed.
//
CIntermediateClass*
CCompiler::GetCurrentClass()
{
  if (fParsingClasses.empty()) {
    return 0;
  } else {
    return fParsingClasses.back();
  }
}

//
//  Method name : GetCurrentClass
//  Description : Returns the class that is currently being parsed.
//
const CIntermediateClass*
CCompiler::GetCurrentClass() const
{
  if (fParsingClasses.empty()) {
    return 0;
  } else {
    return fParsingClasses.back();
  }
}

//
//  Method name : InStatementBlock
//  Description : Returns true if the compiler is currently within a block
//    of statements.
//
bool
CCompiler::InStatementBlock() const
{
  return (GetCurrentClass()->fVariableScopes.size() > 0);
}

//
//  Method name : GenerateSyntheticConstructorArguments
//  Description : As of JDK 1.1, calls to create a 'new' object may need
//    to insert hidden arguments to allow local variable access in nested
//    blocks.  This method finds the intermediate class with the provided
//    name and tries to insert the correct arguments to insert into the
//    provided list.
//
void
CCompiler::GenerateSyntheticConstructorArguments(const unicode_string& name,
						 ExpressionList& into) const
{
  const CIntermediateClass* newClass = LookupIntermediateClass(name);
  if (newClass != 0) {
    if (!newClass->fSyntheticLocals.empty()) {
      for (list<COuterLocalExpression*>::const_iterator i =
	     newClass->fSyntheticLocals.begin();
	   !(i == newClass->fSyntheticLocals.end()); ++i) {
	CLocalVariableExpression* expression = new CLocalVariableExpression(
			     (*i)->GetLocalDeclaration().GetSignature(),
			     (*i)->GetLocalVariableIndex(),
			     (*i)->IsLocalFinal());
	into.push_back(expression);
      }
    }
  }
}

//
//  Method name : GenerateSyntheticCode
//  Description : This method is used to generate the synthetic code used
//    in constructors of inner class objects.  This requires inserting
//    assignments from hidden constructor arguments into hidden class
//    fields.
//
void
CCompiler::GenerateSyntheticCode(CCompileContext& context,
				 CCodeSequence& code)
{
  const CIntermediateFunction* method = context.GetIntermediateMethod();
  const CIntermediateClass* intermediate = context.GetIntermediateClass();
  assert(intermediate != 0 && method != 0 && intermediate->fInsideClass != 0);
  unsigned long lineNumber = method->fStartLineNumber;

  CJavaTypeSignature outerType(intermediate->fInsideClass->fName);
  JavaConstantIndex thisIndex =
    context.GetClass().AddFieldConstant(
			       intermediate->fName, kOuterThisName, outerType);
  code.Append(CJavaCodeAttribute::aload_0, lineNumber);
  code.Append(CJavaCodeAttribute::aload_1, lineNumber);
  code.Append(CJavaCodeAttribute::putfield, thisIndex, lineNumber);
  
  unsigned long localIndex = method->fRealParametersSize;
  for (list<COuterLocalExpression*>::const_iterator i =
	 intermediate->fSyntheticLocals.begin();
       !(i == intermediate->fSyntheticLocals.end()); ++i) {
    CVariableDeclaration declaration = (*i)->GetLocalDeclaration();
    CJavaTypeSignature type = FixType(declaration.GetType());
    JavaConstantIndex fieldIndex =
      context.GetClass().AddFieldConstant(intermediate->fName,
		   kSyntheticFieldPrefix + declaration.GetName(), type);
    code.Append(CJavaCodeAttribute::aload_0, lineNumber);
    if (type.IsReference()) {
      code.Append(CJavaCodeAttribute::aload, localIndex, lineNumber);
    } else {
      switch (type.GetBaseType()) {
      case CJavaTypeSignature::Double:
	code.Append(CJavaCodeAttribute::dload, localIndex, lineNumber);
	break;
      case CJavaTypeSignature::LongInteger:
	code.Append(CJavaCodeAttribute::lload, localIndex, lineNumber);
	break;
      case CJavaTypeSignature::Float:
	code.Append(CJavaCodeAttribute::fload, localIndex, lineNumber);
	break;
      default:
	code.Append(CJavaCodeAttribute::iload, localIndex, lineNumber);
	break;
      }
    }
    code.Append(CJavaCodeAttribute::putfield, fieldIndex, lineNumber);
    localIndex += type.GetWidth();
  }
}
