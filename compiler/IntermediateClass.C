// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: IntermediateClass.C,v 1.7 1997/11/10 00:48:06 geppetto Exp $
#pragma implementation
#include "IntermediateClass.h"
#include "IntermediateFunction.h"
#include "LocalVariableRecord.h"
#include "Statement.h"
#include "Expression.h"

//
//  Method name : CIntermediateClass
//  Description : Constructs a class.
//
CIntermediateClass::CIntermediateClass(const string& sourceFileName,
				       const unicode_string& className,
				       const unicode_string& shortName,
				       CJavaAccessFlags* adoptModifiers,
				       unicode_string* adoptExtends,
				       deque<unicode_string>* adoptInterfaces,
				       bool deprecated)
  : fSourceFileName(sourceFileName),
    fName(className),
    fShortName(shortName),
    fExtends(adoptExtends),
    fStaticLocalVariables(0),
    fAnyConstructors(false),
    fStaticInitializer(0),
    fRealClass(0),
    fDeprecated(deprecated),
    fInsideClass(0),
    fIsInner(false),
    fIsAnonymous(false),
    fSyntheticInnerCount(0),
    fCurrentFunction(0),
    fCurrentLocalVariable(0)
{
  if (adoptModifiers != 0) {
    fAccessFlags = *adoptModifiers;
    delete adoptModifiers;
  }
  if (adoptInterfaces != 0) {
    fInterfaces = *adoptInterfaces;
    delete adoptInterfaces;
  }
}

//
//  Method name : ~CIntermediateClass
//  Description : Destructor
//
CIntermediateClass::~CIntermediateClass()
{
  delete fExtends;
  for (deque<CIntermediateFunction*>::iterator i = fFunctions.begin();
       i != fFunctions.end(); i++) {
    delete *i;
  }
  delete fStaticInitializer;
  for (StatementList::iterator i = fStaticDeclarations.begin();
       !(i == fStaticDeclarations.end()); ++i) {
    delete *i;
  }
  for (StatementList::iterator i = fNonStaticDeclarations.begin();
       !(i == fNonStaticDeclarations.end()); ++i) {
    delete *i;
  }
  for (list<COuterLocalExpression*>::iterator i = fSyntheticLocals.begin();
       !(i == fSyntheticLocals.end()); ++i) {
    delete *i;
  }
  delete fRealClass;
}

//
//  Method name : GetShortName
//  Description : This method is used to get the part of the class name after
//    the last package delimiter.  For example, if this class name is
//    java.foo.Blah, then this will return "Blah".  If this class name is
//    just "Blah", then it will return "Blah".
//
unicode_string
CIntermediateClass::GetShortName() const
{
  return fShortName;
}
