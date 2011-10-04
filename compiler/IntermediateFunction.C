// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: IntermediateFunction.C,v 1.6 1997/11/10 00:48:06 geppetto Exp $
#pragma implementation
#include "IntermediateFunction.h"
#include "Statement.h"

//
//  Method name : CIntermediateFunction
//  Description : Constructs an intermediate function out of its parts.
//
CIntermediateFunction::CIntermediateFunction(
			const CJavaMethodSignature& signature,
			const CJavaAccessFlags& modifiers,
			deque<unicode_string>* adoptThrows,
			unsigned long startLineNumber,
			bool deprecated)
  : fSignature(signature),
    fMaxLocalVariables(0),
    fRealParametersSize(0),
    fSyntheticParametersSize(0),
    fBlock(0),
    fAccessFlags(modifiers),
    fStartLineNumber(startLineNumber),
    fDeprecated(deprecated),
    fMethodInfoAlias(0)
{
  if (adoptThrows != 0) {
    fThrows = *adoptThrows;
    delete adoptThrows;
  }
}

//
//  Method name : ~CIntermediateFunction
//  Description : Destructor
//
CIntermediateFunction::~CIntermediateFunction()
{
  delete fBlock;
}

//
//  Method name : GetLocalVariableLocation
//  Description : The local variable offsets that are originally assigned
//    are later thrown off by the stupid synthetic constructor parameters
//    that need to be inserted to support access to outer final local
//    variables.  As a result, any access to a local variable slot needs
//    to call this to make sure its offset is correct.
//
unsigned short
CIntermediateFunction::GetLocalVariableLocation(unsigned short from) const
{
  if (from >= fRealParametersSize) {
    return from + fSyntheticParametersSize;
  }
  return from;
}
