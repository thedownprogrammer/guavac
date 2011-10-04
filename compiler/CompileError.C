// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: CompileError.C,v 1.2 1996/01/08 03:03:38 geppetto Exp $
#pragma implementation
#include "CompileError.h"

const unsigned long CCompileError::kNoLineNumber = 0;

//
//  Method name : CCompileError
//  Description : Constructor.
//
CCompileError::CCompileError(const unicode_string& message, unsigned long line)
  : fMessage(message),
    fLineNumber(line)
{
}

//
//  Method name : CCompileError
//  Description : Constructor.
//
CCompileError::CCompileError(const string& message, unsigned long line)
  : fMessage(::StringToUnicode(message)),
    fLineNumber(line)
{
}

//
//  Method name : CCompileError
//  Description : Copy constructor.
//
CCompileError::CCompileError(const CCompileError& source)
  : fMessage(source.fMessage),
    fLineNumber(source.fLineNumber)
{
}

//
//  Method name : operator=
//  Description : Assignment operator.
//
CCompileError&
CCompileError::operator=(const CCompileError& source)
{
  if (&source != this) {
    fMessage = source.fMessage;
    fLineNumber = source.fLineNumber;
  }
  return *this;
}
