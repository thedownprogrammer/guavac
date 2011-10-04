// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: CompileError.h,v 1.2 1996/01/08 03:03:38 geppetto Exp $
#ifndef _CompileError_h
#define _CompileError_h
#pragma interface

#include "unicode_string.h"

//
//  Class name : CCompileError
//  Description : This class is used by the compiler to transmit around the
//    information needed to represent an error during compilation.  This
//    typically includes the approximate source line of the error and a
//    human-readable error message.
//    Not intended for subclassing.
//
class CCompileError {
public:
  static const unsigned long kNoLineNumber;
  CCompileError(const unicode_string& message,
		unsigned long line = kNoLineNumber);
  CCompileError(const string& message,
		unsigned long line = kNoLineNumber);
  CCompileError(const CCompileError& source);
  ~CCompileError() {}
  CCompileError& operator=(const CCompileError& source);
  
  unicode_string GetMessage() const { return fMessage; }
  unsigned long GetLine() const { return fLineNumber; }
private:
  unicode_string fMessage;
  unsigned long fLineNumber;
};

#endif
