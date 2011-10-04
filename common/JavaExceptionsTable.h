// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: JavaExceptionsTable.h,v 1.3 1996/07/12 20:42:34 geppetto Exp $
#ifndef _JavaExceptionsTable_h
#define _JavaExceptionsTable_h
#pragma interface

#include "JavaAttribute.h"
class ostream;

//
//  Class name : CJavaExceptionsTable
//  Description : This attribute stores the exceptions that a method is
//    declared.
//
class CJavaExceptionsTable : public CJavaAttribute {
  DynamicCastDeclarations;
public:
  static CJavaExceptionsTable* ParseBuffer(string::const_iterator& javaBuffer,
					   const CJavaClassFile& classFile);
  CJavaExceptionsTable();
  CJavaExceptionsTable(const deque<unicode_string>& exceptions);
  virtual ~CJavaExceptionsTable();
  void Disassemble(ostream& toStream) const;
  string Compile(CJavaClassFile& inClass) const;
  
  unsigned long Size() const { return fExceptions.size(); }

  void AddException(const unicode_string& className);
  deque<unicode_string>::const_iterator ThrowsBegin() const;
  deque<unicode_string>::const_iterator ThrowsEnd() const;

private:
  deque<unicode_string> fExceptions;
};

#endif
