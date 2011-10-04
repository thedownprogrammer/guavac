// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: JavaExceptionsTable.C,v 1.3 1996/07/12 20:42:34 geppetto Exp $
#pragma implementation
#include "JavaExceptionsTable.h"
#include "JavaClassFile.h"

//
//  Method name : CJavaExceptionsTable
//  Description : Default constructor.
//
CJavaExceptionsTable::CJavaExceptionsTable()
{
}

//
//  Method name : CJavaExceptionsTable
//  Description : Creates an exception table from a provided list of exception
//    class names.  Assumes these are valid.
//
CJavaExceptionsTable::CJavaExceptionsTable(
				 const deque<unicode_string>& exceptions)
  : fExceptions(exceptions)
{
}

//
//  Method name : ~CJavaExceptionsTable
//  Description : Destructor
//
CJavaExceptionsTable::~CJavaExceptionsTable()
{
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable description of this class to
//    the provided stream.  For debugging purposes.
//
void
CJavaExceptionsTable::Disassemble(ostream& toStream) const
{
  toStream << "Throws Exceptions: ";
  for (deque<unicode_string>::const_iterator i = fExceptions.begin();
       i != fExceptions.end(); ++i) {
    toStream << ::UnicodeToString(*i) << " ";
  }
}

//
//  Method name : ParseBuffer
//  Description : Reads bytes from the current buffer position to try to
//    assemble a java exception table.  If it is successful, the new table
//    is returned, otherwiese it returns 0.
//
CJavaExceptionsTable*
CJavaExceptionsTable::ParseBuffer(string::const_iterator& buffer,
				  const CJavaClassFile& classFile)
{
  CJavaExceptionsTable* result = new CJavaExceptionsTable;
  unsigned short tableSize = CJavaClassFile::ReadJavaU2(buffer);
  while (tableSize-- > 0) {
    unsigned short index = CJavaClassFile::ReadJavaU2(buffer);
    const CJavaClassConstant* classConstant =
      DYNAMIC_CAST(CJavaClassConstant, classFile.LookupConstant(index));
    if (classConstant != 0) {
      const CJavaAscizConstant* stringConstant =
	DYNAMIC_CAST(CJavaAscizConstant,
		     classFile.LookupConstant(classConstant->GetNameIndex()));
      if (stringConstant != 0) {
	result->fExceptions.push_back(stringConstant->GetUnicodeString());
      } else {
	delete result;
	result = 0;
      }
    } else {
      delete result;
      result = 0;
    }
  }
  return result;
}

//
//  Method name : Compile
//  Description : Dumps out this exception table as a sequence of bytes
//    formatted in the manner specified by the Java VM spec.
//
string
CJavaExceptionsTable::Compile(CJavaClassFile& inClass) const
{
  string buffer;
  CJavaClassFile::WriteJavaU2(buffer, inClass.AddAscizConstant("Exceptions"));
  unsigned long tableSize = 2 + 2 * fExceptions.size();
  CJavaClassFile::WriteJavaU4(buffer, tableSize);
  CJavaClassFile::WriteJavaU2(buffer, fExceptions.size());
  for (deque<unicode_string>::const_iterator i = fExceptions.begin();
       i != fExceptions.end(); ++i) {
    CJavaClassFile::WriteJavaU2(buffer, inClass.AddClassConstant(*i));
  }
  return buffer;
}

//
//  Method name : ThrowsBegin
//  Description : Returns an iterator over the names of types that can be
//    thrown by this method.
//
deque<unicode_string>::const_iterator
CJavaExceptionsTable::ThrowsBegin() const
{
  return fExceptions.begin();
}

//
//  Method name : ThrowsEnd
//  Description : Returns an iterator for the end of the list of types that
//    can be thrown by this method.
//
deque<unicode_string>::const_iterator
CJavaExceptionsTable::ThrowsEnd() const
{
  return fExceptions.end();
}

//
//  Method name : AddException
//  Description : Specifies that the provided class name should be part of this
//    exception table.
//
void
CJavaExceptionsTable::AddException(const unicode_string& className)
{
  fExceptions.push_back(className);
}
