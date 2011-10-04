// Copyright (c) 1997  David Engberg  All rights reserved
// $Id: InnerClassesTable.C,v 1.1 1997/11/10 00:47:24 geppetto Exp $
#pragma implementation
#include "InnerClassesTable.h"
#include "JavaClassFile.h"
#include "JavaAccessFlags.h"
#include <vector>

//
//  Method name : CInnerClassesTable
//  Description : Default constructor.
//
CInnerClassesTable::CInnerClassesTable()
{
}

//
//  Method name : ~CInnerClassesTable
//  Description : Destructor.
//
CInnerClassesTable::~CInnerClassesTable()
{
  for (InnerClassTable::const_iterator i = fInnerClasses.begin();
       i != fInnerClasses.end(); ++i) {
    delete *i;
  }
}

//
//  Method name : InnerClassesBegin
//  Description : Returns a const iterator to the beginning of the list of
//    inner classes accessible from a class.
//
CInnerClassesTable::InnerClassTable::const_iterator
CInnerClassesTable::InnerClassesBegin() const
{
  return fInnerClasses.begin();
}

//
//  Method name : InnerClassesEnd
//  Description : Returns a const iterator to the end of the list of
//    inner classes accessible from a class.
//
CInnerClassesTable::InnerClassTable::const_iterator
CInnerClassesTable::InnerClassesEnd() const
{
  return fInnerClasses.end();
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable description of this class to
//    the provided stream.  For debugging purposes.
//
void
CInnerClassesTable::Disassemble(ostream& toStream) const
{
  if (Size() > 0) {
    toStream << "Inner Classes:" << endl;
    for (InnerClassTable::const_iterator i = fInnerClasses.begin();
	 i != fInnerClasses.end(); ++i) {
      toStream << "  ";
      (*i)->fAccessFlags.Disassemble(toStream);
      toStream << ::UnicodeToUTF((*i)->fSyntheticName);
      if ((*i)->fInnerName.length() > 0) {
	toStream << " (" << ::UnicodeToUTF((*i)->fInnerName) << ")";
      }
      if ((*i)->fOuterName.length() > 0) {
	toStream << " in " << ::UnicodeToUTF((*i)->fOuterName);
      }
      toStream << endl;
    }
  }
}

//
//  Method name : ParseBuffer
//  Description : Reads bytes from the current buffer position to try to
//    assemble a java innerClass table.  If it is successful, the new table
//    is returned, otherwiese it returns 0.
//
CInnerClassesTable*
CInnerClassesTable::ParseBuffer(string::const_iterator& buffer,
				const CJavaClassFile& classFile)
{
  CInnerClassesTable* result = new CInnerClassesTable;
  unsigned short tableSize = CJavaClassFile::ReadJavaU2(buffer);
  while (tableSize-- > 0) {
    unsigned short syntheticIndex = CJavaClassFile::ReadJavaU2(buffer);
    unsigned short outerIndex = CJavaClassFile::ReadJavaU2(buffer);
    unsigned short nameIndex = CJavaClassFile::ReadJavaU2(buffer);
    CJavaAccessFlags accessFlags(CJavaClassFile::ReadJavaU2(buffer));

    const CJavaClassConstant* syntheticConstant =
      DYNAMIC_CAST(CJavaClassConstant,
		   classFile.LookupConstant(syntheticIndex));
    const CJavaAscizConstant* nameConstant =
      DYNAMIC_CAST(CJavaAscizConstant, classFile.LookupConstant(nameIndex));
    const CJavaClassConstant* outerConstant =
      DYNAMIC_CAST(CJavaClassConstant, classFile.LookupConstant(outerIndex));
    const CJavaAscizConstant* syntheticNameConstant;
    if (syntheticConstant != 0 &&
	(syntheticNameConstant = DYNAMIC_CAST(CJavaAscizConstant,
	  classFile.LookupConstant(syntheticConstant->GetNameIndex()))) != 0) {
      unicode_string innerName;
      if (nameConstant != 0) {
	innerName = nameConstant->GetUnicodeString();
      }
      unicode_string outerName;
      if (outerConstant != 0) {
	const CJavaAscizConstant* outerNameConstant =
	  DYNAMIC_CAST(CJavaAscizConstant,
	       classFile.LookupConstant(outerConstant->GetNameIndex()));
	if (outerNameConstant != 0) {
	  outerName = outerNameConstant->GetUnicodeString();
	}
      }
      result->AddInnerClass(syntheticNameConstant->GetUnicodeString(),
			    innerName, accessFlags, outerName);
    } else {
      delete result;
      result = 0;
      break;
    }
  }
  return result;
}

//
//  Method name : Compile
//  Description : Dumps out this innerClass table as a sequence of bytes
//    formatted in the manner specified by the Java VM spec.
//
string
CInnerClassesTable::Compile(CJavaClassFile& inClass) const
{
  string buffer;
  CJavaClassFile::WriteJavaU2(buffer,
			      inClass.AddAscizConstant("InnerClasses"));
  unsigned long tableSize = 2 + 8 * Size();
  CJavaClassFile::WriteJavaU4(buffer, tableSize);
  CJavaClassFile::WriteJavaU2(buffer, Size());
  for (InnerClassTable::const_iterator i = fInnerClasses.begin();
       i != fInnerClasses.end(); ++i) {
    CJavaClassFile::WriteJavaU2(buffer,
			inClass.AddClassConstant((*i)->fSyntheticName));
    if ((*i)->fOuterName.length() > 0) {
      CJavaClassFile::WriteJavaU2(buffer,
			inClass.AddClassConstant((*i)->fOuterName));
    } else {
      CJavaClassFile::WriteJavaU2(buffer, 0);
    }
    if ((*i)->fInnerName.length() > 0) {
      CJavaClassFile::WriteJavaU2(buffer,
			inClass.AddAscizConstant((*i)->fInnerName));
    } else {
      CJavaClassFile::WriteJavaU2(buffer, 0);
    }
    CJavaClassFile::WriteJavaU2(buffer, (*i)->fAccessFlags.GetJavaFlags());
  }
  return buffer;
}

//
//  Method name : AddInnerClass
//  Description : Specifies that the provided class name should be part of this
//    innerClass table.
//
void
CInnerClassesTable::AddInnerClass(const unicode_string& syntheticName,
				  const unicode_string& innerName,
				  const CJavaAccessFlags& access,
				  const unicode_string& outerName)
{
  InnerClassInfo* info = new InnerClassInfo;
  info->fSyntheticName = syntheticName;
  info->fInnerName = innerName;
  info->fAccessFlags = access;
  info->fOuterName = outerName;
  fInnerClasses.push_back(info);
}

//
//  Method name : FindNonPrivate
//  Description : Looks for an inner class in this table that matches the
//    provided short name.  This will only return non-private inner classes,
//    which could be visible from other classes.  If no match is found,
//    this returns 0.
//
const CInnerClassesTable::InnerClassInfo*
CInnerClassesTable::FindNonPrivate(const unicode_string& name) const
{
  for (InnerClassTable::const_iterator i = fInnerClasses.begin();
       i != fInnerClasses.end(); ++i) {
    if ((*i)->fInnerName == name && (*i)->fAccessFlags.fPrivate == 0) {
      return (*i);
    }
  }
  return 0;
}
