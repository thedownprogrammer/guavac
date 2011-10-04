// Copyright (c) 1997  David Engberg  All rights reserved
// $Id: InnerClassesTable.h,v 1.1 1997/11/10 00:47:24 geppetto Exp $
#ifndef _InnerClassesTable_h
#define _InnerClassesTable_h
#pragma interface

#include "JavaAttribute.h"
class ostream;

//
//  Class name : CInnerClassesTable
//  Description : This attribute stores the list of inner classes that are
//    contained within a class.
//
class CInnerClassesTable : public CJavaAttribute {
  DynamicCastDeclarations;
public:
  static CInnerClassesTable* ParseBuffer(string::const_iterator& javaBuffer,
					 const CJavaClassFile& classFile);
  CInnerClassesTable();
  virtual ~CInnerClassesTable();

  void Disassemble(ostream& toStream) const;
  string Compile(CJavaClassFile& inClass) const;
  
  void AddInnerClass(const unicode_string& syntheticName,
		     const unicode_string& innerName,
		     const CJavaAccessFlags& access,
		     const unicode_string& outerName);

  typedef struct {
    unicode_string fSyntheticName;
    unicode_string fInnerName;
    CJavaAccessFlags fAccessFlags;
    unicode_string fOuterName;
  } InnerClassInfo;
  typedef vector<InnerClassInfo*> InnerClassTable;

  unsigned long Size() const { return fInnerClasses.size(); }
  InnerClassTable::const_iterator InnerClassesBegin() const;
  InnerClassTable::const_iterator InnerClassesEnd() const;

  const InnerClassInfo* FindNonPrivate(const unicode_string& name) const;

private:
  InnerClassTable fInnerClasses;
};

#endif
