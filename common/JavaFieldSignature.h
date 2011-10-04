// $Id: JavaFieldSignature.h,v 1.3 1996/01/08 03:03:38 geppetto Exp $
// Copyright (c) 1995  David Engberg  All rights reserved
#ifndef _JavaFieldSignature_h
#define _JavaFieldSignature_h

#include "JavaTypeSignature.h"
class ostream;

//
//  Class name : CJavaFieldSignature
//  Description : This class encapsulates the entire signature for a field
//    on a class.  This signature can be used to quickly search for a
//    desired field at runtime.
//    The three parts that make up a Java field signature are:
//      the class it belongs to, a string
//      the name of the field, a string
//      the type of the field.  this is represented as a string in the 
//        Java bytecode class file format, but is represented by a
//        CJavaTypeSignature in this implementation.
//    The 'Initialize' function must be called successfully on this object
//    before it is valid for use.
//
class CJavaFieldSignature {
public:
  CJavaFieldSignature();
  CJavaFieldSignature(const CJavaTypeSignature& type,
		      const unicode_string& name);
  CJavaFieldSignature(const CJavaFieldSignature& source);
  ~CJavaFieldSignature();
  CJavaFieldSignature& operator=(const CJavaFieldSignature& source);
  bool operator==(const CJavaFieldSignature& other) const;
  bool operator<(const CJavaFieldSignature& other) const;
  int Compare(const CJavaFieldSignature& other) const;

  unicode_string GetFieldName() const { return fFieldName; }
  CJavaTypeSignature GetType() const { return fType; }

  bool Initialize(const unicode_string& fieldName,
		  const unicode_string& signatureString);

  unsigned long Hash() const { return fHashKey; }
  
  void Disassemble(ostream& toStream) const;
private:
  unicode_string fFieldName;
  CJavaTypeSignature fType;
  unsigned long fHashKey;
};

#endif
