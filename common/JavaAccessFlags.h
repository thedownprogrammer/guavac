// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: JavaAccessFlags.h,v 1.3 1996/07/12 20:42:34 geppetto Exp $
#ifndef _JavaAccessFlags_h
#define _JavaAccessFlags_h
#pragma interface

#include <string>
class ostream;

//
//  Class name : CJavaAccessFlags
//  Description : Just a simple little structure to keep track of all of the
//    access flags used by Java classes, methods, etc.
//
struct CJavaAccessFlags {
public:
  CJavaAccessFlags();
  CJavaAccessFlags(unsigned short javaFlagWord);
  CJavaAccessFlags& operator=(const CJavaAccessFlags& source);

  void SetFlags(unsigned short javaFlagWord);
  unsigned short GetJavaFlags() const;
  void Disassemble(ostream& toStream) const;
  
  unsigned short Count() const;
  string FlagNames() const;
  bool MorePrivateThan(const CJavaAccessFlags& other) const;
  
  // all fields are publicly accessible
  unsigned int fPublic : 1;
  unsigned int fPrivate : 1;
  unsigned int fProtected : 1;
  unsigned int fStatic : 1;
  unsigned int fFinal : 1;
  unsigned int fSynchronized : 1;
  unsigned int fTransient : 1;
  unsigned int fNative : 1;
  unsigned int fInterface : 1;
  unsigned int fAbstract : 1;
  unsigned int fVolatile : 1;

private:
  int CalculatePrivacy() const;
};

#endif
