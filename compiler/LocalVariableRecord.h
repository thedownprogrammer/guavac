// Copyright (c) 1997  David Engberg  All rights reserved
// $Id: LocalVariableRecord.h,v 1.1 1997/11/10 00:48:06 geppetto Exp $
#ifndef _LocalVariableRecord_h
#define _LocalVariableRecord_h
#pragma interface

#include "VariableDeclaration.h"

//
//  Class name : CLocalVariableRecord
//  Description : This is a single record storing the information about a local
//    variable and its index in the variable table.
//
class CLocalVariableRecord {
public:
  typedef unsigned short VariableIndex;
  CLocalVariableRecord();
  CLocalVariableRecord(const CVariableDeclaration& declaration,
		       VariableIndex index);
  CLocalVariableRecord(const CLocalVariableRecord& source);
  virtual ~CLocalVariableRecord();
  
  VariableIndex GetVariableIndex() const { return fIndex; }
  void SetVariableIndex(VariableIndex index);
  CVariableDeclaration GetDeclaration() const { return fDeclaration; }

private:
  CVariableDeclaration fDeclaration;
  VariableIndex fIndex;
};

#endif
