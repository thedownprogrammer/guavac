// Copyright (c) 1997  David Engberg  All rights reserved
// $Id: LocalVariableRecord.C,v 1.1 1997/11/10 00:48:06 geppetto Exp $
#pragma implementation
#include "LocalVariableRecord.h"

//
//  Method name : CLocalVariableRecord
//  Description : Default constructor.  Don't use directly, just required
//    for templates.
//
CLocalVariableRecord::CLocalVariableRecord()
  : fDeclaration(),
    fIndex(0)
{
}

//
//  Method name : CLocalVariableRecord
//  Description : Constructor.
//
CLocalVariableRecord::CLocalVariableRecord(const CVariableDeclaration& decl,
					   VariableIndex index)
  : fDeclaration(decl),
    fIndex(index)
{
}

//
//  Method name : CLocalVariableRecord
//  Description : Copy constructor.
//
CLocalVariableRecord::CLocalVariableRecord(const CLocalVariableRecord& source)
  : fDeclaration(source.fDeclaration),
    fIndex(source.fIndex)
{
}

//
//  Method name : ~CLocalVariableRecord
//  Description : Destructor
//
CLocalVariableRecord::~CLocalVariableRecord()
{
}

//
//  Method name : SetVariableIndex
//  Description : Specifes the index of this local variable in the list for
//    the method.
//
void
CLocalVariableRecord::SetVariableIndex(VariableIndex index)
{
  fIndex = index;
}
