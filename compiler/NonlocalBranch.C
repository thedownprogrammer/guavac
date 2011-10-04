// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: NonlocalBranch.C,v 1.2 1997/11/10 00:48:06 geppetto Exp $
#pragma implementation
#include "NonlocalBranch.h"
#include <cassert>

//
//  Method name : CNonlocalBranch
//  Description : Default constructor required by list<>.  Don't use directly.
//
CNonlocalBranch::CNonlocalBranch()
  : fContext(0, 0, 0, 0, 0, 0)
{
  assert(0);
}

//
//  Method name : CNonlocalBranch
//  Description : Constructs a non-local branch from the location of its branch
//    address and the context the branch occurs under.
//
CNonlocalBranch::CNonlocalBranch(unsigned long address,
				 const CCompileContext& branchContext)
  : fContext(branchContext),
    fBranchAddress(address)
{
}

//
//  Method name : CNonlocalBranch
//  Description : Copy constructor.
//
CNonlocalBranch::CNonlocalBranch(const CNonlocalBranch& source)
  : fContext(source.fContext),
    fBranchAddress(source.fBranchAddress)
{
}

//
//  Method name : ~CNonlocalBranch
//  Description : Destructor
//
CNonlocalBranch::~CNonlocalBranch()
{
}

//
//  Method name : operator=
//  Description : Assignment operator.
//
CNonlocalBranch&
CNonlocalBranch::operator=(const CNonlocalBranch& source)
{
  if (&source != this) {
    fContext = source.fContext;
    fBranchAddress = source.fBranchAddress;
  }
  return *this;
}

//
//  Method name : operator==
//  Description : Equality operator.  Don't use directly ... demanded by list<>
//
bool
CNonlocalBranch::operator==(const CNonlocalBranch& source) const
{
  return fBranchAddress == source.fBranchAddress;
}

//
//  Method name : operator<
//  Description : Equality operator.  Don't use directly ... demanded by list<>
//
bool
CNonlocalBranch::operator<(const CNonlocalBranch& source) const
{
  return fBranchAddress < source.fBranchAddress;
}
