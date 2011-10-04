// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: NonlocalBranch.h,v 1.1 1996/03/31 19:35:29 geppetto Exp $
#ifndef _NonlocalBranch_h
#define _NonlocalBranch_h
#pragma interface

#include "CompileContext.h"

//
//  Class name : CNonlocalBranch
//  Description : The nonlocal branch object encapsulates a non-structed
//    branch from a deeper scope to a higher scope.  For example, if you
//    use 'break' in a switch or while body, you are generating one of these.
//    This bundles two important pieces of information.  The first is the
//    location of the branch offset in the code body.  The enclosing scope
//    needs to know where the branch instruction is so that it can go back and
//    insert the correct address for the code to jump to.
//    The other piece of information is the compile context that was in use
//    at the brach site.  This is important because it needs to be merged with
//    the context at the point where it branches to to correctly reflect the
//    initialization of local variables, etc.
//
class CNonlocalBranch {
public:
  CNonlocalBranch(unsigned long address, const CCompileContext& branchContext);
  CNonlocalBranch(const CNonlocalBranch& source);
  ~CNonlocalBranch();
  CNonlocalBranch& operator=(const CNonlocalBranch& source);

  const CCompileContext& GetContext() const { return fContext; }
  unsigned long GetAddress() const { return fBranchAddress; }

  CNonlocalBranch();
  bool operator==(const CNonlocalBranch& source) const;
  bool operator<(const CNonlocalBranch& source) const;
private:
  CCompileContext fContext;
  unsigned long fBranchAddress;
};

#endif
