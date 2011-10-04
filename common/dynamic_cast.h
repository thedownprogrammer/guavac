// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: dynamic_cast.h,v 1.2 1996/01/08 02:58:17 geppetto Exp $
#ifndef _dynamic_cast_h
#define _dynamic_cast_h

//
// Description : This is an extremely temporary ugly kludgy way of getting
//   dynamic cast functionality in g++ while I wait for real rtti.
//   This implementation isn't nice or portable in any way, but hopefully the
//   basic DYNAMIC_CAST macro can be redefined later to use the real
//   dynamic_cast mechanism, when available.
//   This is really only useful if you're trying to identify the absolute
//   bottom class in a tree:
//      A
//      | 
//      B 
//      | 
//      C
//
//  If I have pointers:      A* a1 = new B;   A* a2 = new C;
//  Then these will work correctly:
//     B* b = DYNAMIC_CAST(B, a1);   // returns (B*)a1
//     C* c = DYNAMIC_CAST(C, a2);   // returns (C*)a2
//     C* c = DYNAMIC_CAST(C, a1);   // returns (C*)0
//  But, this will not do the right thing:
//     B* b = DYNAMIC_CAST(B, a2);
//  This will return (B*)0, but a real rtti implementation should return
//  (B*)a2.  I.e. this only checks for the _most_derived_ type of an
//  instantiated object.
//
//  To use this in a class, the DynamicCastDeclarations macro needs to be
//  somewhere in the class declaration somewhere.
//

#define DYNAMIC_CAST(toclass, pointer) \
         (((pointer) != 0 && \
          ((void*)(pointer)->_dcast == (void*)(toclass::_dcast))) ? \
	  (toclass*)(pointer) : (toclass*)0)

#define DynamicCastDeclarations \
  public: \
    virtual void _dcast() {} \
  private: \
    //

#endif
