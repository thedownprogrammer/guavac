// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: JavaAccessFlags.C,v 1.4 1996/07/12 20:42:34 geppetto Exp $
#pragma implementation
#include "JavaAccessFlags.h"
#include <iostream.h>

const unsigned short ACC_PUBLIC = 0x1;
const unsigned short ACC_PRIVATE = 0x2;
const unsigned short ACC_PROTECTED = 0x4;
const unsigned short ACC_STATIC = 0x8;
const unsigned short ACC_FINAL = 0x10;
const unsigned short ACC_SYNCHRONIZED = 0x20;
const unsigned short ACC_TRANSIENT = 0x80;
const unsigned short ACC_NATIVE = 0x100;
const unsigned short ACC_INTERFACE = 0x200;
const unsigned short ACC_ABSTRACT = 0x400;
const unsigned short ACC_VOLATILE = 0x40;

//
//  Method name : CJavaAccessFlags
//  Description : Default constructor
//
CJavaAccessFlags::CJavaAccessFlags()
  : fPublic(0), fPrivate(0), fProtected(0), fStatic(0), fFinal(0),
    fSynchronized(0), fTransient(0), fNative(0),
    fInterface(0), fAbstract(0), fVolatile(0)
{
}

//
//  Method name : CJavaAccessFlags
//  Description : Constructor.  Uses the provided argument as a Java VM-
//    specified integer storing the bits for this access control structure.
//
CJavaAccessFlags::CJavaAccessFlags(unsigned short javaFlagWord)
{
  SetFlags(javaFlagWord);
}

//
//  Method name : operator=
//  Description : Assignment operator.
//
CJavaAccessFlags&
CJavaAccessFlags::operator=(const CJavaAccessFlags& source)
{
  if (&source != this) {
    fPublic = source.fPublic;
    fPrivate = source.fPrivate;
    fProtected = source.fProtected;
    fStatic = source.fStatic;
    fFinal = source.fFinal;
    fSynchronized = source.fSynchronized;
    fTransient = source.fTransient;
    fVolatile = source.fVolatile;
    fNative = source.fNative;
    fInterface = source.fInterface;
    fAbstract = source.fAbstract;
  }
  return *this;
}


//
//  Method name : GetJavaFlags
//  Description : Produces a compacted Java flag integer corresponding to the
//    format specified in the VM spec.
//
void
CJavaAccessFlags::SetFlags(unsigned short javaFlagWord)
{
  fPublic = (javaFlagWord & ACC_PUBLIC) ? 1 : 0;
  fPrivate = (javaFlagWord & ACC_PRIVATE) ? 1 : 0;
  fProtected = (javaFlagWord & ACC_PROTECTED) ? 1 : 0;
  fStatic = (javaFlagWord & ACC_STATIC) ? 1 : 0;
  fFinal = (javaFlagWord & ACC_FINAL) ? 1 : 0;
  fSynchronized = (javaFlagWord & ACC_SYNCHRONIZED) ? 1 : 0;
  fTransient = (javaFlagWord & ACC_TRANSIENT) ? 1 : 0;
  fVolatile = (javaFlagWord & ACC_VOLATILE) ? 1 : 0;
  fNative = (javaFlagWord & ACC_NATIVE) ? 1 : 0;
  fInterface = (javaFlagWord & ACC_INTERFACE) ? 1 : 0;
  fAbstract = (javaFlagWord & ACC_ABSTRACT) ? 1 : 0;
}


//
//  Method name : GetJavaFlags
//  Description : Produces a compacted Java flag integer corresponding to the
//    format specified in the VM spec.
//
unsigned short
CJavaAccessFlags::GetJavaFlags() const
{
  return
    (fPublic ? ACC_PUBLIC : 0) |
    (fPrivate ? ACC_PRIVATE : 0) |
    (fProtected ? ACC_PROTECTED : 0) |
    (fStatic ? ACC_STATIC : 0) |
    (fFinal ? ACC_FINAL : 0) |
    (fSynchronized ? ACC_SYNCHRONIZED : 0) |
    (fTransient ? ACC_TRANSIENT : 0) |
    (fVolatile ? ACC_TRANSIENT : 0) |
    (fNative ? ACC_NATIVE : 0) |
    (fInterface ? ACC_INTERFACE : 0) |
    (fAbstract ? ACC_ABSTRACT : 0);
}

//
//  Method name : Disassemble
//  Description : A little method to dump the set flags to the provided stream.
//
void
CJavaAccessFlags::Disassemble(ostream& toStream) const
{
  toStream << FlagNames();
}


//
//  Method name : FlagNames
//  Description : Returns a string holding the (human-readable) names of all
//    of the flags that are currently set on this object.
//    There's an extra space at the end, because I'm weak and lazy.
//
string
CJavaAccessFlags::FlagNames() const
{
  string outString;
  if (fPublic) { outString += "public "; }
  if (fPrivate) { outString += "private "; }
  if (fProtected) { outString += "protected "; }
  if (fStatic) { outString += "static "; }
  if (fFinal) { outString += "final "; }
  if (fSynchronized) { outString += "synchronized "; }
  if (fTransient) { outString += "transient "; }
  if (fVolatile) { outString += "volatile "; }
  if (fNative) { outString += "native "; }
  if (fInterface) { outString += "interface "; }
  if (fAbstract) { outString += "abstract "; }
  return outString;
}

//
//  Method name : Count
//  Description : Returns a count of the flags that are set on this object.
//
unsigned short
CJavaAccessFlags::Count() const
{
  return fPublic + fPrivate + fProtected + fStatic + fFinal + fSynchronized +
    fTransient + fVolatile + fNative + fInterface + fAbstract;
}

//
//  Method name : MorePrivateThan
//  Description : Returns true if this set of permissions is more private
//    than the provided one.
//
bool
CJavaAccessFlags::MorePrivateThan(const CJavaAccessFlags& other) const
{
  return CalculatePrivacy() > other.CalculatePrivacy();
}


//
//  Method name : CalculatePrivacy
//  Description : Returns an arbitrary number that represents how 'private'
//    these permissions are.  A set of permissions with a higher number than
//    these will be 'more private.'
//
int
CJavaAccessFlags::CalculatePrivacy() const
{
  return fPrivate ? 4 : (fProtected ? 2 : (fPublic ? 1 : 3));
}
