// $Id: JavaMethodSignature.C,v 1.5 1997/11/10 00:47:24 geppetto Exp $
// Copyright (c) 1995  David Engberg  All rights reserved
#include "JavaMethodSignature.h"

//
//  Method name : CJavaMethodSignature
//  Description : Default constructor.
//
CJavaMethodSignature::CJavaMethodSignature()
{
}

//
//  Method name : CJavaMethodSignature
//  Description : Constructs a method signature out of a the constituent
//    parts.
//
CJavaMethodSignature::CJavaMethodSignature(
			      const CJavaTypeSignature& returnType,
			      const unicode_string& name,
			      const deque<CJavaTypeSignature>& argumentTypes)
  : fMethodName(name),
    fArgumentTypes(argumentTypes),
    fReturnType(returnType)
{
  SetHashKey();
}

//
//  Method name : CJavaMethodSignature
//  Description : Copy constructor.
//
CJavaMethodSignature::CJavaMethodSignature(const CJavaMethodSignature& source)
  : fMethodName(source.fMethodName),
    fReturnType(source.fReturnType),
    fArgumentTypes(source.fArgumentTypes),
    fHashKey(source.fHashKey)
{
}

//
//  Method name : ~CJavaMethodSignature
//  Description : Destructor
//
CJavaMethodSignature::~CJavaMethodSignature()
{
}

//
//  Method name : operator=
//  Description : Assignment operator.
//
CJavaMethodSignature&
CJavaMethodSignature::operator=(const CJavaMethodSignature& source)
{
  if (&source != this) {
    fReturnType = source.fReturnType;
    fArgumentTypes = source.fArgumentTypes;
    fHashKey = source.fHashKey;
    fMethodName = source.fMethodName;
  }
  return *this;
}

//
//  Method name : EqualsIgnoreReturn
//  Description : Returns true if the name and arguments of this signature
//    matches the name and arguments of the provided signature.  The return
//    types do not affect the result of this function.
//
bool
CJavaMethodSignature::EqualsIgnoreReturn(
				const CJavaMethodSignature& other) const
{
  bool equal = fMethodName == other.fMethodName &&
    fArgumentTypes.size() == other.fArgumentTypes.size();
  if (equal) {
    deque<CJavaTypeSignature>::const_iterator i1 = fArgumentTypes.begin();
    deque<CJavaTypeSignature>::const_iterator i2 =
      other.fArgumentTypes.begin();
    for (; equal && i1 != fArgumentTypes.end(); ++i1, ++i2) {
      equal = (*i1) == (*i2);
    }
  }
  return equal;
}

//
//  Method name : Compare
//  Description : Compares the arguments of two CJavaMethodSignature objects
//    and returns 0 if they are equal, -1 if this is greater than the other and
//    1 if the other is greater than this.
//
int
CJavaMethodSignature::Compare(const CJavaMethodSignature& other) const
{
  int result = other.fHashKey - fHashKey;
  if (result == 0) {
    result = fMethodName.compare(other.fMethodName);
    if (result == 0) {
      result = fReturnType.Compare(other.fReturnType) ||
	(other.fArgumentTypes.size() - fArgumentTypes.size());
      if (result == 0) {
	deque<CJavaTypeSignature>::const_iterator i1 = fArgumentTypes.begin();
	deque<CJavaTypeSignature>::const_iterator i2 =
	  other.fArgumentTypes.begin();
	for (; i1 != fArgumentTypes.end(); ++i1, ++i2) {
	  if ((result = (*i1).Compare(*i2)) != 0) {
	    break;
	  }
	}
      }
    }
  }
  return result;
}

//
//  Method name : operator==
//  Description : Compares two method signatures for equality.
//
bool
CJavaMethodSignature::operator==(const CJavaMethodSignature& other) const
{
  return Compare(other) == 0;
}

//
//  Method name : operator<
//  Description : Returns true if this method signature does not match the
//    argument and the hash value for this signature is lower than the other.
//
bool
CJavaMethodSignature::operator<(const CJavaMethodSignature& other) const
{
  return Compare(other) > 0;
}


//
//  Method name : SetHashKey
//  Description : Internal method to set the hash key for this signature.
//
void
CJavaMethodSignature::SetHashKey()
{
  fHashKey = ::Hash(fMethodName) + fReturnType.Hash();
  for (deque<CJavaTypeSignature>::iterator i = fArgumentTypes.begin();
       i != fArgumentTypes.end(); ++i) {
    fHashKey += (*i).Hash();
  }
}


//
//  Method name : Initialize
//  Description : This function chews over the input strings and tries to
//     interpret it as a method signature.  If it is successful, then
//     this value is set appropriately and 'true' is returned.  If
//     not, then this value becomes invalid and a false value is given.
//
bool
CJavaMethodSignature::Initialize(const unicode_string& methodName,
				 const unicode_string& signatureString)
{
  fMethodName = methodName;
  unicode_string::const_iterator stringPointer = signatureString.begin();
  unicode_string::const_iterator stringEnd = signatureString.end();
  bool result = stringPointer < stringEnd && *stringPointer++ == '(';
  if (result) {
    fArgumentTypes.erase(fArgumentTypes.begin(), fArgumentTypes.end());
    while (stringPointer < stringEnd && *stringPointer != ')') {
      CJavaTypeSignature argument;
      bool valid = argument.ParseSignatureString(stringPointer, stringEnd);
      if (valid) {
	fArgumentTypes.push_back(argument);
      } else {
	result = false;
	break;
      }
    }
    result = result && *stringPointer++ == ')' && stringPointer < stringEnd;
    if (result) {
      result = fReturnType.ParseSignatureString(stringPointer, stringEnd);
    }
  }
  SetHashKey();
  return result;
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable description of this class to
//    the provided stream.  For debugging purposes.
//
void
CJavaMethodSignature::Disassemble(ostream& toStream) const
{
  fReturnType.Disassemble(toStream);
  toStream << " " << ::UnicodeToUTF(fMethodName) << "(";
  for (deque<CJavaTypeSignature>::const_iterator i = fArgumentTypes.begin();
       i != fArgumentTypes.end();) {
    (*i).Disassemble(toStream);
    i++;
    if (i != fArgumentTypes.end()) {
      toStream << ", ";
    }
  }
  toStream << ")";
}


//
//  Method name : Disassemble
//  Description : Returns a human-readable description of this signature in the
//    form of a string.
//
unicode_string
CJavaMethodSignature::Disassemble() const
{
  unicode_string result = fReturnType.Disassemble();
  result += (unicode_char)' ';
  result += fMethodName;
  result += (unicode_char)'(';
  for (deque<CJavaTypeSignature>::const_iterator i = fArgumentTypes.begin();
       i != fArgumentTypes.end();) {
    result += (*i).Disassemble();
    i++;
    if (i != fArgumentTypes.end()) {
      result += (unicode_char)',';
      result += (unicode_char)' ';
    }
  }
  result += (unicode_char)')';
  return result;
}

//
//  Method name : CompileType
//  Description : This method is used to produce the method type signature
//    string used in the Java VM class file format.  Spits out an appropriate
//    string, ready for use.
//
string
CJavaMethodSignature::CompileType() const
{
  string result;
  result += '(';
  for (deque<CJavaTypeSignature>::const_iterator i = fArgumentTypes.begin();
       i != fArgumentTypes.end(); ++i) {
    result += (*i).Compile();
  }
  result += ')';
  result += fReturnType.Compile();
  return result;
}

//
//  Method name : ParametersBegin
//  Description : Returns an iterator for the beginning of the list of
//    parameter types.
//
deque<CJavaTypeSignature>::const_iterator
CJavaMethodSignature::ParametersBegin() const
{
  return fArgumentTypes.begin();
}

//
//  Method name : ParametersEnd
//  Description : Returns an iterator for the end of the list of parameter
//    types.
//
deque<CJavaTypeSignature>::const_iterator
CJavaMethodSignature::ParametersEnd() const
{
  return fArgumentTypes.end();
}

//
//  Method name : CopyParameters
//  Description : Copies the list of parameters into the provided deque.
//
void
CJavaMethodSignature::CopyParameters(deque<CJavaTypeSignature>& copyTo) const
{
  copyTo = fArgumentTypes;
}
