// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: JavaClassFile.C,v 1.11 1997/11/10 00:47:24 geppetto Exp $
#pragma implementation
#include "JavaClassFile.h"
#include "JavaFieldInfo.h"
#include "JavaMethodInfo.h"
#include "JavaAttribute.h"
#include "JavaConstant.h"
#include "InnerClassesTable.h"
#include <iostream.h>
extern "C" {
  #include <sys/types.h> // in.h requires u_long etc. by Masayuki Ida 96.1.10
  #include <netinet/in.h>
}
#include "config.h"

// They re-used this flag, which is the same value as ACC_SYNCHRONIZED,
// but it is only relevant in class files.  Every emitted class file should
// use this value.
const unsigned short ACC_SUPER = 0x20;

//
//  Method name : CJavaClassFile
//  Description : Default constructor.
//
CJavaClassFile::CJavaClassFile()
  : fSourceFile(0),
    fInnerClasses(0),
    fDeprecated(false)
{
  // spec says to skip constant[0], which is always unused.
  fConstants.push_back((CJavaConstant*)0);
}

//
//  Method name : ~CJavaClassFile
//  Description : Destructor
//
CJavaClassFile::~CJavaClassFile()
{
  for (ConstantPool::iterator i = fConstants.begin();
       i < fConstants.end(); i++) {
    delete *i;
  }
  for (FieldTable::iterator i = fFields.begin(); i != fFields.end(); i++) {
    delete (*i).second;
  }
  for (MethodTable::iterator i = fMethods.begin(); i != fMethods.end(); i++) {
    (*i).second->RemoveReference();
  }
  delete fSourceFile;
  delete fInnerClasses;
}


//
//  Method name : ParseFromStream
//  Description : This function allows a user to take an input stream and try
//     to interpret it as a Java class bytecode file.  If this operation is
//     successful, a pointer to the new class is returned, otherwise 0 is
//     returned.  If the 'interfaceOnly' flag is true, then the class file is
//     loaded without any code attributes for the methods.  This can be
//     used whenever the goal is to get the signatures of methods (like
//     during compilation) wihtout the implementations.
//
CJavaClassFile*
CJavaClassFile::ParseFromStream(istream& inStream, bool interfaceOnly)
{
  string buffer;
  // inefficient, yeah.  I hate iostreams.
  int c;
  while ((c = inStream.get()) != EOF) {
    buffer += (char)c;
  }
  CJavaClassFile* result = ParseFromString(buffer, interfaceOnly);
  return result;
}


//
//  Method name : ParseFromString
//  Description : Tries to parse the provided string as if it were a Java
//    input file.  If it succeeds, this class should be fully initialized.
//    If it fails, 0 is returned.  If the 'interfaceOnly' flag is true,
//    then the class file is loaded without any code attributes for the
//    methods.  This can be used whenever the goal is to get the signatures
//    of methods (like during compilation) wihtout the implementations.
//
CJavaClassFile*
CJavaClassFile::ParseFromString(const string& fileBuffer, bool interfaceOnly)
{
  string::const_iterator bufferPointer = fileBuffer.begin();
  unsigned long magic = ReadJavaU4(bufferPointer);
  CJavaClassFile* result = (magic == 0xCAFEBABE) ? new CJavaClassFile : 0;
  if (result != 0) {
    result->fVersion = ReadJavaU4(bufferPointer);
    unsigned short poolCount = ReadJavaU2(bufferPointer);
    result->fConstants.reserve(poolCount);
    for (unsigned short index = 1; result != 0 && index < poolCount; index++) {
      bool doubleWidth = false;
      CJavaConstant* constant = 0;
      JavaConstantIndex index1, index2;
      switch(*bufferPointer++) {
      case CJavaConstant::kClass:
	index1 = ReadJavaU2(bufferPointer);
	constant = new CJavaClassConstant(index1);
	break;
      case CJavaConstant::kField:
	index1 = ReadJavaU2(bufferPointer);
	index2 = ReadJavaU2(bufferPointer);
	constant = new CJavaFieldConstant(index1, index2);
	break;
      case CJavaConstant::kMethod:
	index1 = ReadJavaU2(bufferPointer);
	index2 = ReadJavaU2(bufferPointer);
	constant = new CJavaMethodConstant(index1, index2);
	break;
      case CJavaConstant::kString:
	index1 = ReadJavaU2(bufferPointer);
	constant = new CJavaStringConstant(index1);
	break;
      case CJavaConstant::kInteger:
	{long intValue = ReadJavaU4(bufferPointer);
	constant = new CJavaIntegerConstant(intValue);}
	break;
      case CJavaConstant::kFloat:
	{
	  long dummyLong;
	  ::memcpy(&dummyLong, bufferPointer, 4);
	  dummyLong = ntohl(dummyLong);  // why isn't there a ntohf function?
	  float floatValue;
	  ::memcpy(&floatValue, &dummyLong, 4);
	  bufferPointer += 4;
	  constant = new CJavaFloatConstant(floatValue);
	}
	break;
      case CJavaConstant::kDouble:
	{
	  long dummyLong1, dummyLong2;
	  ::memcpy(&dummyLong1, bufferPointer, 4);
	  dummyLong1 = ntohl(dummyLong1);
	  bufferPointer += 4;
	  ::memcpy(&dummyLong2, bufferPointer, 4);
	  dummyLong2 = ntohl(dummyLong2);
	  bufferPointer += 4;
	  double doubleValue;
	  #ifdef WORDS_BIGENDIAN
	    ::memcpy(&doubleValue, &dummyLong1, 4);
	    ::memcpy(((char*)&doubleValue) + 4, &dummyLong2, 4);
	  #else
	    ::memcpy(((char*)&doubleValue) + 4, &dummyLong1, 4);
	    ::memcpy(&doubleValue, &dummyLong2, 4);
          #endif
	  constant = new CJavaDoubleConstant(doubleValue);
	  doubleWidth = true;
	}
	break;
      case CJavaConstant::kLong:
	{
	  unsigned long oneWord = ReadJavaU4(bufferPointer);
	  long long longValue;
	  #ifdef WORDS_BIGENDIAN
	  ::memcpy(((char*)&longValue), &oneWord, 4);
	  #else
	  ::memcpy(((char*)&longValue) + 4, &oneWord, 4);
	  #endif
	  oneWord = ReadJavaU4(bufferPointer);
	  #ifdef WORDS_BIGENDIAN
	  ::memcpy(((char*)&longValue) + 4, &oneWord, 4);
	  #else
	  ::memcpy(((char*)&longValue), &oneWord, 4);
	  #endif
	  constant = new CJavaLongConstant(longValue);
	  doubleWidth = true;
	}
	break;
      case CJavaConstant::kInterface:
	index1 = ReadJavaU2(bufferPointer);
	index2 = ReadJavaU2(bufferPointer);
	constant = new CJavaInterfaceConstant(index1, index2);
	break;
      case CJavaConstant::kNameType:
	index1 = ReadJavaU2(bufferPointer);
	index2 = ReadJavaU2(bufferPointer);
	constant = new CJavaNameTypeConstant(index1, index2);
	break;
      case CJavaConstant::kAsciz:
	{unsigned short stringLength = ReadJavaU2(bufferPointer);
	constant = new CJavaAscizConstant(bufferPointer, stringLength);
	bufferPointer += stringLength;}
	break;
      case CJavaConstant::kUnicode:
	{unsigned short stringLength = ReadJavaU2(bufferPointer);
	 unicode_string newString;
	 while (stringLength-- > 0) {
	   newString += ReadJavaU2(bufferPointer);
	 }
	 constant = new CJavaAscizConstant(newString);
	 break;}
      }
      if (constant == 0) {
	delete result;
	result = 0;
      } else {
	result->fConstants.push_back(constant);
	if (doubleWidth) {
	  index++;
	  result->fConstants.push_back((CJavaConstant*)0);
	}
	if (bufferPointer >= fileBuffer.end()) { delete result; result = 0; }
      }
    }
  }
  if (bufferPointer >= fileBuffer.end()) { delete result; result = 0; }
  if (result != 0) {
    unsigned short flagShort = ReadJavaU2(bufferPointer);
    result->fAccessFlags.SetFlags(flagShort & (~ACC_SUPER));
    JavaConstantIndex thisClassIndex = ReadJavaU2(bufferPointer);
    const CJavaClassConstant* tempClassConstant =
      DYNAMIC_CAST(CJavaClassConstant, result->LookupConstant(thisClassIndex));
    if (tempClassConstant == 0) {
      delete result; result = 0;
    } else {
      JavaConstantIndex thisClassNameIndex = tempClassConstant->GetNameIndex();
      const CJavaAscizConstant* tempAscizConstant =
	DYNAMIC_CAST(CJavaAscizConstant,
		     result->LookupConstant(thisClassNameIndex));
      if (tempAscizConstant == 0) {
	delete result; result = 0;
      } else {
	result->fThisClassName = tempAscizConstant->GetUnicodeString();
      }
    }
    if (result != 0) {
      JavaConstantIndex superclassIndex = ReadJavaU2(bufferPointer);
      if (superclassIndex > 0) {   // java.lang.Object has superclass index 0
	tempClassConstant =
	  DYNAMIC_CAST(CJavaClassConstant,
		       result->LookupConstant(superclassIndex));
	if (tempClassConstant == 0) {
	  delete result; result = 0;
	} else {
	  JavaConstantIndex superclassNameIndex =
	    tempClassConstant->GetNameIndex();
	  const CJavaAscizConstant* tempAscizConstant =
	    DYNAMIC_CAST(CJavaAscizConstant,
			 result->LookupConstant(superclassNameIndex));
	  if (tempAscizConstant == 0) {
	    delete result; result = 0;
	  } else {
	    result->fSuperclassName = tempAscizConstant->GetUnicodeString();
	  }
	}
      }
    }
  }
  if (bufferPointer >= fileBuffer.end()) { delete result; result = 0; }
  if (result != 0) {
    unsigned short interfaceCount = ReadJavaU2(bufferPointer);
    if (bufferPointer + 2 * interfaceCount >= fileBuffer.end()) {
      delete result; result = 0;
    }
    while (result != 0 && interfaceCount-- > 0) {
      JavaConstantIndex interface = ReadJavaU2(bufferPointer);
      const CJavaClassConstant* tempClassConstant =
	DYNAMIC_CAST(CJavaClassConstant, result->LookupConstant(interface));
      if (tempClassConstant == 0) {
	delete result; result = 0;
      } else {
	JavaConstantIndex nameIndex = tempClassConstant->GetNameIndex();
	const CJavaAscizConstant* tempAscizConstant =
	  DYNAMIC_CAST(CJavaAscizConstant,
		       result->LookupConstant(nameIndex));
	if (tempAscizConstant == 0) {
	  delete result; result = 0;
	} else {
	  result->fInterfaces.push_back(tempAscizConstant->GetUnicodeString());
	}
      }
    }
  }
  if (bufferPointer >= fileBuffer.end()) { delete result; result = 0; }
  if (result != 0) {
    unsigned short fieldCount = ReadJavaU2(bufferPointer);
    while (fieldCount-- > 0 && result != 0) {
      CJavaFieldInfo* fieldInfo =
	CJavaFieldInfo::ParseBuffer(bufferPointer, *result);
      if (fieldInfo == 0) {
	delete result; result = 0;
      } else {
	result->fFields.insert(
 	       FieldTable::value_type(fieldInfo->GetSignature(), fieldInfo));
      }
    }
  }
  if (bufferPointer >= fileBuffer.end()) { delete result; result = 0; }
  if (result != 0) {
    unsigned short methodCount = ReadJavaU2(bufferPointer);
    while (methodCount-- > 0 && result != 0) {
      CJavaMethodInfo* method =
	CJavaMethodInfo::ParseBuffer(bufferPointer, *result, interfaceOnly);
      if (method == 0) {
	delete result; result = 0;
      } else {
	result->InsertMethod(method);
      }
      if (bufferPointer >= fileBuffer.end()) { delete result; result = 0; }
    }
  }
  if (bufferPointer >= fileBuffer.end()) { delete result; result = 0; }
  if (result != 0) {
    unsigned short attributeCount = ReadJavaU2(bufferPointer);
    while (attributeCount-- > 0 && result != 0) {
      CJavaAttribute* attribute =
	CJavaAttribute::ParseBuffer(bufferPointer, *result);
      if (attribute != 0) {
	CJavaSourceFileAttribute* sourceFile =
	  DYNAMIC_CAST(CJavaSourceFileAttribute, attribute);
	if (sourceFile != 0) {
	  delete result->fSourceFile;
	  result->fSourceFile = sourceFile;
	} else if (DYNAMIC_CAST(CJavaDeprecatedAttribute, attribute) != 0) {
	  result->fDeprecated = true;
	  delete attribute;
	} else if (DYNAMIC_CAST(CInnerClassesTable, attribute) != 0) {
	  result->fInnerClasses = (CInnerClassesTable*)attribute;
	} else {
	  delete attribute;
	}
      }
    }
  }
  if (bufferPointer != fileBuffer.end()) {
    delete result; result = 0;
  }
  return result;
}


//
//  Method name : ReadJavaU2
//  Description : This assumes that the buffer being referenced is part of a
//    Java class definition file, and that the next two bytes correspond to
//    a two-byte integer written in big-endian order.  This function extracts
//    that value and returns it as a short.
//    The side-effect of this function is that the referenced iterator is
//    advanced by two characters.
//
unsigned short
CJavaClassFile::ReadJavaU2(string::const_iterator& bufferPointer)
{
  unsigned short netshort = 0;
  ::memcpy(&netshort, bufferPointer, 2);
  bufferPointer += 2;
  return ntohs(netshort);
}


//
//  Method name : ReadJavaU4
//  Description : This function is similar to ReadJavaU2, but the integer
//    being extracted is a 32-bit value instead of 16.
//    The side effect is therefore doubled:  the pointer advances by 4 bytes.
//
unsigned long
CJavaClassFile::ReadJavaU4(string::const_iterator& bufferPointer)
{
  unsigned long netlong = 0;
  ::memcpy(&netlong, bufferPointer, 4);
  bufferPointer += 4;
  return ntohl(netlong);
}

//
//  Method name : WriteJavaU2
//  Description : This function writes out a two-byte integral value to the
//    provided output stream in the format specified by the Java VM spec.
//
void
CJavaClassFile::WriteJavaU2(ostream& to, unsigned short value)
{
  unsigned short netshort = htons(value);
  unsigned char* bytePointer = (unsigned char*)&netshort;
  to << *bytePointer++;
  to << *bytePointer;
}

//
//  Method name : WriteJavaU2
//  Description : This function writes out a two-byte integral value to the
//    provided string in the format specified by the Java VM spec.
//
void
CJavaClassFile::WriteJavaU2(string& to, unsigned short value)
{
  unsigned short netshort = htons(value);
  unsigned char* bytePointer = (unsigned char*)&netshort;
  to += *bytePointer++;
  to += *bytePointer;
}

//
//  Method name : WriteJavaU4
//  Description : This function is used to spit out a four-byte integer value
//    to a stream in the network format specified by the Java VM spec.
//
void
CJavaClassFile::WriteJavaU4(ostream& to, unsigned long value)
{
  unsigned long netlong = htonl(value);
  unsigned char* bytePointer = (unsigned char*)&netlong;
  to << *bytePointer++;
  to << *bytePointer++;
  to << *bytePointer++;
  to << *bytePointer;
}

//
//  Method name : WriteJavaU4
//  Description : This function is used to spit out a four-byte integer value
//    to a string in the network format specified by the Java VM spec.
//
void
CJavaClassFile::WriteJavaU4(string& to, unsigned long value)
{
  unsigned long netlong = htonl(value);
  unsigned char* bytePointer = (unsigned char*)&netlong;
  to += *bytePointer++;
  to += *bytePointer++;
  to += *bytePointer++;
  to += *bytePointer;
}

//
//  Method name : LookupConstant
//  Description : If the provided index points to a valid index in this
//    class's constant pool, then a pointer to that constant is returned
//    for the user to access.  If the index is invalid, then 0 is returned.
//
const CJavaConstant*
CJavaClassFile::LookupConstant(JavaConstantIndex index) const
{
  return ((index < fConstants.size()) ?
	  fConstants[index] :
	  (const CJavaConstant*)0);
}

//
//  Method name : GetCompilerVersion
//  Description : Returns the version of the Java compiler that was used to
//     compile this file.
//
unsigned long
CJavaClassFile::GetCompilerVersion() const
{
  return fVersion;
}


//
//  Method name : DisassembleClass
//  Description : This method is used to get an 8-bit string containing a
//    human-readable dump of this Java class's information.  This is a
//    relatively slow and inefficient operation (lots of copying), and is
//    basically intended for debugging purposes.
//
void
CJavaClassFile::DisassembleClass(ostream& toStream) const
{
  toStream << "Class: " << ::UnicodeToUTF(fThisClassName);
  if (fDeprecated) {
    toStream << "  (Deprecated)";
  }
  toStream << endl;
  toStream << "Superclass: " << ::UnicodeToUTF(fSuperclassName) << endl;
  toStream << "Interfaces: { ";
  for (deque<unicode_string>::const_iterator i = fInterfaces.begin();
       i != fInterfaces.end(); i++) {
    toStream << ::UnicodeToUTF(*i) << " ";
  }
  toStream << "}" << endl;
  ::hex(toStream);
  toStream << "Source File: ";
  if (fSourceFile != 0) {
    toStream << ::UnicodeToUTF(fSourceFile->GetFileName()) << endl;
  } else {
    toStream << "(not provided)" << endl;
  }
  toStream << "Compiler Version: 0x" << fVersion << endl;
  toStream << "Access Flags: { ";
  fAccessFlags.Disassemble(toStream);
  toStream << "}" << endl;
  ::dec(toStream);
  if (fInnerClasses != 0) {
    fInnerClasses->Disassemble(toStream);
  }
  toStream << "Constant Pool:" << endl;
  int constantNumber = 1;
  ConstantPool::const_iterator constantIterator = fConstants.begin();
  if (constantIterator != fConstants.end()) {
    for (constantIterator++; constantIterator != fConstants.end();
	 constantIterator++, constantNumber++) {
      toStream.width(6);
      toStream << constantNumber << ": ";
      if (*constantIterator == 0) {
	toStream << "(null)";
      } else {
	(*constantIterator)->Disassemble(toStream);
      }
      toStream << endl;      
    }
  }
  toStream << "Fields:" << endl;
  for (FieldTable::const_iterator fieldIterator = fFields.begin();
       fieldIterator != fFields.end(); ++fieldIterator) {
    toStream << "   ";
    assert((*fieldIterator).second != 0);
    (*fieldIterator).second->Disassemble(toStream);
    toStream << endl;
  }
  toStream << "Methods:" << endl;
  for (MethodTable::const_iterator methodIterator = fMethods.begin();
       methodIterator != fMethods.end(); ++methodIterator) {
    toStream << "   ";
    assert((*methodIterator).second != 0);
    (*methodIterator).second->Disassemble(toStream);
    toStream << endl << endl;
  }
}

//
//  Method name : Compile
//  Description : This method is used to output this class file to the stream
//    in the format specified by the Java VM Class specification.
//
void
CJavaClassFile::Compile(ostream& toStream)
{
  WriteJavaU4(toStream, 0xCAFEBABE);
  WriteJavaU4(toStream, fVersion);
  // aaaaargh!  they want the constant pool at the beginning and i haven't
  // finished making it yet.  Have to use a temporary buffer for the rest...
  string buffer;
  WriteJavaU2(buffer, fAccessFlags.GetJavaFlags() | ACC_SUPER);
  WriteJavaU2(buffer, AddClassConstant(fThisClassName));
  if (fThisClassName == ::StringToUnicode("java/lang/Object")) {
    WriteJavaU2(buffer, 0);
  } else {
    WriteJavaU2(buffer, AddClassConstant(fSuperclassName));
  }
  WriteJavaU2(buffer, fInterfaces.size());
  for (deque<unicode_string>::iterator i = fInterfaces.begin();
       i != fInterfaces.end(); ++i) {
    WriteJavaU2(buffer, AddClassConstant(*i));
  }
  WriteJavaU2(buffer, fFields.size());
  for (FieldTable::iterator i = fFields.begin(); i != fFields.end(); ++i) {
    buffer += (*i).second->Compile(*this);
  }
  WriteJavaU2(buffer, fMethods.size());
  for (MethodTable::iterator i = fMethods.begin(); i != fMethods.end(); ++i) {
    buffer += (*i).second->Compile(*this);
  }
  unsigned short attributeCount = 0;
  attributeCount += (fSourceFile != 0) ? 1 : 0;
  attributeCount += fDeprecated ? 1 : 0;
  attributeCount += (fInnerClasses != 0) ? 1 : 0;
  WriteJavaU2(buffer, attributeCount);
  if (fSourceFile != 0) {
    WriteJavaU2(buffer, AddAscizConstant("SourceFile"));
    WriteJavaU4(buffer, 2);
    WriteJavaU2(buffer, AddAscizConstant(fSourceFile->GetFileName()));
  }
  if (fDeprecated) {
    WriteJavaU2(buffer, AddAscizConstant("Deprecated"));
    WriteJavaU4(buffer, 0);
  }
  if (fInnerClasses != 0) {
    buffer += fInnerClasses->Compile(*this);
  }
  WriteJavaU2(toStream, fConstants.size());
  for (ConstantPool::iterator i = fConstants.begin();
       i != fConstants.end(); ++i) {
    if (*i != 0) {
      (*i)->Compile(toStream);
    }
  }
  toStream << buffer;
}

//
//  Method name : GetPackageName
//  Description : Returns the package to which this class belongs.
//
unicode_string
CJavaClassFile::GetPackageName() const
{
  unicode_string::size_type lastSlash = fThisClassName.find_last_of('/');
  if (lastSlash == unicode_string::npos) {
    lastSlash = 0;
  }
  return unicode_string(fThisClassName, 0, lastSlash);
}

//
//  Method name : GetShortName
//  Description : This method is used to get the part of the class name after
//    the last package delimiter.  For example, if this class name is
//    java.foo.Blah, then this will return "Blah".  If this class name is
//    just "Blah", then it will return "Blah".
//
unicode_string
CJavaClassFile::GetShortName() const
{
  unicode_string shortName = fThisClassName;
  unicode_string::size_type lastSlash = fThisClassName.find_last_of('/');
  if (lastSlash != unicode_string::npos) {
    shortName.assign(fThisClassName, lastSlash + 1, unicode_string::npos);
  }  
  return shortName;
}

//
//  Method name : LookupFieldSignature
//  Description : This method is used to look up the field signature that
//    corresponds to the provided constant pool index in the current
//    constant pool.  If this operation is successful, the input parameters
//    is modified appropriately and true is returned.  Otherwise, it returns
//    false.
//
bool
CJavaClassFile::LookupFieldSignature(JavaConstantIndex index,
				     unicode_string& setClassName,
				     CJavaFieldSignature& setField) const
{
  const CJavaFieldConstant* field =
    DYNAMIC_CAST(CJavaFieldConstant, LookupConstant(index));
  bool success = field != 0;
  if (success) {
    const CJavaNameTypeConstant* nameType =
      DYNAMIC_CAST(CJavaNameTypeConstant,
		   LookupConstant(field->GetNameTypeIndex()));
    success = nameType != 0 && 
      LookupClassName(field->GetClassIndex(), setClassName);
    if (success) {
      const CJavaAscizConstant* name =
	DYNAMIC_CAST(CJavaAscizConstant,
		     LookupConstant(nameType->GetNameIndex()));
      const CJavaAscizConstant* signature =
	DYNAMIC_CAST(CJavaAscizConstant,
		     LookupConstant(nameType->GetSignatureIndex()));
      success = name != 0 && signature != 0;
      if (success) {
	success = setField.Initialize(name->GetUnicodeString(),
				      signature->GetUnicodeString());
      }
    }
  }
  return success;
}

//
//  Method name : LookupMethodSignature
//  Description : This method is used to look up the method signature that
//    corresponds to the provided constant pool index in the current constant
//    pool.  If this operation is successful, the provided 'set*' parameters
//    are modified to take on the appropriate method information and true is
//    returned.  Otherwise, this method returns false.
//
bool
CJavaClassFile::LookupMethodSignature(JavaConstantIndex index,
				      unicode_string& setClassName,
				      CJavaMethodSignature& setMethod) const
{
  const CJavaMethodConstant* method =
    DYNAMIC_CAST(CJavaMethodConstant, LookupConstant(index));
  bool success = method != 0;
  if (success) {
    const CJavaNameTypeConstant* nameType =
      DYNAMIC_CAST(CJavaNameTypeConstant,
		   LookupConstant(method->GetNameTypeIndex()));
    success = nameType != 0 &&
      LookupClassName(method->GetClassIndex(), setClassName);
    if (success) {
      const CJavaAscizConstant* name =
	DYNAMIC_CAST(CJavaAscizConstant,
		     LookupConstant(nameType->GetNameIndex()));
      const CJavaAscizConstant* signature =
	DYNAMIC_CAST(CJavaAscizConstant,
		     LookupConstant(nameType->GetSignatureIndex()));
      success = name != 0 && signature != 0;
      if (success) {
	success = setMethod.Initialize(name->GetUnicodeString(),
				       signature->GetUnicodeString());
      }      
    }
  }
  return success;
}

//
//  Method name : LookupInterfaceSignature
//  Description : This method is used to look up the interface signature that
//    corresponds to the provided constant pool index in the current constant
//    pool.  If this operation is successful, the provided 'set*' parameters
//    are modified to take on the appropriate method information and true is
//    returned.  Otherwise, this method returns false.
//
bool
CJavaClassFile::LookupInterfaceSignature(JavaConstantIndex index,
					 unicode_string& setClassName,
					 CJavaMethodSignature& setMethod) const
{
  const CJavaInterfaceConstant* method =
    DYNAMIC_CAST(CJavaInterfaceConstant, LookupConstant(index));
  bool success = method != 0;
  if (success) {
    const CJavaNameTypeConstant* nameType =
      DYNAMIC_CAST(CJavaNameTypeConstant,
		   LookupConstant(method->GetNameTypeIndex()));
    success = nameType != 0 &&
      LookupClassName(method->GetClassIndex(), setClassName);
    if (success) {
      const CJavaAscizConstant* name =
	DYNAMIC_CAST(CJavaAscizConstant,
		     LookupConstant(nameType->GetNameIndex()));
      const CJavaAscizConstant* signature =
	DYNAMIC_CAST(CJavaAscizConstant,
		     LookupConstant(nameType->GetSignatureIndex()));
      success = name != 0 && signature != 0;
      if (success) {
	success = setMethod.Initialize(name->GetUnicodeString(),
				       signature->GetUnicodeString());
      }      
    }
  }
  return success;
}

//
//  Method name : LookupClassName
//  Description : This method is used to lookup a class name from the constant
//    pool.  If the class is found, the 'setName' parameter is set to the name
//    of the class and true is returned.  Otherwise, false is returned.
//
bool
CJavaClassFile::LookupClassName(JavaConstantIndex index,
				unicode_string& setName) const
{
  const CJavaClassConstant* classConstant =
    DYNAMIC_CAST(CJavaClassConstant, LookupConstant(index));
  bool success = classConstant != 0;
  if (success) {
    const CJavaAscizConstant* className = DYNAMIC_CAST(CJavaAscizConstant,
				LookupConstant(classConstant->GetNameIndex()));
    if (success = className != 0) {
      setName = className->GetUnicodeString();
    }
  }
  return success;
}

//
//  Method name : LookupField
//  Description : If a field with the given name is found on this class, then
//    an alias to its field info structure is returned.  Otherwise, 0 is
//    returned.  Has to do this linear search since the field table
//    is keyed by the whole signature (name and type) instead of just
//    the name, for efficiency of the VM.
//
const CJavaFieldInfo*
CJavaClassFile::LookupField(const unicode_string& name) const
{
  for (FieldTable::const_iterator i = fFields.begin();
       i != fFields.end(); ++i) {
    if (name == (*i).first.GetFieldName()) {
      return (*i).second;
    }
  }
  return 0;
}

//
//  Method name : LookupField
//  Description : If a field with the given signature is found on this class,
//    an alias to its field info structure is returned.  Otherwise, 0 is
//    returned.
//
const CJavaFieldInfo*
CJavaClassFile::LookupField(const CJavaFieldSignature& signature) const
{
  FieldTable::const_iterator found = fFields.find(signature);
  return (found == fFields.end()) ? 0 : (*found).second;
}

//
//  Method name : LookupMethod
//  Description : If a method with the given signature is found on this class,
//    an alias to its method info structure is returned.  Otherwise, 0 is
//    returned.
//
const CJavaMethodInfo*
CJavaClassFile::LookupMethod(const CJavaMethodSignature& signature) const
{
  MethodTable::const_iterator found = fMethods.find(signature);
  return (found == fMethods.end()) ? 0 : (*found).second;
}

//
//  Method name : LookupMethodIgnoreReturn
//  Description : If a method with the same name and arguments as the provided
//    signature is found on this class, than an alias to its method info
//    structure is returned.  Otherwise, 0 is returned.  This is a linear
//    search, so is slower than LookupMethod()
//
const CJavaMethodInfo*
CJavaClassFile::LookupMethodIgnoreReturn(
			   const CJavaMethodSignature& signature) const
{
  MethodTable::const_iterator found = fMethods.begin();
  for (; found != fMethods.end(); ++found) {
    if ((*found).first.EqualsIgnoreReturn(signature)) {
      return (*found).second;
    }
  }
  return 0;
}

//
//  Method name : AddLongConstant
//  Description : Used during compilation to push a 64-bit integer onto
//    the constant pool.  Returns the index of the corresponding constant,
//    which may be a match to an existing constant or may be a new one added
//    to the constant pool.
//
JavaConstantIndex
CJavaClassFile::AddLongConstant(unsigned long long doubleLong)
{
  return InsertConstant(new CJavaLongConstant(doubleLong));
}

//
//  Method name : AddIntegerConstant
//  Description : Used during compilation to push a 32-bit integer into the
//    constant pool.  Returns the index of the corresponding constant,
//    which may be a match to an existing constant or may be a new one added
//    to the constant pool.
//
JavaConstantIndex
CJavaClassFile::AddIntegerConstant(unsigned long integerValue)
{
  return InsertConstant(new CJavaIntegerConstant(integerValue));
}

//
//  Method name : AddFloatConstant
//  Description : Used during compilation to push a 32-bit float into the
//    constant pool.  Returns the index of the corresponding constant,
//    which may be a match to an existing constant or may be a new one added
//    to the constant pool.
//
JavaConstantIndex
CJavaClassFile::AddFloatConstant(float value)
{
  return InsertConstant(new CJavaFloatConstant(value));
}

//
//  Method name : AddDoubleConstant
//  Description : Used during compilation to push a 64-bit float into the
//    constant pool.  Returns the index of the corresponding constant,
//    which may be a match to an existing constant or may be a new one added
//    to the constant pool.
//
JavaConstantIndex
CJavaClassFile::AddDoubleConstant(double value)
{
  return InsertConstant(new CJavaDoubleConstant(value));
}

//
//  Method name : AddAscizConstant
//  Description : Used during compilation to push a unicode string into the
//    constant pool.  Returns the index of the corresponding constant,
//    which may be a match to an existing constant or may be a new one added
//    to the constant pool.
//
JavaConstantIndex
CJavaClassFile::AddAscizConstant(const unicode_string& value)
{
  return InsertConstant(new CJavaAscizConstant(value));
}

//
//  Method name : AddAscizConstant
//  Description : Used during compilation to push a unicode string into the
//    constant pool.  Returns the index of the corresponding constant,
//    which may be a match to an existing constant or may be a new one added
//    to the constant pool.
//
JavaConstantIndex
CJavaClassFile::AddAscizConstant(const string& value)
{
  return InsertConstant(new CJavaAscizConstant(::StringToUnicode(value)));
}

//
//  Method name : AddStringConstant
//  Description : Used during compilation to push a string constant into the
//    constant pool.  Returns the index of the corresponding constant,
//    which may be a match to an existing constant or may be a new one added
//    to the constant pool.
//
JavaConstantIndex
CJavaClassFile::AddStringConstant(JavaConstantIndex ascizIndex)
{
  return InsertConstant(new CJavaStringConstant(ascizIndex));
}

//
//  Method name : AddClassConstant
//  Description : Used during compilation to push a class constant into the
//    constant pool.  Returns the index of the corresponding constant,
//    which may be a match to an existing constant or may be a new one added
//    to the constant pool.
//
JavaConstantIndex
CJavaClassFile::AddClassConstant(const unicode_string& name)
{
  JavaConstantIndex ascizIndex = AddAscizConstant(name);
  return InsertConstant(new CJavaClassConstant(ascizIndex));
}

//
//  Method name : AddClassConstant
//  Description : Used during compilation to push a class constant into the
//    constant pool.  Returns the index of the corresponding constant,
//    which may be a match to an existing constant or may be a new one added
//    to the constant pool.
//
JavaConstantIndex
CJavaClassFile::AddFieldConstant(const unicode_string& className,
				 const unicode_string& fieldName,
				 const CJavaTypeSignature& type)
{
  JavaConstantIndex classIndex = AddClassConstant(className);
  unicode_string typeString = ::UTFToUnicode(type.Compile());
  JavaConstantIndex nameTypeIndex = AddNameTypeConstant(fieldName, typeString);
  return InsertConstant(new CJavaFieldConstant(classIndex, nameTypeIndex));
}

//
//  Method name : AddMethodConstant
//  Description : Used during compilation to push a method constant into the
//    constant pool.  Returns the index of the corresponding constant,
//    which may be a match to an existing constant or may be a new one added
//    to the constant pool.
//
JavaConstantIndex
CJavaClassFile::AddMethodConstant(const unicode_string& className,
				  const CJavaMethodSignature& method)
{
  JavaConstantIndex classIndex = AddClassConstant(className);
  string typeString = method.CompileType();
  JavaConstantIndex nameTypeIndex =
    AddNameTypeConstant(method.GetName(), ::UTFToUnicode(typeString));
  return InsertConstant(new CJavaMethodConstant(classIndex, nameTypeIndex));
}

//
//  Method name : AddInterfaceConstant
//  Description : Used during compilation to push a interface constant into the
//    constant pool.  Returns the index of the corresponding constant,
//    which may be a match to an existing constant or may be a new one added
//    to the constant pool.
//
JavaConstantIndex
CJavaClassFile::AddInterfaceConstant(const unicode_string& className,
				     const CJavaMethodSignature& method)
{
  JavaConstantIndex classIndex = AddClassConstant(className);
  string typeString = method.CompileType();
  JavaConstantIndex nameTypeIndex =
    AddNameTypeConstant(method.GetName(), ::UTFToUnicode(typeString));
  return InsertConstant(new CJavaInterfaceConstant(classIndex, nameTypeIndex));
}

//
//  Method name : AddNameTypeConstant
//  Description : Used during compilation to push a class constant into the
//    constant pool.  Returns the index of the corresponding constant,
//    which may be a match to an existing constant or may be a new one added
//    to the constant pool.
//
JavaConstantIndex
CJavaClassFile::AddNameTypeConstant(const unicode_string& name,
				    const unicode_string& typeString)
{
  JavaConstantIndex nameIndex = AddAscizConstant(name);
  JavaConstantIndex typeIndex = AddAscizConstant(typeString);
  return InsertConstant(new CJavaNameTypeConstant(nameIndex, typeIndex));
}

//
//  Method name : InsertConstant
//  Description : Used internally by the JavaClassFile to insert a constant
//    into its constnat pool.  Returns the index where the constant ended
//    up.  If it can find a matching constant already in the pool, it deletes
//    the argument and returns the old one.
//
JavaConstantIndex
CJavaClassFile::InsertConstant(CJavaConstant* adoptConstant)
{
  JavaConstantIndex index = 1;
  ConstantPool::iterator i = fConstants.begin();
  assert(i != fConstants.end() && *i == 0);
  for (++i; i != fConstants.end(); ++i, ++index) {
    if (*i != 0 && (*i)->operator==(*adoptConstant)) {
      delete adoptConstant;
      return index;
    }
  }
  fConstants.push_back(adoptConstant);
  if (adoptConstant->GetWidth() == 2) {
    class CJavaConstant* nullConstant = 0;
    fConstants.push_back(nullConstant);
  }
  return index;
}

//
//  Method name : CountInterfaces
//  Description : Returns the number of interfaces that this class directly
//    implements.
//
unsigned long
CJavaClassFile::CountInterfaces() const
{
  return fInterfaces.size();
}

//
//  Method name : GetInterfaceBegin
//  Description : Returns an iterator to the beginning of the list of
//    interface class names for this class.
//
CJavaClassFile::InterfaceList::const_iterator
CJavaClassFile::GetInterfaceBegin() const
{
  return fInterfaces.begin();
}

//
//  Method name : GetInterfaceEnd
//  Description : Returns an iterator to the end of the list of interface
//     class names for this class.
//
CJavaClassFile::InterfaceList::const_iterator
CJavaClassFile::GetInterfaceEnd() const
{
  return fInterfaces.end();
}

//
//  Method name : InsertMethod
//  Description : Inserts the provided method into this class's method table.
//
void
CJavaClassFile::InsertMethod(CJavaMethodInfo* method)
{
  assert(method != 0);
  method->AddReference();
  fMethods.insert(MethodTable::value_type(method->GetSignature(), method));
}

//
//  Method name : SameParents
//  Description : Returns true if this class has the same parents (extends and
//    implements) as the provided class.
//
bool
CJavaClassFile::SameParents(const CJavaClassFile& other) const
{
  bool same = CountInterfaces() == other.CountInterfaces() &&
    GetSuperclassName() == other.GetSuperclassName();
  if (same) {
    InterfaceList::const_iterator thisIterator = GetInterfaceBegin();
    InterfaceList::const_iterator otherIterator = other.GetInterfaceBegin();
    for (; same && !(thisIterator == GetInterfaceEnd());
	 ++thisIterator, ++otherIterator) {
      same = *thisIterator == *otherIterator;
    }
  }
  return same;
}

//
//  Method name : IsDeprecated
//  Description : Returns true if this class is deprecated from use.
//
bool
CJavaClassFile::IsDeprecated() const
{
  return fDeprecated;
}

//
//  Method name : GetSourceFile
//  Description : Returns the source file that this was compiled from, or 0
//    if no source information is available.
//
CJavaSourceFileAttribute*
CJavaClassFile::GetSourceFile() const
{
  return fSourceFile;
}

//
//  Method name : AddInnerClass
//  Description : Specifies the information for an inner class that is
//    contained within this class.
//
void
CJavaClassFile::AddInnerClass(const unicode_string& syntheticName,
			      const unicode_string& innerName,
			      const CJavaAccessFlags& access)
{
  AddInnerClass(syntheticName, innerName, access, fThisClassName);
}

//
//  Method name : AddInnerClass
//  Description : Specifies the information for an inner class that is
//    contained within a named outer class.
//
void
CJavaClassFile::AddInnerClass(const unicode_string& syntheticName,
			      const unicode_string& innerName,
			      const CJavaAccessFlags& access,
			      const unicode_string& outerName)
{
  if (fInnerClasses == 0) {
    fInnerClasses = new CInnerClassesTable();
  }
  fInnerClasses->AddInnerClass(syntheticName, innerName,
			       access, outerName);
}

//
//  Method name : IsInner
//  Description : Returns true if this class is a non-static inner class
//    within some other class.  This affects construction and other aspects
//    of the class.
//
bool
CJavaClassFile::IsInner() const
{
  if (fAccessFlags.fStatic == 0 && fInnerClasses != 0) {
    CInnerClassesTable::InnerClassTable::const_iterator i =
      fInnerClasses->InnerClassesBegin();
    CInnerClassesTable::InnerClassTable::const_iterator end =
      fInnerClasses->InnerClassesEnd();
    for (; !(i == end); ++i) {
      if ((*i)->fSyntheticName == fThisClassName &&
	  (*i)->fAccessFlags.fStatic == 0) {
	return true;
      }
    }
  }
  return false;
}

//
//  Method name : GetOuterName
//  Description : If this is an inner class, this will return the class name
//    of its outer file.  If it is not, it will return an empty string.
//
unicode_string
CJavaClassFile::GetOuterName() const
{
  unicode_string result;
  if (fInnerClasses != 0) {
    CInnerClassesTable::InnerClassTable::const_iterator i =
      fInnerClasses->InnerClassesBegin();
    CInnerClassesTable::InnerClassTable::const_iterator end =
      fInnerClasses->InnerClassesEnd();
    for (; !(i == end); ++i) {
      if ((*i)->fSyntheticName == fThisClassName) {
	// && (*i)->fAccessFlags.fStatic == 0) {
	result = (*i)->fOuterName;
	break;
      }
    }
  }
  return result;
}

//
//  Method name : FindInnerClass
//  Description : Looks for an inner class in this class that matches the
//    provided short name.  This will only return non-private inner classes,
//    which could be visible from other classes.  Returns true if a match
//    is found, and assigns the full sythesized class name to the match
//    argument.
//
bool
CJavaClassFile::FindInnerClass(const unicode_string& name,
			       unicode_string& match) const
{
  if (fInnerClasses != 0) {
    const CInnerClassesTable::InnerClassInfo* result =
      fInnerClasses->FindNonPrivate(name);
    if (result != 0) {
      match = result->fSyntheticName;
      return true;
    }
  }
  return false;
}
