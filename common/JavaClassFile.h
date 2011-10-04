// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: JavaClassFile.h,v 1.11 1997/11/10 00:47:24 geppetto Exp $
#ifndef _JavaClassFile_h
#define _JavaClassFile_h
#pragma interface

#include "JavaConstant.h"
#include "JavaFieldInfo.h"
#include "JavaMethodInfo.h"
#include "JavaMethodSignature.h"
#include "JavaAccessFlags.h"
#include <vector>
#include <map>
class CJavaSourceFileAttribute;
class CCompiler;
class CInnerClassesTable;
class CJavaMethodInfo;
class ostream;
class istream;

typedef map<CJavaFieldSignature, const CJavaFieldInfo*, less<CJavaFieldSignature> > FieldTable;
typedef map<CJavaMethodSignature, const CJavaMethodInfo*, less<CJavaMethodSignature> > MethodTable;

//
//  Class name : CJavaClassFile
//  Description : This class tries to encapsulate the Java class file format
//    as specified in the Java virtual machine documentation.
//    As such, this isn't designed to be optimized for any specific usage
//    other than streaming to and from a java class file.
//
class CJavaClassFile {
public:
 virtual ~CJavaClassFile();
  
  static CJavaClassFile* ParseFromString(const string& fileBuffer,
					 bool interfaceOnly = false);
  static CJavaClassFile* ParseFromStream(istream& inStream,
					 bool interfaceOnly = false);
  static unsigned short ReadJavaU2(string::const_iterator& bufferPointer);
  static unsigned long ReadJavaU4(string::const_iterator& bufferPointer);
  static void WriteJavaU2(ostream& to, unsigned short);
  static void WriteJavaU4(ostream& to, unsigned long);
  static void WriteJavaU2(string& to, unsigned short);
  static void WriteJavaU4(string& to, unsigned long);
  
  CJavaAccessFlags GetAccessFlags() const { return fAccessFlags; }

  typedef vector<CJavaConstant*> ConstantPool;
  const CJavaConstant* LookupConstant(JavaConstantIndex index) const;
  unicode_string GetClassName() const { return fThisClassName; }
  unicode_string GetSuperclassName() const { return fSuperclassName; }
  typedef deque<unicode_string> InterfaceList;
  unsigned long CountInterfaces() const;
  InterfaceList::const_iterator GetInterfaceBegin() const;
  InterfaceList::const_iterator GetInterfaceEnd() const;
  bool SameParents(const CJavaClassFile& other) const;

  unicode_string GetShortName() const;
  unicode_string GetPackageName() const;
  unsigned long GetCompilerVersion() const;
  bool LookupFieldSignature(JavaConstantIndex index,
			    unicode_string& setClassName,
			    CJavaFieldSignature& setField) const;
  bool LookupMethodSignature(JavaConstantIndex index,
			     unicode_string& setClassName,
			     CJavaMethodSignature& setMethod) const;
  bool LookupInterfaceSignature(JavaConstantIndex index,
				unicode_string& setClassName,
				CJavaMethodSignature& setMethod) const;
  bool LookupClassName(JavaConstantIndex index, unicode_string& setName) const;

  const CJavaFieldInfo* LookupField(const unicode_string& name) const;
  const CJavaFieldInfo* LookupField(const CJavaFieldSignature& name) const;
  const CJavaMethodInfo*
                    LookupMethod(const CJavaMethodSignature& method) const;
  const CJavaMethodInfo*
           LookupMethodIgnoreReturn(const CJavaMethodSignature& method) const;
  bool IsDeprecated() const;

  void AddInnerClass(const unicode_string& syntheticName,
		     const unicode_string& innerName,
		     const CJavaAccessFlags& access);
  void AddInnerClass(const unicode_string& syntheticName,
		     const unicode_string& innerName,
		     const CJavaAccessFlags& access,
		     const unicode_string& outerName);
  bool IsInner() const;
  unicode_string GetOuterName() const;

  void DisassembleClass(ostream& toStream) const;
  void Compile(ostream& toStream);
  JavaConstantIndex AddLongConstant(unsigned long long doubleLong);
  JavaConstantIndex AddIntegerConstant(unsigned long integerValue);
  JavaConstantIndex AddFloatConstant(float value);
  JavaConstantIndex AddDoubleConstant(double value);
  JavaConstantIndex AddAscizConstant(const unicode_string& value);
  JavaConstantIndex AddAscizConstant(const string& value);
  JavaConstantIndex AddStringConstant(JavaConstantIndex ascizIndex);
  JavaConstantIndex AddClassConstant(const unicode_string& name);
  JavaConstantIndex AddFieldConstant(const unicode_string& className,
	     const unicode_string& fieldName, const CJavaTypeSignature& type);
  JavaConstantIndex AddMethodConstant(const unicode_string& className,
				      const CJavaMethodSignature& method);
  JavaConstantIndex AddInterfaceConstant(const unicode_string& className,
					 const CJavaMethodSignature& method);
  JavaConstantIndex AddNameTypeConstant(const unicode_string& name,
					const unicode_string& typeString);

  bool FindInnerClass(const unicode_string& className,
		      unicode_string& match) const;

  CJavaSourceFileAttribute* GetSourceFile() const;
protected:
  friend class CCompiler;
  CJavaClassFile();
  void InsertMethod(CJavaMethodInfo* methodInfo);
  JavaConstantIndex InsertConstant(CJavaConstant* adoptConstant);
private:
  unsigned long fVersion;
  ConstantPool fConstants;
  CJavaAccessFlags fAccessFlags;
  unicode_string fThisClassName;
  unicode_string fSuperclassName;
  InterfaceList fInterfaces;
  FieldTable fFields;
  MethodTable fMethods;
  bool fDeprecated;

  CJavaSourceFileAttribute* fSourceFile;
  CInnerClassesTable* fInnerClasses;
};

#endif

