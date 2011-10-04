// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: JavaDirectory.h,v 1.2 1996/07/12 20:43:40 geppetto Exp $
#ifndef _JavaDirectory_h
#define _JavaDirectory_h
#pragma interface

#include "unicode_string.h"
#include "FilePath.h"
extern "C" {
  #include "zextract.h"
}
class CJavaClassFile;

//
//  Class name : CJavaDirectory
//  Description : This class represents a conceptual directory which may hold
//    Java class files.  Since Java can use archived class files found in
//    a compressed ("zip") file, this entity may or may not correspond to
//    an actual directory on disk.
//
class CJavaDirectory {
public:
  CJavaDirectory(const string& baseDirectory);
  CJavaDirectory(const CJavaDirectory& source);
  CJavaDirectory(const CJavaDirectory& base, const string& relativePath);
  ~CJavaDirectory();
  CJavaDirectory& operator=(const CJavaDirectory& source);
  bool operator==(const CJavaDirectory& other) const;
  bool operator<(const CJavaDirectory& other) const;

  bool IsValid() const { return fIsValid; }
  bool InZipFile() const { return fZipFile != 0; }
  string GetRealDirectory() const;
  string GetPackage() const;

  CJavaClassFile* LoadClassFile(const unicode_string& className) const;

  CJavaDirectory();
private:

  CFilePath fBasePath;
  string fRelativePath;
  ZipFile* fZipFile;
  bool fIsValid;
};

#endif
