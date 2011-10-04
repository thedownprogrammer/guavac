// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: FilePath.h,v 1.4 1997/06/13 18:18:49 geppetto Exp $
#ifndef _FilePath_h
#define _FilePath_h
#pragma interface

#include <string>
#include <deque>

//
//  Class name : CFilePath
//  Description : Represents a Unix file path
//
class CFilePath {
public:
  CFilePath(const string& path);
  CFilePath(const CFilePath& source);
  ~CFilePath();
  CFilePath& operator=(const CFilePath& source);
  bool operator==(const CFilePath& other) const;

  bool Exists() const;
  bool IsFile() const;
  bool IsDirectory() const;
  bool NoUpDirectory() const;
  bool IsWritable() const;
  bool IsReadable() const;
  bool IsExecutable() const;
  bool ContainsLinks() const;

  int ListDirectoryContents(deque<string>& listToFill) const;

  bool MakeDirectory() const;

  string GetString() const;
  void SetFromString(const string& path);
private:
  string fPathString;
};

#endif
