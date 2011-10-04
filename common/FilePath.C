// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: FilePath.C,v 1.8 1997/11/10 00:47:24 geppetto Exp $
#pragma implementation
#include "FilePath.h"
#include <stream.h>
extern "C" {
  #include <sys/types.h>
  #include <sys/stat.h>
  #include <unistd.h>

  #ifdef NeXT
    #include <sys/dir.h>
    #define S_IWUSR S_IWRITE
    #define S_IWGRP (S_IWRITE>>1)
    #define S_IWOTH (S_IWRITE>>2)
    #define S_IRUSR S_IREAD
    #define S_IRGRP (S_IREAD>>1)
    #define S_IROTH (S_IREAD>>2)
    #define S_IXUSR S_IEXEC
    #define S_IXGRP (S_IEXEC>>1)
    #define S_IXOTH (S_IEXEC>>2)
    #define S_ISREG(x) (x & S_IFREG)
    #define S_ISDIR(x) (x & S_IFDIR)
    #define S_ISLNK(x) (x & S_IFLNK)
    #define dirent direct
  #else
    #include <dirent.h>
  #endif

}

//
//  Method name : CFilePath
//  Description : Constructs a path out of a string representation.
//    Does no checking for validity, etc.
//
CFilePath::CFilePath(const string& path)
    : fPathString(path)
{
}

//
//  Method name : CFilePath
//  Description : Copy constructor.
//
CFilePath::CFilePath(const CFilePath& source)
    : fPathString(source.fPathString)
{
}

//
//  Method name : ~CFilePath
//  Description : Destructor.
//
CFilePath::~CFilePath()
{
}

//
//  Method name : operator=
//  Description : Assignment operator, sets this path to the same value
//     as another.
//
CFilePath&
CFilePath::operator=(const CFilePath& source)
{
  if (&source != this) {
    fPathString = source.fPathString;
  }
  return *this;
}

//
//  Method name : operator==
//  Description : Returns true if this file path is the same as the provided
//    file entity.  If one or both of these does not exist, this returns
//    false.
//
bool
CFilePath::operator==(const CFilePath& other) const
{
  struct stat thisStatus;
  int thisStatReturn = ::stat(fPathString.c_str(), &thisStatus);
  struct stat otherStatus;
  int otherStatReturn = ::stat(other.fPathString.c_str(), &otherStatus);
  return (thisStatReturn == 0) && (otherStatReturn == 0) &&
    thisStatus.st_ino == otherStatus.st_ino;
}

//
//  Method name : Exists
//  Description : Returns true if the provided path indicates a real
//    file system entity.
//
bool
CFilePath::Exists() const
{
  struct stat fileStatus;
  const char* filePath = fPathString.c_str();
  int statReturn = ::stat(filePath, &fileStatus);
  return (statReturn == 0);
}

//
//  Method name : IsFile
//  Description : Returns true if the path points to a regular file.
//
bool
CFilePath::IsFile() const
{
  bool result = Exists();
  if (result) {
    struct stat fileStatus;
    int statReturn = ::stat(fPathString.c_str(), &fileStatus);
    result = S_ISREG(fileStatus.st_mode);
  }
  return result;
}

//
//  Method name : IsDirectory
//  Description : Returns true if the path points to a directory.
//
bool
CFilePath::IsDirectory() const
{
  bool result = Exists();
  if (result) {
    struct stat fileStatus;
    int statReturn = ::stat(fPathString.c_str(), &fileStatus);
    result = S_ISDIR(fileStatus.st_mode);
  }
  return result;
}

//
//  Method name : NoUpDirectory
//  Description : Returns true if this path doesn't contain any
//    portions which move up the directory heirarchy (i.e. "..")
//
bool
CFilePath::NoUpDirectory() const
{
  return fPathString.find("..") == string::npos;
}

//
//  Method name : IsWritable
//  Description : Returns true if this process has permission
//    to write to the entity pointed to by this path.
//
bool
CFilePath::IsWritable() const
{
  bool result = Exists();
  if (result) {
    struct stat fileStatus;
    int statReturn = ::stat(fPathString.c_str(), &fileStatus);
    if (fileStatus.st_uid == getuid()) {
      result = (fileStatus.st_mode & S_IWUSR) != 0;
    } else if (fileStatus.st_gid == getgid()) {
      result = (fileStatus.st_mode & S_IWGRP) != 0;
    } else {
      result = (fileStatus.st_mode & S_IWOTH) != 0;
    }
  }
  return result;
}

//
//  Method name : IsReadable
//  Description : Returns true if the file system entity pointed to
//    by this path is readable by this process.
//
bool
CFilePath::IsReadable() const
{
  bool result = Exists();
  if (result) {
    struct stat fileStatus;
    int statReturn = ::stat(fPathString.c_str(), &fileStatus);
    if (fileStatus.st_uid == getuid()) {
      result = (fileStatus.st_mode & S_IRUSR) != 0;
    } else if (fileStatus.st_gid == getgid()) {
      result = (fileStatus.st_mode & S_IRGRP) != 0;
    } else {
      result = (fileStatus.st_mode & S_IROTH) != 0;
    }
  }
  return result;
}

//
//  Method name : IsExecutable
//  Description : Returns true if the file system entity pointed to
//    by this path is executable by this process.  This only indicates
//    the Unix 'execute' bit, which has different semantics when the
//    target is a directory.
//    I believe, but am not sure, that you must also have read
//    permission to actually execute a program or script.
//
bool
CFilePath::IsExecutable() const
{
  bool result = Exists();
  if (result) {
    struct stat fileStatus;
    int statReturn = ::stat(fPathString.c_str(), &fileStatus);
    if (fileStatus.st_uid == getuid()) {
      result = (fileStatus.st_mode & S_IXUSR) != 0;
    } else if (fileStatus.st_gid == getgid()) {
      result = (fileStatus.st_mode & S_IXGRP) != 0;
    } else {
      result = (fileStatus.st_mode & S_IXOTH) != 0;
    }
  }
  return result;
}

//
//  Method name : GetString
//  Description : Gets the path, as a string.
//
string
CFilePath::GetString() const
{
  return fPathString;
}

//
//  Method name : SetFromString
//  Description : Sets the path from the provided string.
//
void
CFilePath::SetFromString(const string& path)
{
  fPathString = path;
}

//
//  Method name : ContainsLinks
//  Description : Returns true if any part of the path is
//    a link (hard or symbolic)
//
bool
CFilePath::ContainsLinks() const
{
#if defined(__OS2__) || defined(__EMX__)
  return false; // OS/2 doesn't have links
#else
  bool hasLinks = false;
  if (Exists()) {
    string::size_type lastSlash = 0;
    string partialPath;
    while (!hasLinks && partialPath.size() != fPathString.size()) {
      lastSlash = fPathString.find("/", lastSlash + 1);
      if (lastSlash != string::npos) {
	partialPath = fPathString.substr(0, lastSlash);
      } else {
	partialPath = fPathString;
      }
      struct stat fileStatus;
      ::lstat(partialPath.c_str(), &fileStatus);
      hasLinks = S_ISLNK(fileStatus.st_mode);
    }
  }
  return hasLinks;
#endif
}


//
//  Method name : ListDirectoryContents
//  Description : If this file path is a valid readable directory, this
//    function takes the provided list and appends on the names of all of
//    the entities contained in this directory level, then returns a count
//    of the total inserted.
//    If this operation can't be completed, -1 is returned.
//
int
CFilePath::ListDirectoryContents(deque<string>& listToFill) const
{
  int result = -1;
  if (IsReadable() && IsDirectory()) {
    DIR* directory = ::opendir(fPathString.c_str());
    if (directory != 0) {
      struct dirent* entry;
      result = 0;
      while ((entry = ::readdir(directory)) != 0) {
	listToFill.push_back(string(entry->d_name));
	result++;
      }
      ::closedir(directory);
    }
  }
  return result;
}

//
//  Method name : MakeDirectory
//  Description : Tries to make a directory node in this path.  Returns true
//    if this was successful.
//
bool
CFilePath::MakeDirectory() const
{
  return ::mkdir(fPathString.c_str(), 0777) == 0;
}
