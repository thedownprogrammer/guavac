// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: JavaDirectory.C,v 1.4 1997/11/10 00:48:06 geppetto Exp $
#pragma implementation
#include "JavaDirectory.h"
#include "JavaClassFile.h"
#include "config.h"
#include <fstream.h>
extern "C" {
  #include <sys/types.h>
  #include <sys/stat.h>
  #include <fcntl.h>
  #include <unistd.h>
  #include <stdio.h>
}

//
//  Method name : CJavaDirectory
//  Description : Default constructor.  Required for list<CJavaDirectory>.
//    Don't call directly.
//
CJavaDirectory::CJavaDirectory()
  : fBasePath(""),
    fZipFile(0),
    fIsValid(false)
{
}

//
//  Method name : CJavaDirectory
//  Description : Constructs a java directory corresponding to the provided
//    base directory name.  This can either be a real directory on disk or
//    a path to a zip file.
//
CJavaDirectory::CJavaDirectory(const string& baseDirectory)
  : fBasePath(baseDirectory),
    fZipFile(0),
    fIsValid(false)
{
  if (fBasePath.IsReadable() && fBasePath.IsDirectory()) {
    fIsValid = true;
  } else if (fBasePath.IsFile() && baseDirectory.length() > 4 &&
	     baseDirectory.compare(".zip", baseDirectory.length() - 4) == 0) {
    fZipFile = new ZipFile;
    fZipFile->reference_count = 1;
    fZipFile->fd = ::open(baseDirectory.c_str(), O_RDONLY);
    if (fZipFile->fd >= 0 && ::read_zip_archive(fZipFile) == 0) {
      fIsValid = true;
    } else {
      delete fZipFile;
      fZipFile = 0;
    }
  }
}

//
//  Method name : CJavaDirectory
//  Description : Copy constructor.
//
CJavaDirectory::CJavaDirectory(const CJavaDirectory& source)
  : fBasePath(source.fBasePath),
    fRelativePath(source.fRelativePath),
    fZipFile(source.fZipFile),
    fIsValid(source.fIsValid)
{
  if (fZipFile != 0) {
    fZipFile->reference_count++;
  }
}

//
//  Method name : CJavaDirectory
//  Description : Creates a relative java directory, indicating some package
//    directory that is relative to the provided base.
//
CJavaDirectory::CJavaDirectory(const CJavaDirectory& base,
			       const string& relativePath)
  : fBasePath(base.fBasePath),
    fRelativePath(base.fRelativePath + relativePath + "/"),
    fZipFile(base.fZipFile),
    fIsValid(base.fIsValid)
{
  if (fZipFile != 0) {
    fZipFile->reference_count++;
  }
  if (fIsValid) {
    if (fZipFile == 0) {
      CFilePath fullPath(fBasePath.GetString() + "/" + fRelativePath);
      fIsValid = fullPath.IsDirectory() && fullPath.IsReadable();
    } else {
      fIsValid = false;
      ZipDirectory* directory = (ZipDirectory*)fZipFile->central_directory;
      for (int i = 0; i < fZipFile->count;
	   ++i, directory = ZIPDIR_NEXT(directory)) {
	const char* zipFile = ZIPDIR_FILENAME(directory);
	if (fRelativePath.compare(zipFile, 0, fRelativePath.length()) == 0) {
	  fIsValid = true;
	  break;
	}
      }
    }
  }
}

//
//  Method name : ~CJavaDirectory
//  Description : Destructor
//
CJavaDirectory::~CJavaDirectory()
{
  if (fZipFile != 0 && --fZipFile->reference_count == 0) {
    ::close(fZipFile->fd);
    free(fZipFile->central_directory);
    delete fZipFile;
  }
}

//
//  Method name : operator=
//  Description : Assignment operator.
//
CJavaDirectory&
CJavaDirectory::operator=(const CJavaDirectory& source)
{
  if (&source != this) {
    fBasePath = source.fBasePath;
    fRelativePath = source.fRelativePath;
    fZipFile = source.fZipFile;
    fIsValid = source.fIsValid;
    if (fZipFile != 0) {
      fZipFile->reference_count++;
    }
  }
  return *this;
}

//
//  Method name : operator==
//  Description : Compares this directory against another, returns true if they
//    are identical.
//
bool
CJavaDirectory::operator==(const CJavaDirectory& other) const
{
  return fBasePath == other.fBasePath &&
    fRelativePath == other.fRelativePath &&
    fZipFile == other.fZipFile;
}

//
//  Method name : operator<
//  Description : This method is required to put CJavaDirectory in a list<>.
//    It doesn't make much sense by itself.
//
bool
CJavaDirectory::operator<(const CJavaDirectory& other) const
{
  return fBasePath.GetString() < other.fBasePath.GetString() ||
    fRelativePath < other.fRelativePath;
}

//
//  Method name : LoadClassFile
//  Description : This method attempts to load the java class file with the
//    provided name, relative to this java directory.  If this operation could
//    be completed, this returns the newly constructed class file object,
//    otherwise it returns 0.
//
CJavaClassFile*
CJavaDirectory::LoadClassFile(const unicode_string& className) const
{
  CJavaClassFile* result = 0;
  if (IsValid()) {
    string classFileName =
      fRelativePath + ::UnicodeToUTF(className) + ".class";
    if (fZipFile != 0) {
      ZipDirectory* directory = (ZipDirectory*)fZipFile->central_directory;
      for (int i = 0; i < fZipFile->count;
	   ++i, directory = ZIPDIR_NEXT(directory)) {
	const char* zipFileName = ZIPDIR_FILENAME(directory);
	if (classFileName == zipFileName) {
	  long fileOffset = directory->filestart;
	  long fileLength = directory->size;
	  if (::lseek(fZipFile->fd, fileOffset, SEEK_SET) >= 0) {
	    string buffer;
	    char bufferBytes[512];
	    while (fileLength > 0) {
	      long length = (512 > fileLength) ? fileLength : 512;
	      ::read(fZipFile->fd, bufferBytes, length);
	      buffer.append(bufferBytes, length);
	      fileLength -= length;
	    }
	    result = CJavaClassFile::ParseFromString(buffer);
	    break;
	  }
	}
      }
    } else {
      string fullPath = fBasePath.GetString() + "/" + classFileName;
      ifstream inFile;
      inFile.open(fullPath.c_str());
      if (inFile.is_open()) {
	result = CJavaClassFile::ParseFromStream(inFile);
	inFile.close();
	if (result == 0) {
	  cerr << "Warning:  Invalid class file " << fullPath << endl;
	}
      }
    }
  }
  return result;
}

//
//  Method name : GetRealDirectory
//  Description : This method will return the actual directory path represented
//    by this path.  This method assumes that this JavaDirectory is not part
//    of a zip file.
//
string
CJavaDirectory::GetRealDirectory() const
{
  assert(!InZipFile());
  return fBasePath.GetString() + "/" + fRelativePath;
}

//
//  Method name : GetPackage
//  Description : Returns the portion of this directory that represents the
//    package information.  This is relative to the base directory.
//
string
CJavaDirectory::GetPackage() const
{
  return fRelativePath;
}
