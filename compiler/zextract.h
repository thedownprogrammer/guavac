/* $Id: zextract.h,v 1.2 1996/07/12 20:43:40 geppetto Exp $ */
/* Definitions for using a zipped' archive.

Copyright (c) 1996 Cygnus Support

See the file "zextract.license.terms" for information on usage and
redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES.

Written by Per Bothner <bothner@cygnus.com>, February 1996.
*/

struct ZipFile {
  int fd;
  long size;
  long count;
  long dir_size;
  long reference_count;  /* Added by David Engberg, June 1996. */
  char *central_directory;
};

typedef struct ZipFile ZipFile;

struct ZipDirectory {
  int direntry_size;
  int filename_offset;
  long size; /* length of file */
  long filestart;  /* start of file in archive */
  long filename_length;
  /* char mid_padding[...]; */
  /* char filename[filename_length]; */
  /* char end_padding[...]; */
};

typedef struct ZipDirectory ZipDirectory;

#define ZIPDIR_FILENAME(ZIPD) ((char*)(ZIPD)+(ZIPD)->filename_offset)
#define ZIPDIR_NEXT(ZIPD) \
   ((ZipDirectory*)((char*)(ZIPD)+(ZIPD)->direntry_size))


int read_zip_archive(ZipFile*);
