// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: DisassembleMain.C,v 1.4 1996/01/08 03:03:38 geppetto Exp $
#include "JavaClassFile.h"
#include <string>
#include <iostream.h>
#include <fstream.h>

int
main(int argc, char** argv) {
  for (int count=1; count < argc; count++) {
    ifstream infile;
    infile.open(argv[count]);
    if (infile.is_open() == 0) {
      cerr << "File " << argv[count] << " can't be opened\n";
    } else {
      CJavaClassFile* file = CJavaClassFile::ParseFromStream(infile);
      cout << ((file != 0) ? "success" : "failure") << endl;
      if (file != 0) {
	file->DisassembleClass(cout);
      }
      delete file;
    }
  }
}

