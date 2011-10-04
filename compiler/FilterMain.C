// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: FilterMain.C,v 1.1 1995/12/15 10:57:59 geppetto Exp $
#include "JavaClassFile.h"
#include <string>
#include <iostream.h>
#include <fstream.h>

int main() {
  CJavaClassFile* file = CJavaClassFile::ParseFromStream(cin, true);
  if (file == 0) {
    cerr << "Unable to handle input file" << endl;
    return 1;
  } else {
    file->Compile(cout);
    delete file;
    return 0;
  }
}
