// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: CommandLine.h,v 1.1 1996/01/08 03:02:32 geppetto Exp $
#ifndef _CommandLine_h
#define _CommandLine_h
#pragma interface

#include <deque>
#include "string.h"

//
//  Class name : CCommandLine
//  Description : Represents a command line which keeps a command and a
//     number of arguments, kept in order.
//
class CCommandLine {
public:
  CCommandLine();
  CCommandLine(const CCommandLine& source);
  ~CCommandLine();

  string GetCommand() const;
  void SetCommand(const string& command);
  string GetLineAsString() const;
  typedef deque<string> ArgumentListType;
  typedef deque<string>::const_iterator ArgumentIteratorType;
  ArgumentIteratorType GetArgumentBegin() const;
  ArgumentIteratorType GetArgumentEnd() const;
  int GetArgumentCount() const;
  bool HasArgument(const string& name) const;
  ArgumentIteratorType ArgumentPosition(const string& name) const;
  
  bool ParseCommandString(const string& commandString);
  bool ParseArgumentVector(int argc, char const** argv);
  void AddArgument(const string& argument);

private:
  string fCommand;
  ArgumentListType fArgumentList;
};

#endif
