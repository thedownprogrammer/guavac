// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: CommandLine.C,v 1.1 1996/01/08 03:03:02 geppetto Exp $
#pragma implementation
#include "CommandLine.h"
#include <stream.h>
#include <cassert>
#include <algorithm>

//
//  Method name : CCommandLine
//  Description : Default constructor.  Makes an empty
//    command line.
//
CCommandLine::CCommandLine()
{
}

//
//  Method name : CCommandLine
//  Description : Copy constructor.
//
CCommandLine::CCommandLine(const CCommandLine& source)
    : fCommand(source.fCommand), fArgumentList(source.fArgumentList)
{
}

//
//  Method name : ~CCommandLine
//  Description : Destructor.
//
CCommandLine::~CCommandLine()
{
}

//
//  Method name : GetCommand
//  Description : Returns the command from the command line.
//
string
CCommandLine::GetCommand() const
{
  return fCommand;
}

//
//  Method name : SetCommand
//  Description : Sets the command to the provided string.
//
void
CCommandLine::SetCommand(const string& command)
{
  fCommand = command;
}

//
//  Method name : GetLineAsString
//  Description : Returns the command and its arguments as a single
//    string.  Arguments that contain spaces are surrounded by
//    quotes.
//
string
CCommandLine::GetLineAsString() const
{
  string result(GetCommand());
  ArgumentIteratorType iterator = GetArgumentBegin();
  for (; !(iterator == GetArgumentEnd()); iterator++) {
    result += " ";
    if ((*iterator).find(string(" ")) != string::npos) {
      result += '"';
      result += *iterator;
      result += '"';
    } else {
      result += *iterator;
    }
  }
  return result;
}

//
//  Method name : GetArgumentIterator
//  Description : Returns an iterator to allow someone to traverse the
//    arguments to this command, from the beginning.
//
CCommandLine::ArgumentIteratorType
CCommandLine::GetArgumentBegin() const
{
  return fArgumentList.begin();
}

//
//  Method name : GetArgumentEnd
//  Description : Returns an iterator to the last element in the list.
//
CCommandLine::ArgumentIteratorType
CCommandLine::GetArgumentEnd() const
{
  return fArgumentList.end();
}

//
//  Method name : ParseArgumentVector
//  Description : Takes an array of C-style arguments and puts them in
//    the argument vector, declaring the first argument to be the
//    command.
//
bool
CCommandLine::ParseArgumentVector(int argc, char const** argv)
{
  fArgumentList.erase(fArgumentList.begin(), fArgumentList.end());  
  bool result = argc > 0;
  if (result) {
    SetCommand(string(argv[0]));
    for (char const** i = &argv[1]; i != &argv[argc]; i++) {
       fArgumentList.insert(fArgumentList.end(), string(*i));
    }
  }
  return result;
}

//
//  Method name : GetArgumentCount
//  Description : Returns the number of arguments passed to this command
//
int
CCommandLine::GetArgumentCount() const
{
  return fArgumentList.size();
}

//
//  Method name : HasArgument
//  Description : Returns true if this commandline contains an argument
//      that matches the provided name.
//
bool
CCommandLine::HasArgument(const string& name) const
{
  return !(ArgumentPosition(name) == GetArgumentEnd());
}

//
//  Method name : ArgumentPosition
//  Description : Looks through the argument vector for the argument
//     named 'name'.  If it finds it, the iterator for that value is
//     returned, otherwise the iterator returned will be equal to
//     the result of GetArgumentEnd()
//
CCommandLine::ArgumentIteratorType
CCommandLine::ArgumentPosition(const string& name) const
{
  return ::find(GetArgumentBegin(), GetArgumentEnd(), name);
}

//
//  Method name : AddArgument
//  Description : Adds the provided string to this command as an
//    argument
//
void
CCommandLine::AddArgument(const string& argumentString)
{
  fArgumentList.insert(fArgumentList.end(), argumentString);  
}

