// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: unicode_string.h,v 1.3 1996/03/09 20:08:37 geppetto Exp $
#ifndef _unicode_string_h
#define _unicode_string_h
#pragma interface

#include "string.h"

typedef unsigned short unicode_char;

// I changed this from a typedef to a subclass to try to reduce template
// symbol length.  This may prevent brain-dead linkers from exploding.
// typedef basic_string <unicode_char, string_char_traits <unicode_char> > unicode_string;
class unicode_string :
    public basic_string <unicode_char, string_char_traits <unicode_char> > {
public:
  typedef basic_string <unicode_char, string_char_traits <unicode_char> >
     inherited;
  
  explicit unicode_string() : inherited() {}
  explicit unicode_string(const inherited& str) : inherited(str) {}
  unicode_string(const unicode_string& str) : inherited(str) {}
  unicode_string(const unicode_string& str, size_type pos, size_type n = npos)
    : inherited(str, pos, n) {}
  unicode_string(const value_type* s, size_type n) : inherited(s, n) {}
  unicode_string(size_type n, value_type c) : inherited(n, c) {}
  
  unicode_string& operator=(const unicode_string& str)
  { inherited::operator=(str); return *this; }

  unicode_string& operator+=(const unicode_string& rhs)
  { inherited::operator+=(rhs); return *this; }
  
  unicode_string& operator+=(unicode_char c)
  { inherited::operator+=(c); return *this; }
  
  int compare(const unicode_string& str, size_type pos = 0,
    size_type n = npos) const { return inherited::compare(str, pos, npos); }
  
};
inline bool operator==(const unicode_string& lhs,
                       const unicode_string& rhs)
{
  return lhs.compare(rhs) == 0;
}
inline bool operator<(const unicode_string& lhs,
                      const unicode_string& rhs)
{
  return lhs.compare(rhs) < 0;
}
inline unicode_string operator+(const unicode_string& lhs,
				const unicode_string& rhs)
{
  unicode_string str(lhs);
  str.append(rhs);
  return str;
}


unicode_string UTFToUnicode(const string&);
string UnicodeToUTF(const unicode_string&);

unicode_string StringToUnicode(const string&);
unicode_string StringToUnicode(const char*);
unicode_string StringToUnicode(const char*, size_t length);
string UnicodeToString(const unicode_string&);

unsigned long Hash(const unicode_string&);
unsigned long Hash(unicode_string::const_iterator start,
		   unicode_string::const_iterator end);

#endif
