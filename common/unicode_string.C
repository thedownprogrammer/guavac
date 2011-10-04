// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: unicode_string.C,v 1.2 1996/03/09 20:08:37 geppetto Exp $
#pragma implementation
#include "unicode_string.h"

//
//  Function name : UTFToUnicode
//  Description : This global function is used to facilitate the translation
//    from Java bytecode encoded UTF strings to 16-bit Unicode strings.
//    It takes in a sequence of 8-bit characters and spits out a
//    unicode_string.
//
unicode_string
UTFToUnicode(const string& inString)
{
  unicode_string outString;
  string::const_iterator i = inString.begin();
  string::const_iterator end = inString.end();
  while (i != end) {
    string::value_type firstChar = *i++;
    if (firstChar & 0x80 &&
	(end - i) >= ((firstChar & 0x20) ? 2 : 1)) {
      string::value_type secondChar = *i++;
      unicode_char outChar;
      if (firstChar & 0x20) {
	outChar = ((0x0f & firstChar) << 12) |
	  ((0x3f & secondChar) << 6) | (0x3f & *i++);
      } else {
	outChar = ((0x1f & firstChar) << 6) | (0x3f & secondChar);
      }
      outString += outChar;
    } else {
      outString += (unicode_char)firstChar;
    }
  }
  return outString;
}

//
//  Function name : UnicodeToUTF
//  Description : This global function is the opposite of UTFToUnicode.  It
//    goes from a 16-bit unicode string to an 8-bit UTF string as documented
//    in the Java reference manual.
//
string
UnicodeToUTF(const unicode_string& inString)
{
  string outString;
  unicode_string::const_iterator i = inString.begin();
  for (; i != inString.end(); i++) {
    if (*i > 0x3ff) {
      outString += (string::value_type)(0xe0 | ((0xf000 & *i) >> 12));
      outString += (string::value_type)(0x80 | ((0xfc0 & *i) >> 6));
      outString += (string::value_type)(0x80 | (0x3f & *i));
    } else if (*i == 0 || *i > 0x7f) {
      outString += (string::value_type)(0xc0 | ((0x7c0 & *i) >> 6));
      outString += (string::value_type)(0x80 | (0x3f & *i));
    } else {
      outString += (string::value_type)*i;
    }
  }
  return outString;
}

//
//  Function name : StringToUnicode
//  Description : This global function takes an 8-bit string and returns the
//    equivalent unicode string.
//
unicode_string
StringToUnicode(const string& inString) {
  unicode_string outString;
  string::const_iterator i = inString.begin();
  while (i != inString.end()) {
    outString += (unicode_string::value_type)*i++;
  }
  return outString;
}

//
//  Function name : StringToUnicode
//  Description : This global function takes an 8-bit C-style string and
//    returns the equivalent unicode string.
//
unicode_string
StringToUnicode(const char* inString) {
  unicode_string outString;
  while (*inString != 0) {
    outString += (unicode_string::value_type)*inString++;
  }
  return outString;
}

//
//  Function name : StringToUnicode
//  Description : This global function takes a sequence of 8-bit characters
//    and their length and returns the equivalent unicode string.
//
unicode_string
StringToUnicode(const char* inString, size_t length) {
  unicode_string outString;
  for (int i=0 ; i < length; i++) {
    outString += (unicode_string::value_type)inString[i];
  }
  return outString;
}

//
//  Function name : UnicodeToString
//  Description : This global function takes a unicode string and hacks it
//    down to an 8-bit character string.  This is a destructive transformation,
//    so any charaters with ordinal value above 255 will be corrupted.
//    If this is a problem, you should use the non-destructive UnicodeToUTF
//    to encode in 8-bit strings.
//
string
UnicodeToString(const unicode_string& inString) {
  string outString;
  unicode_string::const_iterator i = inString.begin();
  for (; i != inString.end(); i++) {
    outString += (unicode_string::value_type)*i;
  }
  return outString;
}


//
//  Function name : Hash
//  Description : This global function allows a hash value to be generated
//    from a range of 16-bit characters.  This is done by multiplying each
//    character in the sequence by a constant and then summing them together,
//    tossing away overflow.
//
unsigned long
Hash(unicode_string::const_iterator start,
     unicode_string::const_iterator end) {
  unsigned long hash = 0;
  for (; start < end; start++) {
    hash += 0xfedc * (*start);
  }
  return hash;
}

//
//  Function name : Hash
//  Description : This global function takes a unicode string and tries to
//    provide a general-purpose 32-bit hash value from it using a relatively
//    basic hash function described above.
//
unsigned long
Hash(const unicode_string& s) {
  return ::Hash(s.begin(), s.end());
}
