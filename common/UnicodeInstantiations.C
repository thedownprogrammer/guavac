// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: UnicodeInstantiations.C,v 1.3 1996/01/22 05:37:05 geppetto Exp $

// 16-bit character string instantiations
#include <deque>

#include "unicode_string.h"
#include <std/bastring.cc>
#define USREP __bsrep<unicode_char, string_char_traits<unicode_char> >
#define USTR basic_string <unicode_char, string_char_traits <unicode_char> >
template class USREP;
template class USTR;
USREP USTR::nilRep = { 0, 0, 1 };
template USTR operator+ (const USTR&, const USTR&);
template bool operator==(const USTR&, const USTR&);

template class deque<string>;
template bool operator!=(deque<string>::iterator const &,
			 deque<string>::iterator const &);
template bool operator!=(deque<string>::const_iterator const &,
			 deque<string>::const_iterator const &);
template class deque<unicode_string>;
template bool operator!=(deque<unicode_string>::iterator const &,
			 deque<unicode_string>::iterator const &);
template bool operator!=(deque<unicode_string>::const_iterator const &,
			 deque<unicode_string>::const_iterator const &);
