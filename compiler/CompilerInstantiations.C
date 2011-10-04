// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: CompilerInstantiations.C,v 1.5 1997/11/10 00:48:06 geppetto Exp $

#include <map>
#include <algorithm>
#include "unicode_string.h"
#include "VariableDeclaration.h"
#include "Statement.h"
#include "LocalVariableRecord.h"

template class map<unicode_string, const CJavaClassFile*, less<unicode_string> >;
typedef map<unicode_string, const CJavaClassFile*, less<unicode_string> >
      ClassMap;
template class pair<const unicode_string, const CJavaClassFile*>;
template class less<unicode_string>;
template ClassMap::const_reference ClassMap::iterator::operator*() const;
template bool ClassMap::iterator::operator==(ClassMap::iterator const&) const;
template ClassMap::reference ClassMap::const_iterator::operator*() const;
template bool operator!=(ClassMap::rep_type::iterator const &,
			 ClassMap::rep_type::iterator const &);
template bool operator!=(ClassMap::rep_type::const_iterator const &,
			 ClassMap::rep_type::const_iterator const &);

template class pair<const CJavaClassFile*, const CJavaMethodInfo*>;

template class map<unicode_string, CLocalVariableRecord,
                   less<unicode_string> >;
typedef map<unicode_string, CLocalVariableRecord,
            less<unicode_string> > LVarMap;
template LVarMap::reference LVarMap::iterator::operator*() const;
template LVarMap::const_reference LVarMap::const_iterator::operator*() const;
template bool operator!=(LVarMap::rep_type::iterator const &,
			 LVarMap::rep_type::iterator const &);
template bool operator!=(LVarMap::rep_type::const_iterator const &,
			 LVarMap::rep_type::const_iterator const &);

template bool operator!=(CJavaTypeSignature const &,
			 CJavaTypeSignature const &);
template bool max(const unsigned short&, const unsigned short&);
// template bool max(const unsigned long&, const unsigned long&);
