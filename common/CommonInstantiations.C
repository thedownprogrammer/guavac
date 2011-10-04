// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: CommonInstantiations.C,v 1.5 1997/11/10 00:47:24 geppetto Exp $

#include <vector>
#include <deque>
#include <algorithm>
#include "JavaAttribute.h"
#include "JavaCodeAttribute.h"
#include "JavaTypeSignature.h"
#include "JavaFieldInfo.h"
#include "JavaFieldSignature.h"
#include "InnerClassesTable.h"
class CJavaConstant;
class CJavaMethodInfo;

template deque<string>::const_iterator
  find(deque<string>::const_iterator, deque<string>::const_iterator,
       const string&);
template class vector<CJavaConstant*>;
template class vector<CJavaCodeAttribute::ExceptionInfo>;
template class vector<CJavaLineNumberTable::LineInfo>;
template class vector<CInnerClassesTable::InnerClassInfo*>;
template class vector<CJavaLocalVariableTable::VariableInfo>;
template class deque<CJavaTypeSignature>;
template bool operator!=(deque<CJavaTypeSignature>::iterator const &,
			 deque<CJavaTypeSignature>::iterator const &);
template bool operator!=(deque<CJavaTypeSignature>::const_iterator const &,
			 deque<CJavaTypeSignature>::const_iterator const &);

template class map<CJavaFieldSignature, const CJavaFieldInfo*, less<CJavaFieldSignature> >;
template class rb_tree<CJavaFieldSignature, pair<CJavaFieldSignature const, CJavaFieldInfo const *>, select1st<pair<CJavaFieldSignature const, CJavaFieldInfo const *>, CJavaFieldSignature>, less<CJavaFieldSignature> >;
template class less<CJavaFieldSignature>;
template class pair<CJavaFieldSignature const, CJavaFieldInfo const *>;

#define FIELDTREE rb_tree<CJavaFieldSignature, pair<CJavaFieldSignature const, CJavaFieldInfo const *>, select1st<pair<CJavaFieldSignature const, CJavaFieldInfo const *>, CJavaFieldSignature>, less<CJavaFieldSignature> >
template bool operator!=(FIELDTREE::iterator const &,
			 FIELDTREE::iterator const &);
template bool operator!=(FIELDTREE::const_iterator const &,
			 FIELDTREE::const_iterator const &);

typedef map<CJavaFieldSignature, const CJavaFieldInfo*, less<CJavaFieldSignature> > JavaFieldMap;

template class map<CJavaMethodSignature, const CJavaMethodInfo*, less<CJavaMethodSignature> >;
typedef map<CJavaMethodSignature, const CJavaMethodInfo*, less<CJavaMethodSignature> > JavaMethodMap;
template class rb_tree<CJavaMethodSignature, pair<CJavaMethodSignature const, CJavaMethodInfo const *>, select1st<pair<CJavaMethodSignature const, CJavaMethodInfo const *>, CJavaMethodSignature>, less<CJavaMethodSignature> >;
template class pair<CJavaMethodSignature const, CJavaMethodInfo const *>;
template class less<CJavaMethodSignature>;
template bool operator!=(JavaMethodMap::iterator const &,
			 JavaMethodMap::iterator const &);
template bool operator!=(JavaMethodMap::const_iterator const &,
			 JavaMethodMap::const_iterator const &);
