// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: CompilerInstantiations3.C,v 1.5 1997/11/10 00:48:06 geppetto Exp $

#include "NonlocalBranch.h"
#include "JavaMethodSignature.h"
#include "JavaDirectory.h"
#include "Expression.h"
#include <deque>
#include <list>
#include <vector>
class CIntermediateClass;
class CIntermediateFunction;
class CVariableDeclaration;
class CStatement;
class CJavaClassFile;

template class deque<CIntermediateClass*>;
template bool operator!=(deque<CIntermediateClass*>::iterator const &,
			 deque<CIntermediateClass*>::iterator const &);
template bool operator!=(deque<CIntermediateClass*>::const_iterator const &,
			 deque<CIntermediateClass*>::const_iterator const &);

template class deque<CIntermediateFunction*>;
template bool operator!=(deque<CIntermediateFunction*>::iterator const &,
			 deque<CIntermediateFunction*>::iterator const &);
template bool operator!=(deque<CIntermediateFunction*>::const_iterator const &,
			 deque<CIntermediateFunction*>::const_iterator const&);
template class deque<CVariableDeclaration*>;
template bool operator!=(deque<CVariableDeclaration*>::iterator const &,
			 deque<CVariableDeclaration*>::iterator const &);
template bool operator!=(deque<CVariableDeclaration*>::const_iterator const &,
			 deque<CVariableDeclaration*>::const_iterator const &);
template class deque<CStatement*>;
template bool operator!=(deque<CStatement*>::iterator const &,
			 deque<CStatement*>::iterator const &);
template bool operator!=(deque<CStatement*>::const_iterator const &,
			 deque<CStatement*>::const_iterator const &);
template class vector<CJavaClassFile*>;

template class list<CNonlocalBranch>;
template class list<CJavaMethodSignature>;
template class list<CJavaDirectory>;
template class list<COuterLocalExpression*>;
