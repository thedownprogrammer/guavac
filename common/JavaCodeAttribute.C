// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: JavaCodeAttribute.C,v 1.8 1997/03/02 22:54:43 geppetto Exp $
#include "JavaCodeAttribute.h"
#include "JavaClassFile.h"

//
// This array stores global information about the Java VM bytecodes.
// Currently, this includes the name of the bytecode and the number of
// additional bytes it uses as operands.
// If the operand count is less than 0, that means that the width of the
// operator is variable and special care must be taken.
//
const CJavaCodeAttribute::OpcodeInfo
CJavaCodeAttribute::gOpcodeSpecs[] = {
  {"nop", 0},
  {"aconst_null", 0},
  {"iconst_m1", 0},
  {"iconst_0", 0},
  {"iconst_1", 0},
  {"iconst_2", 0},
  {"iconst_3", 0},
  {"iconst_4", 0},
  {"iconst_5", 0},
  {"lconst_0", 0},
  {"lconst_1", 0},
  {"fconst_0", 0},
  {"fconst_1", 0},
  {"fconst_2", 0},
  {"dconst_0", 0},
  {"dconst_1", 0},
  {"bipush", 1},
  {"sipush", 2},
  {"ldc", 1},
  {"ldc_w", 2},
  {"ldc2_w", 2},
  {"iload", 1},
  {"lload", 1},
  {"fload", 1},
  {"dload", 1},
  {"aload", 1},
  {"iload_0", 0},
  {"iload_1", 0},
  {"iload_2", 0},
  {"iload_3", 0},
  {"lload_0", 0},
  {"lload_1", 0},
  {"lload_2", 0},
  {"lload_3", 0},
  {"fload_0", 0},
  {"fload_1", 0},
  {"fload_2", 0},
  {"fload_3", 0},
  {"dload_0", 0},
  {"dload_1", 0},
  {"dload_2", 0},
  {"dload_3", 0},
  {"aload_0", 0},
  {"aload_1", 0},
  {"aload_2", 0},
  {"aload_3", 0},
  {"iaload", 0},
  {"laload", 0},
  {"faload", 0},
  {"daload", 0},
  {"aaload", 0},
  {"baload", 0},
  {"caload", 0},
  {"saload", 0},
  {"istore", 1},
  {"lstore", 1},
  {"fstore", 1},
  {"dstore", 1},
  {"astore", 1},
  {"istore_0", 0},
  {"istore_1", 0},
  {"istore_2", 0},
  {"istore_3", 0},
  {"lstore_0", 0},
  {"lstore_1", 0},
  {"lstore_2", 0},
  {"lstore_3", 0},
  {"fstore_0", 0},
  {"fstore_1", 0},
  {"fstore_2", 0},
  {"fstore_3", 0},
  {"dstore_0", 0},
  {"dstore_1", 0},
  {"dstore_2", 0},
  {"dstore_3", 0},
  {"astore_0", 0},
  {"astore_1", 0},
  {"astore_2", 0},
  {"astore_3", 0},
  {"iastore", 0},
  {"lastore", 0},
  {"fastore", 0},
  {"dastore", 0},
  {"aastore", 0},
  {"bastore", 0},
  {"castore", 0},
  {"sastore", 0},
  {"pop", 0},
  {"pop2", 0},
  {"dup", 0},
  {"dup_x1", 0},
  {"dup_x2", 0},
  {"dup2", 0},
  {"dup2_x1", 0},
  {"dup2_x2", 0},
  {"swap", 0},
  {"iadd", 0},
  {"ladd", 0},
  {"fadd", 0},
  {"dadd", 0},
  {"isub", 0},
  {"lsub", 0},
  {"fsub", 0},
  {"dsub", 0},
  {"imul", 0},
  {"lmul", 0},
  {"fmul", 0},
  {"dmul", 0},
  {"idiv", 0},
  {"ldiv", 0},
  {"fdiv", 0},
  {"ddiv", 0},
  {"irem", 0},
  {"lrem", 0},
  {"frem", 0},
  {"drem", 0},
  {"ineg", 0},
  {"lneg", 0},
  {"fneg", 0},
  {"dneg", 0},
  {"ishl", 0},
  {"lshl", 0},
  {"ishr", 0},
  {"lshr", 0},
  {"iushr", 0},
  {"lushr", 0},
  {"iand", 0},
  {"land", 0},
  {"ior", 0},
  {"lor", 0},
  {"ixor", 0},
  {"lxor", 0},
  {"iinc", 2},  // special behavior after 'wide' instruction
  {"i2l", 0},
  {"i2f", 0},
  {"i2d", 0},
  {"l2i", 0},
  {"l2f", 0},
  {"l2d", 0},
  {"f2i", 0},
  {"f2l", 0},
  {"f2d", 0},
  {"d2i", 0},
  {"d2l", 0},
  {"d2f", 0},
  {"int2byte", 0},
  {"int2char", 0},
  {"int2short", 0},
  {"lcmp", 0},
  {"fcmpl", 0},
  {"fcmpg", 0},
  {"dcmpl", 0},
  {"dcmpg", 0},
  {"ifeq", 2},
  {"ifne", 2},
  {"iflt", 2},
  {"ifge", 2},
  {"ifgt", 2},
  {"ifle", 2},
  {"if_icmpeq", 2},
  {"if_icmpne", 2},
  {"if_icmplt", 2},
  {"if_icmpge", 2},
  {"if_icmpgt", 2},
  {"if_icmple", 2},
  {"if_acmpeq", 2},
  {"if_acmpne", 2},
  {"goto", 2},
  {"jsr", 2},
  {"ret", 1},
  {"tableswitch", -1},
  {"lookupswitch", -1},
  {"ireturn", 0},
  {"lreturn", 0},
  {"freturn", 0},
  {"dreturn", 0},
  {"areturn", 0},
  {"return", 0},
  {"getstatic", 2},
  {"putstatic", 2},
  {"getfield", 2},
  {"putfield", 2},
  {"invokevirtual", 2},
  {"invokenonvirtual", 2},
  {"invokestatic", 2},
  {"invokeinterface", 4},
  {"<<<unused>>>", -1},
  {"new", 2},
  {"newarray", 1},
  {"anewarray", 2},
  {"arraylength", 0},
  {"athrow", 0},
  {"checkcast", 2},
  {"instanceof", 2},
  {"monitorenter", 0},
  {"monitorexit", 0},
  {"wide", 1},
  {"multianewarray", 3},
  {"ifnull", 2},
  {"ifnonnull", 2},
  {"goto_w", 4},
  {"jsr_w", 4},
  {"breakpoint", 0},
  {"<<<unused>>>", -1},
  {"<<<unused>>>", -1},
  {"<<<unused>>>", -1},
  {"<<<unused>>>", -1},
  {"<<<unused>>>", -1},
  {"<<<unused>>>", -1},
  {"ret_w", 2}
};

//
//  Method name : CJavaCodeAttribute
//  Description : Constructor.
//
CJavaCodeAttribute::CJavaCodeAttribute()
  : fLineNumberTable(0),
    fLocalVariableTable(0)
{
}

//
//  Method name : ~CJavaCodeAttribute
//  Description : Destructor.
//
CJavaCodeAttribute::~CJavaCodeAttribute()
{
  delete fLineNumberTable;
  delete fLocalVariableTable;
}

//
//  Method name : ParseBuffer
//  Description : Reads in bytes from the current buffer position to try
//    to assemble a java code attribute.  If it is successful, the newly
//    created attribute is returned, otherwise it returns 0.
//    This starts after the 'attributeLength' field in the class file entry.
//
CJavaCodeAttribute*
CJavaCodeAttribute::ParseBuffer(string::const_iterator& buffer,
				const CJavaClassFile& classFile)
{
  CJavaCodeAttribute* result = new CJavaCodeAttribute;
  unsigned long codeLength;
  if (classFile.GetCompilerVersion() == 0x2002d) {
    // compiled by hotjava version of javac -- probably obsolete.
    result->fMaxStack = *buffer++;
    result->fMaxLocals = *buffer++;
    codeLength = CJavaClassFile::ReadJavaU2(buffer);
  } else {
    result->fMaxStack = CJavaClassFile::ReadJavaU2(buffer);
    result->fMaxLocals = CJavaClassFile::ReadJavaU2(buffer);
    codeLength = CJavaClassFile::ReadJavaU4(buffer);
  }
  result->fCode.append(buffer, codeLength);
  buffer += codeLength;
  unsigned short exceptionTableLength = CJavaClassFile::ReadJavaU2(buffer);
  result->fExceptionTable.reserve(exceptionTableLength);
  while (exceptionTableLength-- > 0) {
    ExceptionInfo entry;
    entry.fStartPC = CJavaClassFile::ReadJavaU2(buffer);
    entry.fEndPC = CJavaClassFile::ReadJavaU2(buffer);
    entry.fHandlerPC = CJavaClassFile::ReadJavaU2(buffer);
    entry.fCatchType = CJavaClassFile::ReadJavaU2(buffer);
    result->fExceptionTable.push_back(entry);
  }
  unsigned short attributeCount = CJavaClassFile::ReadJavaU2(buffer);
  while (attributeCount-- > 0) {
    CJavaAttribute* attribute =
      CJavaAttribute::ParseBuffer(buffer, classFile);
    if (attribute != 0) {
      CJavaLineNumberTable* lineNumberTable =
	DYNAMIC_CAST(CJavaLineNumberTable, attribute);
      CJavaLocalVariableTable* localVariableTable =
	DYNAMIC_CAST(CJavaLocalVariableTable, attribute);
      if (lineNumberTable != 0) {
	delete result->fLineNumberTable;
	result->fLineNumberTable = lineNumberTable;
      } else if (localVariableTable != 0) {
	delete result->fLocalVariableTable;
	result->fLocalVariableTable = localVariableTable;
      } else {
	delete attribute;
      }
    }
  }
  return result;
}

//
//  Method name : Disassemble
//  Description : Dumps out a human-readable description of this class to
//    the provided stream.  For debugging purposes.
//
void
CJavaCodeAttribute::Disassemble(ostream& toStream) const
{
  toStream << endl;
  toStream << "      Max Stack Depth: " << fMaxStack << endl;
  toStream << "      Local Variables: " << fMaxLocals;
  for (string::const_iterator i = fCode.begin(); i != fCode.end(); ) {
    toStream << endl;
    toStream.fill(' ');
    ::dec(toStream);
    toStream.width(10);
    unsigned short opcodeNumber = (0xff & *i);
    unsigned long instruction = i - fCode.begin();
    ++i;
    toStream << instruction << ": ";
    PrintOpcodeName((Opcode)opcodeNumber, toStream);
    toStream << " ";
    switch (opcodeNumber) {
    case tableswitch:
      while (((i - fCode.begin()) % 4) != 0) {
	++i;
      }
      {
	unsigned long def = CJavaClassFile::ReadJavaU4(i) + instruction;
	long low = CJavaClassFile::ReadJavaU4(i);
	long high = CJavaClassFile::ReadJavaU4(i);
	toStream << low << " to " << high << ": default = " << def;
	for (long index = low; index <= high; index++) {
	  toStream << endl;
	  toStream.width(20);
	  toStream << index << ": "
	    << CJavaClassFile::ReadJavaU4(i) + instruction;
	}
      }
      break;
    case lookupswitch:
      while (((i - fCode.begin()) % 4) != 0) {
	++i;
      }
      {
	unsigned long def = CJavaClassFile::ReadJavaU4(i) + instruction;
	unsigned long pairs = CJavaClassFile::ReadJavaU4(i);
	toStream << pairs << ": default = " << def;
	while (pairs-- > 0) {
	  long match = CJavaClassFile::ReadJavaU4(i);
	  unsigned long offset = CJavaClassFile::ReadJavaU4(i) + instruction;
	  toStream << endl;
	  toStream.width(20);
	  toStream << match << ": " << offset;
	}
      }
      break;
    case sipush:
      toStream << (short)CJavaClassFile::ReadJavaU2(i);
      break;
    case bipush:
      toStream << (int)*(i++);
      break;
    case iinc:
      toStream << " vindex: " << (unsigned long)*(i++);
      toStream << "  by: " << (int)*(i++);
      break;
    case multianewarray:
      toStream << " index: " << CJavaClassFile::ReadJavaU2(i);
      toStream << "  dimensions: " << (unsigned long)*(i++);
      break;
    default:
      int operandCount = gOpcodeSpecs[opcodeNumber].fOperands;
      assert(operandCount >= 0);
      switch (operandCount) {
      case 1:
	toStream << (unsigned long)*(i++);
	break;
      case 2:
	toStream << CJavaClassFile::ReadJavaU2(i);
	break;
      case 4:
	toStream << CJavaClassFile::ReadJavaU4(i);
	break;
      }
    }
    if (fLineNumberTable != 0) {
      ::dec(toStream);
      // yes, this is fairly innefficient, but it's not used often enough
      // to bother with more.
      CJavaLineNumberTable::LineVector::const_iterator line =
	fLineNumberTable->GetLinesBegin();
      for (; !(line == fLineNumberTable->GetLinesEnd()); ++line) {
	if ((*line).fStartPC == instruction) {
	  toStream << "   (source line " << (*line).fLineNumber << ")";
	  break;
	}
      }
      ::hex(toStream);
    }
  }
  toStream.fill(' ');
  ::dec(toStream);
  if (fExceptionTable.size() != 0) {
    toStream << endl << "  Exception table:" << endl
      << "    from   to  target  type";
    for (vector<ExceptionInfo>::const_iterator i = fExceptionTable.begin();
	 i != fExceptionTable.end(); ++i) {
      toStream << endl;
      toStream.width(6);
      toStream << (*i).fStartPC;
      toStream.width(6);
      toStream << (*i).fEndPC;
      toStream.width(6);
      toStream << (*i).fHandlerPC;
      toStream.width(6);
      toStream << (*i).fCatchType;
    }
  }
}

//
//  Method name : Compile
//  Description : Dumps out this attribute as a sequence of bytes in the format
//    specified by the Java VM spec.  Uses the 'inClass' parameter to add
//    entries to the constant table, if needed.
//
string
CJavaCodeAttribute::Compile(CJavaClassFile& inClass) const
{
  string buffer;
  CJavaClassFile::WriteJavaU2(buffer, inClass.AddAscizConstant("Code"));
  string buffer2;
  CJavaClassFile::WriteJavaU2(buffer2, fMaxStack);
  CJavaClassFile::WriteJavaU2(buffer2, fMaxLocals);
  CJavaClassFile::WriteJavaU4(buffer2, fCode.size());
  buffer2 += fCode;
  CJavaClassFile::WriteJavaU2(buffer2, fExceptionTable.size());
  for (vector<ExceptionInfo>::const_iterator i = fExceptionTable.begin();
       i != fExceptionTable.end(); ++i) {
    CJavaClassFile::WriteJavaU2(buffer2, (*i).fStartPC);
    CJavaClassFile::WriteJavaU2(buffer2, (*i).fEndPC);
    CJavaClassFile::WriteJavaU2(buffer2, (*i).fHandlerPC);
    CJavaClassFile::WriteJavaU2(buffer2, (*i).fCatchType);
  }
  unsigned short attributeCount = (fLineNumberTable != 0 ? 1 : 0);
  attributeCount += (fLocalVariableTable != 0 ? 1 : 0);
  CJavaClassFile::WriteJavaU2(buffer2, attributeCount);
  if (fLineNumberTable != 0) {
    buffer2 += fLineNumberTable->Compile(inClass);
  }
  if (fLocalVariableTable != 0) {
    buffer2 += fLocalVariableTable->Compile(inClass);
  }
  CJavaClassFile::WriteJavaU4(buffer, buffer2.size());
  buffer += buffer2;
  return buffer;
}

//
//  Method name : ExceptionsBegin
//  Description : This method returns a const iterator to the beginning of the
//    list of exceptions in this method's exeption table.
//
CJavaCodeAttribute::ExceptionTable::const_iterator
CJavaCodeAttribute::ExceptionsBegin() const
{
  return fExceptionTable.begin();
}

//
//  Method name : ExceptionsEnd
//  Description : This method returns a const iterator to the end of the list
//    of exceptions in this method's exception table.
//
CJavaCodeAttribute::ExceptionTable::const_iterator
CJavaCodeAttribute::ExceptionsEnd() const
{
  return fExceptionTable.end();
}

//
//  Method name : ExceptionsBegin
//  Description : This method returns a const iterator to the beginning of the
//    list of exceptions in this method's exeption table.
//
CJavaCodeAttribute::ExceptionTable::iterator
CJavaCodeAttribute::ExceptionsBegin()
{
  return fExceptionTable.begin();
}

//
//  Method name : ExceptionsEnd
//  Description : This method returns a const iterator to the end of the list
//    of exceptions in this method's exception table.
//
CJavaCodeAttribute::ExceptionTable::iterator
CJavaCodeAttribute::ExceptionsEnd()
{
  return fExceptionTable.end();
}

//
//  Method name : SetLineNumbers
//  Description : This method specifies the line number table that should be
//    used with this code.  The provided value is adopted and any previous
//    value is deleted.
//
void
CJavaCodeAttribute::SetLineNumbers(CJavaLineNumberTable* adoptTable)
{
  delete fLineNumberTable;
  fLineNumberTable = adoptTable;
}

//
//  Method name : ClosestLineNumber
//  Description : This method is used to find the source line number that is
//    closest to the provided instruction index.  If this information is
//    available, the appropriate line number is returned, otherwise 0 is
//    returned.
//
unsigned long
CJavaCodeAttribute::ClosestLineNumber(unsigned long toProgramCounter) const
{
  return (fLineNumberTable == 0) ? 0 :
    fLineNumberTable->ClosestLineNumber(toProgramCounter);
}

//
//  Method name : CJavaLineNumberTable
//  Description : Constructs a line number table from the provided vector
//    of lines.
//
CJavaLineNumberTable::CJavaLineNumberTable(const LineVector& lineInfo)
  : fTable(lineInfo)
{
}

//
//  Method name : ~CJavaLineNumberTable
//  Description : Destructor.
//
CJavaLineNumberTable::~CJavaLineNumberTable()
{
}

//
//  Method name : ParseBuffer
//  Description : Tries to interpret the input buffer as a java line number
//    table attribute, as documented in the virtual machine spec.  If
//    successful, it returns the new line number table, otherwise it returns 0.
//
CJavaLineNumberTable*
CJavaLineNumberTable::ParseBuffer(string::const_iterator& buffer)
{
  CJavaLineNumberTable* result = new CJavaLineNumberTable;
  unsigned short tableSize = CJavaClassFile::ReadJavaU2(buffer);
  result->fTable.reserve(tableSize);
  while (tableSize-- > 0) {
    LineInfo entry;
    entry.fStartPC = CJavaClassFile::ReadJavaU2(buffer);
    entry.fLineNumber = CJavaClassFile::ReadJavaU2(buffer);
    result->fTable.push_back(entry);
  }
  return result;
}

//
//  Method name : Compile
//  Description : Dumps out this attribute as a sequence of bytes in the format
//    specified by the Java VM spec.  Uses the 'inClass' parameter to add
//    entries to the constant table, if needed.
//
string
CJavaLineNumberTable::Compile(CJavaClassFile& inClass) const
{
  string buffer;
  CJavaClassFile::WriteJavaU2(buffer,
			      inClass.AddAscizConstant("LineNumberTable"));
  CJavaClassFile::WriteJavaU4(buffer, 2 + 4 * fTable.size());
  CJavaClassFile::WriteJavaU2(buffer, fTable.size());
  for (vector<LineInfo>::const_iterator i = fTable.begin();
       i != fTable.end(); ++i) {
    CJavaClassFile::WriteJavaU2(buffer, (*i).fStartPC);
    CJavaClassFile::WriteJavaU2(buffer, (*i).fLineNumber);
  }
  return buffer;
}

//
//  Method name : ClosestLineNumber
//  Description : This method is used to find the source line number that is
//    closest to the provided instruction index.  If this information is
//    available, the appropriate line number is returned, otherwise 0 is
//    returned.
//
unsigned long
CJavaLineNumberTable::ClosestLineNumber(unsigned long toProgramCounter) const
{
  unsigned long result = 0;
  for (LineVector::const_iterator i = fTable.begin();
       !(i == fTable.end()); ++i) {
    if ((*i).fStartPC <= toProgramCounter) {
      result = (*i).fLineNumber;
    } else {
      break;
    }
  }
  return result;
}

//
//  Method name : ~CJavaLocalVariableTable
//  Description : Destructor.
//
CJavaLocalVariableTable::~CJavaLocalVariableTable()
{
}

//
//  Method name : ParseBuffer
//  Description : Tries to interpret the input buffer as a java line number
//    table attribute, as documented in the virtual machine spec.  If
//    successful, it returns the new line number table, otherwise it returns 0.
//
CJavaLocalVariableTable*
CJavaLocalVariableTable::ParseBuffer(string::const_iterator& buffer)
{
  CJavaLocalVariableTable* result = new CJavaLocalVariableTable;
  unsigned short tableSize = CJavaClassFile::ReadJavaU2(buffer);
  result->fTable.reserve(tableSize);
  while (tableSize-- > 0) {
    VariableInfo entry;
    entry.fStartPC = CJavaClassFile::ReadJavaU2(buffer);
    entry.fLength = CJavaClassFile::ReadJavaU2(buffer);
    entry.fNameIndex = CJavaClassFile::ReadJavaU2(buffer);
    entry.fSignatureIndex = CJavaClassFile::ReadJavaU2(buffer);
    entry.fSlot = CJavaClassFile::ReadJavaU2(buffer);
    result->fTable.push_back(entry);
  }
  return result;
}

//
//  Method name : Compile
//  Description : Dumps out this attribute as a sequence of bytes in the format
//    specified by the Java VM spec.  Uses the 'inClass' parameter to add
//    entries to the constant table, if needed.
//
string
CJavaLocalVariableTable::Compile(CJavaClassFile& inClass) const
{
  string buffer;
  CJavaClassFile::WriteJavaU2(buffer,
			      inClass.AddAscizConstant("LocalVariableTable"));
  CJavaClassFile::WriteJavaU4(buffer, 2 + 10 * fTable.size());
  CJavaClassFile::WriteJavaU2(buffer, fTable.size());
  for (vector<VariableInfo>::const_iterator i = fTable.begin();
       i != fTable.end(); ++i) {
    CJavaClassFile::WriteJavaU2(buffer, (*i).fStartPC);
    CJavaClassFile::WriteJavaU2(buffer, (*i).fLength);
    CJavaClassFile::WriteJavaU2(buffer, (*i).fNameIndex);
    CJavaClassFile::WriteJavaU2(buffer, (*i).fSignatureIndex);
    CJavaClassFile::WriteJavaU2(buffer, (*i).fSlot);
  }
  return buffer;
}

//
//  Method name : AddLocalVariable
//  Description : This method sticks a new local variable into this method,
//    with the size of the variable given as an argument, and then returns
//    the index of the new value.
//
unsigned short
CJavaCodeAttribute::AddLocalVariable(unsigned short width)
{
  unsigned short result = fMaxLocals;
  fMaxLocals += width;
  return fMaxLocals;
}

//
//  Method name : AddExceptionHandler
//  Description : The Java VM deals with exceptions by marking the critically
//    monitored regions in the code using the ExceptionInfo structure.  This
//    method is used to stick one of these exception-handling region markers
//    into the code attribute.
//
void
CJavaCodeAttribute::AddExceptionHandler(const ExceptionInfo& handlerInfo)
{
  fExceptionTable.push_back(handlerInfo);
}

//
//  Method name : RemoveExceptionHandler
//  Description : Removes the specified exception handler from this code.
//
void
CJavaCodeAttribute::RemoveExceptionHandler(
				  const ExceptionTable::iterator& exception)
{
  fExceptionTable.erase(exception);
}

//
//  Method name : PrintOpcodeName
//  Description : Takes the string equivalent of the provided opcode and
//    dumps it to the provided stream.
//
void
CJavaCodeAttribute::PrintOpcodeName(Opcode opcode, ostream& toStream)
{
  toStream << gOpcodeSpecs[opcode].fName;
}

//
//  Method name : GetOpcodeArguments
//  Description : Returns a count of the number of additional bytes that are
//    used by this instruction in the byte stream.  This call will return
//    an invalid value for tableswitch and lookupswitch since these have a
//    variable number of arguments.
//
short
CJavaCodeAttribute::GetOpcodeArguments(Opcode opcode)
{
  return gOpcodeSpecs[opcode].fOperands;
}

//
//  Method name : AppendToCode
//  Description : Appends the provided value onto the end of this code
//    segment in Java's intermediate format.
//
void
CJavaCodeAttribute::AppendToCode(unsigned short value)
{
  fCode += (unsigned char)(value >> 8);
  fCode += (unsigned char)(value & 0xff);
}

//
//  Method name : AppendToCode
//  Description : Appends the provided value onto the end of this code
//    segment in Java's intermediate format.
//
void
CJavaCodeAttribute::AppendToCode(unsigned long value)
{
  CJavaClassFile::WriteJavaU4(fCode, value);
}
