// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: CodeSequence.C,v 1.6 1997/03/02 22:53:41 geppetto Exp $
#pragma implementation
#include "CodeSequence.h"
#include <iostream.h>
#include <cassert>

// This is a conservative rule-of-thumb number to try to approximate how many
// bytes will be used by each logical instruction in a code sequence.
// Most instructions will be 1 or 3, but this may be offset by large switch
// tables, so I'm erring on the side of caution.
static const int kBytesPerInstruction = 4;

//
//  Method name : CCodeSequence
//  Description : Default constructor.
//
CCodeSequence::CCodeSequence()
  : fLastBasicBlock(0), fLastLine(0)
{
}

//
//  Method name : CCodeSequence
//  Description : Copy constructor.
//
CCodeSequence::CCodeSequence(const CCodeSequence& source)
  : fInstructions(source.fInstructions),
    fBasicBlockOffsets(source.fBasicBlockOffsets),
    fLastBasicBlock(0),
    fLineNumbers(source.fLineNumbers),
    fLineBeginnings(source.fLineBeginnings),
    fLastLine(0)
{
}

//
//  Method name : ~CCodeSequence
//  Description : Destructor
//
CCodeSequence::~CCodeSequence()
{
}

//
//  Method name : operator=
//  Description : Assignment operator.
//
CCodeSequence&
CCodeSequence::operator=(const CCodeSequence& source)
{
  if (&source != this) {
    fInstructions = source.fInstructions;
    fBasicBlockOffsets = source.fBasicBlockOffsets;
    fLastBasicBlock = source.fLastBasicBlock;
    fLineNumbers = source.fLineNumbers;
    fLineBeginnings = source.fLineBeginnings;
    fLastLine = source.fLastLine;
  }
  return *this;
}

//
//  Method name : GetInstructionsBegin
//  Description : Returns an iterator to the beginning of this instruction
//    sequence.
//
CCodeSequence::InstructionList::const_iterator
CCodeSequence::GetInstructionsBegin() const
{
  return fInstructions.begin();
}

//
//  Method name : GetInstructionsEnd
//  Description : Returns an iterator to the end of this instruction sequence.
//
CCodeSequence::InstructionList::const_iterator
CCodeSequence::GetInstructionsEnd() const
{
  return fInstructions.end();
}

//
//  Method name : Insert
//  Description : This method is used to push the provided value into the
//    instruction sequence at the given index.  If this index isn't the end
//    of the sequence, all subsequent elements are pushed ahead one space.
//
void
CCodeSequence::Insert(unsigned long index, const Instruction& value)
{
  unsigned long total = 0;
  for (OffsetList::iterator i = fBasicBlockOffsets.begin();
       !(i == fBasicBlockOffsets.end()); ++i) {
    total += *i;
    if (total > index) {
      ++(*i);
      break;
    }
  }
  total = 0;
  for (OffsetList::iterator i = fLineBeginnings.begin();
       !(i == fLineBeginnings.end()); ++i) {
    total += *i;
    if (total > index) {
      ++(*i);
      break;
    }
  }
  fInstructions.insert(fInstructions.begin() + index, value);
}

//
//  Method name : Remove
//  Description : Removes the instruction at the provided index from this code
//    sequence.  This is implemented for efficiency by replacing the offending
//    instruction with a no-op instruction.  This prevents a lot of expensive
//    copying, but makes pattern matching a little more tricky.
//    As a result, the indices of other elements of the instruction sequence
//    are not changed.
//
void
CCodeSequence::Remove(unsigned long index)
{
  assert(index < size());
  fInstructions[index].fOpcode = CJavaCodeAttribute::nop;
}
//
//  Method name : Remove
//  Description : Removes 'count' instructions starting with the instruction
//    at index 'index.'  This is implemented for efficiency by replacing the
//    offending instruction with a no-op instruction.  This prevents a lot of
//    expensive copying, but makes pattern matching a little more tricky.
//    As a result, the indices of other elements of the instruction sequence
//    are not changed.
//
void
CCodeSequence::Remove(unsigned long index, unsigned long count)
{
  for (unsigned long i = 0; i < count; ++i) {
    Remove(index + i);
  }
}

//
//  Method name : Append
//  Description : This method sticks the provided instruction onto the end of
//    this sequence.  If the provided lineNumber is not equal to 0, this
//    will be reflected in the debugging line number table.
//
void
CCodeSequence::Append(const Instruction& value, unsigned long lineNumber)
{
  if (lineNumber != 0) {
    if (fLineNumbers.empty() || fLineNumbers.back() != lineNumber) {
      fLineNumbers.push_back(lineNumber);
      fLineBeginnings.push_back(fInstructions.size() - fLastLine);
      fLastLine = fInstructions.size();
    }
  }
  fInstructions.push_back(value);
  CreateBlockIfConditional(value.fOpcode);
}

//
//  Method name : Append
//  Description : This method sticks the provided instruction onto the end of
//    this sequence.  If the provided lineNumber is not equal to 0, this
//    will be reflected in the debugging line number table.
//
void
CCodeSequence::Append(CJavaCodeAttribute::Opcode opcode,
		      unsigned long lineNumber)
{
  Instruction instruction;
  instruction.fOpcode = opcode;
  instruction.fArguments.u4 = 0;
  Append(instruction, lineNumber);
}

//
//  Method name : Append
//  Description : This method makes an instruction with the provided opcode
//    and arguments and sticks it onto the end of this sequence.
//    If the provided lineNumber is not equal to 0, this
//    will be reflected in the debugging line number table.
//
void
CCodeSequence::Append(CJavaCodeAttribute::Opcode opcode, unsigned long u4,
		      unsigned long lineNumber)
{
  Instruction instruction;
  instruction.fOpcode = opcode;
  instruction.fArguments.u4 = u4;
  Append(instruction, lineNumber);
}

//
//  Method name : CreateBlockLabel
//  Description : This method marks the current end of instructions as the
//    beginning of a new basic block.  In assembly terms, this is like putting
//    down a label which can be the target of future jumps.  All jumps to
//    blocks can be made safe, but absolute jumps may be thrown off by code
//    motion.  This call returns the block number which should be used as
//    the jump location.
//
unsigned long
CCodeSequence::CreateBlockLabel()
{
  unsigned long index = fBasicBlockOffsets.size();
  if (index != 0 && fInstructions.size() == fLastBasicBlock) {
    --index;   // just use the last one ... no new instructions.
  } else {
    fBasicBlockOffsets.push_back(fInstructions.size() - fLastBasicBlock);
    fLastBasicBlock = fInstructions.size();
  }
  return index;
}

//
//  Method name : CreateBlockIfConditional
//  Description : This method will start a new block at the current insertion
//    point if the provided instruction is is a conditional branch.
//
void
CCodeSequence::CreateBlockIfConditional(CJavaCodeAttribute::Opcode instruction)
{
  switch (instruction) {
  case CJavaCodeAttribute::ifeq:
  case CJavaCodeAttribute::ifne:
  case CJavaCodeAttribute::if_acmpeq:
  case CJavaCodeAttribute::if_acmpne:
  case CJavaCodeAttribute::ifnull:
  case CJavaCodeAttribute::ifnonnull:
  case CJavaCodeAttribute::if_icmpeq:
  case CJavaCodeAttribute::if_icmpne:
  case CJavaCodeAttribute::if_icmplt:
  case CJavaCodeAttribute::if_icmple:
  case CJavaCodeAttribute::if_icmpge:
  case CJavaCodeAttribute::if_icmpgt:
  case CJavaCodeAttribute::iflt:
  case CJavaCodeAttribute::ifle:
  case CJavaCodeAttribute::ifgt:
  case CJavaCodeAttribute::ifge:
    CreateBlockLabel();
  }
}

//
//  Method name : PeekNextBlockLabel
//  Description : Returns the index that will be used for the next block label
//    that is created by the 'CreateBlockLabel' call.
//
// unsigned long
// CCodeSequence::PeekNextBlockLabel() const
// {
//   return fBasicBlockOffsets.size();
// }

//
//  Method name : PrintShortDebug
//  Description : Disassembles this bytecode to the provided stream.
//
void
CCodeSequence::PrintShortDebug(ostream& toStream) const
{
  InstructionList::const_iterator instruction = fInstructions.begin();
  int labelCount = 0;
  for (OffsetList::const_iterator label = fBasicBlockOffsets.begin();
       !(label == fBasicBlockOffsets.end()); ++label) {
    for (int i = 0; i < *label; ++i) {
      PrintInstruction(*instruction++, toStream);
    }
    toStream << labelCount++ << ":" << endl;
  }
  for (; !(instruction == fInstructions.end()); ++instruction) {
    PrintInstruction(*instruction, toStream);
  }
}

//
//  Method name : PrintInstruction
//  Description : This method takes the provided instruction and dumps it
//    out on a line to the provided stream, in human-readable format.
//
void
CCodeSequence::PrintInstruction(const CCodeSequence::Instruction& instruction,
				ostream& toStream) const
{
  toStream << "     ";
  CJavaCodeAttribute::PrintOpcodeName(instruction.fOpcode, toStream);
  toStream << " ";
  ::hex(toStream);
  toStream << instruction.fArguments.u4;
  ::dec(toStream);
  toStream << endl;
}

//
//  Method name : EliminateDeadCode
//  Description : This method will eliminate unreachable instructions from the
//    code sequence.  Must be used before finalization.
//
void
CCodeSequence::EliminateDeadCode(CJavaCodeAttribute& intoCodeAttribute)
{
  bool* reachable = new bool[size()];
  for (int i = size(); i-- != 0; ) {
    reachable[i] = false;
  }
  MarkReachableFrom(0, reachable);
  CJavaCodeAttribute::ExceptionTable::iterator exceptionsBegin =
    intoCodeAttribute.ExceptionsBegin();
  CJavaCodeAttribute::ExceptionTable::iterator exceptionsEnd =
    intoCodeAttribute.ExceptionsEnd();
  bool foundReachableHandler;
  do {
    foundReachableHandler = false;
    CJavaCodeAttribute::ExceptionTable::iterator exception = exceptionsBegin;
    for (; !(exception == exceptionsEnd); ++exception) {
      unsigned long end = LabelToInstruction((*exception).fEndPC);
      if (!reachable[LabelToInstruction((*exception).fHandlerPC)]) {
	for (unsigned long i = LabelToInstruction((*exception).fStartPC);
	     i < end; ++i) {
	  if (reachable[i]) {
	    foundReachableHandler = true;
	    MarkReachableFrom(LabelToInstruction((*exception).fHandlerPC),
			      reachable);
	    break;
	  }
	}
      }
    }
  } while (foundReachableHandler);
  if (!(exceptionsBegin == exceptionsEnd)) {
    CJavaCodeAttribute::ExceptionTable::iterator exception = exceptionsEnd;
    do {
      --exception;
      if (!reachable[LabelToInstruction((*exception).fHandlerPC)]) {
	intoCodeAttribute.RemoveExceptionHandler(exception);
      }
    } while (!(exception == exceptionsBegin));
  }
  for (int i = size(); i-- != 0; ) {
    if (!reachable[i]) {
      Remove(i);
    }
  }
  delete [] reachable;
}

//
//  Method name : MarkReachableFrom
//  Description : This method will mark all of the instructions reachable
//    that are reachable from the instruction at the provided index.
//
void
CCodeSequence::MarkReachableFrom(unsigned long index, bool reachable[])
{
  for (; !reachable[index] && index < size(); ++index) {
    reachable[index] = true;
    unsigned long argument = fInstructions[index].fArguments.u4;
    switch (fInstructions[index].fOpcode) {
    case CJavaCodeAttribute::op_return:
    case CJavaCodeAttribute::ireturn:
    case CJavaCodeAttribute::lreturn:
    case CJavaCodeAttribute::freturn:
    case CJavaCodeAttribute::dreturn:
    case CJavaCodeAttribute::areturn:
    case CJavaCodeAttribute::athrow:
    case CJavaCodeAttribute::ret:
      return;
    case CJavaCodeAttribute::op_goto:
      MarkReachableFrom(LabelToInstruction(argument), reachable);
      return;
    case CJavaCodeAttribute::ifeq:
    case CJavaCodeAttribute::ifne:
    case CJavaCodeAttribute::iflt:
    case CJavaCodeAttribute::ifge:
    case CJavaCodeAttribute::ifgt:
    case CJavaCodeAttribute::ifle:
    case CJavaCodeAttribute::if_icmpeq:
    case CJavaCodeAttribute::if_icmpne:
    case CJavaCodeAttribute::if_icmplt:
    case CJavaCodeAttribute::if_icmpge:
    case CJavaCodeAttribute::if_icmpgt:
    case CJavaCodeAttribute::if_icmple:
    case CJavaCodeAttribute::if_acmpeq:
    case CJavaCodeAttribute::if_acmpne:
    case CJavaCodeAttribute::ifnull:
    case CJavaCodeAttribute::ifnonnull:
    case CJavaCodeAttribute::jsr:
      MarkReachableFrom(LabelToInstruction(argument), reachable);
      break;
    case CJavaCodeAttribute::tableswitch:
      reachable[++index] = true;
      MarkReachableFrom(LabelToInstruction(fInstructions[index].fArguments.u4),
			reachable);
      reachable[++index] = true;
      reachable[++index] = true;
      for (unsigned long key = 0; key < argument; ++key) {
	reachable[++index] = true;
	MarkReachableFrom(
	    LabelToInstruction(fInstructions[index].fArguments.u4), reachable);
      }
      return;
    case CJavaCodeAttribute::lookupswitch:
      reachable[++index] = true;
      MarkReachableFrom(LabelToInstruction(fInstructions[index].fArguments.u4),
			reachable);
      reachable[++index] = true;
      for (unsigned long key = fInstructions[index].fArguments.u4;
	   key > 0; --key) {
	reachable[++index] = true;
	reachable[++index] = true;
	MarkReachableFrom(
	    LabelToInstruction(fInstructions[index].fArguments.u4), reachable);
      }
      return;
    default:
      break;
    }
  }
}

//
//  Method name : Finalize
//  Description : This method is used after the code sequence is all created
//    and transformed and is ready to be dumped into the provided code
//    attribute.
//
void
CCodeSequence::Finalize(CJavaCodeAttribute& intoCodeAttribute)
{
  EliminateDeadCode(intoCodeAttribute);
  OffsetList labelOffsets;
  const unsigned long labelCount = fBasicBlockOffsets.size();
  labelOffsets.reserve(labelCount);
  unsigned long total = 0;
  for (unsigned long i = 0; i < labelCount; ++i) {
    // first, set up approximate values.
    labelOffsets[i] = (total += fBasicBlockOffsets[i]);
  }
  unsigned long blockSize = 0;
  unsigned long blockNumber = 0;
  OffsetList::const_iterator blockLabel = fBasicBlockOffsets.begin();
  for (unsigned long i = 0; i < fInstructions.size(); ) {
    if (!(blockLabel == fBasicBlockOffsets.end())) {
      if (*blockLabel == blockSize) {
	labelOffsets[blockNumber] = i;   // sets up the real offsets
	++blockLabel;
	blockSize = 0;
	++blockNumber;
      } else {
	++blockSize;
      }
    }
    i += FinalizeInstruction(i, labelOffsets);
  }
  total = 0;
  unsigned long previousLabel = 0;
  unsigned long labelNumber = 0;
  assert(fLineBeginnings.size() == fLineNumbers.size());
  unsigned long lineIndex = 0;
  unsigned long previousLine = 0;
  CJavaLineNumberTable::LineVector lineInfo;
  unsigned long lineCount = fLineBeginnings.size();
  lineInfo.reserve(lineCount);
  for (unsigned long i = 0; i < fInstructions.size(); ++i) {
    while (labelNumber < labelCount &&
	   previousLabel + fBasicBlockOffsets[labelNumber] == i) {
      labelOffsets[labelNumber++] = total;
      previousLabel = i;
    }
    while (lineIndex < lineCount &&
	   previousLine + fLineBeginnings[lineIndex] == i) {
      CJavaLineNumberTable::LineInfo linePair;
      linePair.fStartPC = total;
      linePair.fLineNumber = fLineNumbers[lineIndex++];
      lineInfo.push_back(linePair);
      previousLine = i;
    }
    total += InstructionSize(i, total);
  }
  while (labelNumber < labelCount) {
    labelOffsets[labelNumber++] = total;
  }
  total = 0;
  unsigned long lastInstruction = 0;
  for (unsigned long i = 0; i < fInstructions.size(); ++i) {
    EmitCode(i, total, labelOffsets, intoCodeAttribute);
    unsigned long instructionSize = InstructionSize(i, total);
    if (instructionSize != 0) {
      lastInstruction = total;
      total += instructionSize;
    }
  }
  CJavaCodeAttribute::ExceptionTable::iterator exception =
    intoCodeAttribute.ExceptionsBegin();
  for (; !(exception == intoCodeAttribute.ExceptionsEnd()); ++exception) {
    (*exception).fStartPC = labelOffsets[(*exception).fStartPC];
    unsigned long endOffset = labelOffsets[(*exception).fEndPC];
    // XXXX This gets around a bug in JDK.  The exception bounds are inclusive
    // XXXX on the low end and exclusive on the high end, so there is no way
    // XXXX to indicate that the last instruction in a method is covered by
    // XXXX an exception handler.  Their verifier pukes if you give any value
    // XXXX larger than 'lastInstruction.'
    if (endOffset >= total) {
      (*exception).fEndPC = lastInstruction;
    } else {
      (*exception).fEndPC = endOffset;
    }
    (*exception).fHandlerPC = labelOffsets[(*exception).fHandlerPC];
  }
  intoCodeAttribute.SetLineNumbers(new CJavaLineNumberTable(lineInfo));
}

//
//  Method name : InstructionSize
//  Description : Returns the number of bytes that will be taken up by the
//    instruction at the provided index when it is spit out to bytecode.  The
//    code will be written out starting at the provided offset.
//
unsigned long
CCodeSequence::InstructionSize(unsigned long index,
			       unsigned long startingAtPosition) const
{
  unsigned long result;
  switch (fInstructions[index].fOpcode) {
  case CJavaCodeAttribute::nop:
    result = 0;
    break;
  case CJavaCodeAttribute::tableswitch:
  case CJavaCodeAttribute::lookupswitch:
    result = 1 + (4 - (startingAtPosition + 1) % 4) % 4;
    while (fInstructions[++index].fOpcode == CJavaCodeAttribute::nop) {
      result += 4;
    }
    break;
  default:
    result = 1 + CJavaCodeAttribute::GetOpcodeArguments(
				    fInstructions[index].fOpcode);
  }
  return result;
}

//
//  Method name : FinalizeInstruction
//  Description : This call is used to set a finalized form of this instruction
//    to prepare it for final code emission.  In this form, any general-purpose
//    instructions are replaced with specialized versions and wide instructions
//    are used where needed.
//    This method returns an integer that tells how many instruction slots
//    are taken up when this method completes.  For example, if the code is
//    unchanged, this returns 1. If the instruction is removed, this returns 0,
//    and if one instruction became two, this would return 2.
//
unsigned long
CCodeSequence::FinalizeInstruction(unsigned long index,
				   const OffsetList& offsetLabels)
{
  int size = 1;
  switch (fInstructions[index].fOpcode) {
  case CJavaCodeAttribute::sipush:
    {
      short constant = fInstructions[index].fArguments.s2;
      if (constant >= SCHAR_MIN && constant <= SCHAR_MAX) {
	if (constant > -2 && constant < 6) {
	  fInstructions[index].fOpcode =
	    (CJavaCodeAttribute::Opcode)(CJavaCodeAttribute::iconst_0 +
					 constant);
	} else {
	  fInstructions[index].fOpcode = CJavaCodeAttribute::bipush;
	  fInstructions[index].fArguments.s1 = constant;
	}
      }
    }
    break;
  case CJavaCodeAttribute::ldc:
    if (fInstructions[index].fArguments.u4 > 255) {
      fInstructions[index].fOpcode = CJavaCodeAttribute::ldc2;
    }
    break;
  case CJavaCodeAttribute::iload:
    if (fInstructions[index].fArguments.u4 < 4) {
      fInstructions[index].fOpcode =
	(CJavaCodeAttribute::Opcode)(CJavaCodeAttribute::iload_0 +
				     fInstructions[index].fArguments.u4);
      break;
    }
  case CJavaCodeAttribute::lload:
    if (fInstructions[index].fArguments.u4 < 4) {
      fInstructions[index].fOpcode =
	(CJavaCodeAttribute::Opcode)(CJavaCodeAttribute::lload_0 +
				     fInstructions[index].fArguments.u4);
      break;
    }
  case CJavaCodeAttribute::fload:
    if (fInstructions[index].fArguments.u4 < 4) {
      fInstructions[index].fOpcode =
	(CJavaCodeAttribute::Opcode)(CJavaCodeAttribute::fload_0 +
				     fInstructions[index].fArguments.u4);
      break;
    }
  case CJavaCodeAttribute::dload:
    if (fInstructions[index].fArguments.u4 < 4) {
      fInstructions[index].fOpcode =
	(CJavaCodeAttribute::Opcode)(CJavaCodeAttribute::dload_0 +
				     fInstructions[index].fArguments.u4);
      break;
    }
  case CJavaCodeAttribute::aload:
    if (fInstructions[index].fArguments.u4 < 4) {
      fInstructions[index].fOpcode =
	(CJavaCodeAttribute::Opcode)(CJavaCodeAttribute::aload_0 +
				     fInstructions[index].fArguments.u4);
      break;
    }
  case CJavaCodeAttribute::istore:
    if (fInstructions[index].fArguments.u4 < 4) {
      fInstructions[index].fOpcode =
	(CJavaCodeAttribute::Opcode)(CJavaCodeAttribute::istore_0 +
				     fInstructions[index].fArguments.u4);
      break;
    }
  case CJavaCodeAttribute::lstore:
    if (fInstructions[index].fArguments.u4 < 4) {
      fInstructions[index].fOpcode =
	(CJavaCodeAttribute::Opcode)(CJavaCodeAttribute::lstore_0 +
				     fInstructions[index].fArguments.u4);
      break;
    }
  case CJavaCodeAttribute::fstore:
    if (fInstructions[index].fArguments.u4 < 4) {
      fInstructions[index].fOpcode =
	(CJavaCodeAttribute::Opcode)(CJavaCodeAttribute::fstore_0 +
				     fInstructions[index].fArguments.u4);
      break;
    }
  case CJavaCodeAttribute::dstore:
    if (fInstructions[index].fArguments.u4 < 4) {
      fInstructions[index].fOpcode =
	(CJavaCodeAttribute::Opcode)(CJavaCodeAttribute::dstore_0 +
				     fInstructions[index].fArguments.u4);
      break;
    }
  case CJavaCodeAttribute::astore:
    if (fInstructions[index].fArguments.u4 < 4) {
      fInstructions[index].fOpcode =
	(CJavaCodeAttribute::Opcode)(CJavaCodeAttribute::astore_0 +
				     fInstructions[index].fArguments.u4);
      break;
    }
  case CJavaCodeAttribute::ret:
    if (fInstructions[index].fArguments.u4 > 255) {
      Instruction wide;
      wide.fOpcode = CJavaCodeAttribute::wide;
      wide.fArguments.u4 = fInstructions[index].fArguments.u4 >> 8;
      fInstructions[index].fArguments.u4 &= 0xff;
      Insert(index, wide);
      size = 2;
    }
    break;    
  case CJavaCodeAttribute::iinc:
    if (fInstructions[index].fArguments.fIndexPair.index > 255) {
      Instruction wide;
      wide.fOpcode = CJavaCodeAttribute::wide;
      wide.fArguments.u4 =
	fInstructions[index].fArguments.fIndexPair.index >> 8;
      fInstructions[index].fArguments.fIndexPair.index &= 0xff;
      Insert(index, wide);
      size = 2;
    }
    break;    
  case CJavaCodeAttribute::op_goto:
    {
      unsigned long branch = offsetLabels[fInstructions[index].fArguments.u4];
      long offset = (branch - index) * kBytesPerInstruction;
      if (offset < SHRT_MIN || offset > SHRT_MAX) {
	fInstructions[index].fOpcode = CJavaCodeAttribute::goto_w;
      }
    }
    break;
  case CJavaCodeAttribute::jsr:
    {
      unsigned long branch = offsetLabels[fInstructions[index].fArguments.u4];
      long offset = (branch - index) * kBytesPerInstruction;
      if (offset < SHRT_MIN || offset > SHRT_MAX) {
	fInstructions[index].fOpcode = CJavaCodeAttribute::jsr_w;
      }
    }
    break;
  case CJavaCodeAttribute::ifeq:
  case CJavaCodeAttribute::ifne:
  case CJavaCodeAttribute::iflt:
  case CJavaCodeAttribute::ifge:
  case CJavaCodeAttribute::ifgt:
  case CJavaCodeAttribute::ifle:
  case CJavaCodeAttribute::if_icmpeq:
  case CJavaCodeAttribute::if_icmpne:
  case CJavaCodeAttribute::if_icmplt:
  case CJavaCodeAttribute::if_icmpge:
  case CJavaCodeAttribute::if_icmpgt:
  case CJavaCodeAttribute::if_icmple:
  case CJavaCodeAttribute::if_acmpeq:
  case CJavaCodeAttribute::if_acmpne:
  case CJavaCodeAttribute::ifnull:
  case CJavaCodeAttribute::ifnonnull:
    {
      unsigned long branch = offsetLabels[fInstructions[index].fArguments.u4];
      long offset = (branch - index) * kBytesPerInstruction;
      if (offset < SHRT_MIN || offset > SHRT_MAX) {
	Instruction replacement;
	switch (fInstructions[index].fOpcode) {
	case CJavaCodeAttribute::ifeq:
	  replacement.fOpcode = CJavaCodeAttribute::ifne;
	  break;
	case CJavaCodeAttribute::ifne:
	  replacement.fOpcode = CJavaCodeAttribute::ifeq;
	  break;
	case CJavaCodeAttribute::iflt:
	  replacement.fOpcode = CJavaCodeAttribute::ifge;
	  break;
	case CJavaCodeAttribute::ifge:
	  replacement.fOpcode = CJavaCodeAttribute::iflt;
	  break;
	case CJavaCodeAttribute::ifgt:
	  replacement.fOpcode = CJavaCodeAttribute::ifle;
	  break;
	case CJavaCodeAttribute::ifle:
	  replacement.fOpcode = CJavaCodeAttribute::ifgt;
	  break;
	case CJavaCodeAttribute::if_icmpeq:
	  replacement.fOpcode = CJavaCodeAttribute::if_icmpne;
	  break;
	case CJavaCodeAttribute::if_icmpne:
	  replacement.fOpcode = CJavaCodeAttribute::if_icmpeq;
	  break;
	case CJavaCodeAttribute::if_icmplt:
	  replacement.fOpcode = CJavaCodeAttribute::if_icmpge;
	  break;
	case CJavaCodeAttribute::if_icmpge:
	  replacement.fOpcode = CJavaCodeAttribute::if_icmplt;
	  break;
	case CJavaCodeAttribute::if_icmpgt:
	  replacement.fOpcode = CJavaCodeAttribute::if_icmple;
	  break;
	case CJavaCodeAttribute::if_icmple:
	  replacement.fOpcode = CJavaCodeAttribute::if_icmpgt;
	  break;
	case CJavaCodeAttribute::if_acmpeq:
	  replacement.fOpcode = CJavaCodeAttribute::if_acmpne;
	  break;
	case CJavaCodeAttribute::if_acmpne:
	  replacement.fOpcode = CJavaCodeAttribute::if_acmpeq;
	  break;
	case CJavaCodeAttribute::ifnull:
	  replacement.fOpcode = CJavaCodeAttribute::ifnonnull;
	  break;
	case CJavaCodeAttribute::ifnonnull:
	  replacement.fOpcode = CJavaCodeAttribute::ifnull;
	  break;
	}
	replacement.fArguments.fIndexPair.index = 8;  // skip over the goto_w
	replacement.fArguments.fIndexPair.index = 1;
	fInstructions[index].fOpcode = CJavaCodeAttribute::goto_w;
	Insert(index, replacement);
      } else {
	fInstructions[index].fArguments.fIndexPair.index =
	  fInstructions[index].fArguments.u4;
	fInstructions[index].fArguments.fIndexPair.u1 = 0;
      }
    }
    break;
  default:
    break;
  }
  return size;
}

//
//  Method name : EmitCode
//  Description : This method is used to spit the actual instructions used
//    into the code attribute 'intoAttribute.'  In the process, logical jumps
//    (to labels) are translated into absolute branches.
//
void
CCodeSequence::EmitCode(unsigned long index, unsigned long instructionLocation,
			const OffsetList& labelAddressMap,
			CJavaCodeAttribute& intoAttribute) const
{
  CJavaCodeAttribute::Opcode opcode = fInstructions[index].fOpcode;
  if (opcode != CJavaCodeAttribute::nop) {
    intoAttribute.AppendToCode((unsigned char)opcode);
    if (CJavaCodeAttribute::GetOpcodeArguments(opcode) != 0) {
      switch (opcode) {
      case CJavaCodeAttribute::ldc2:
      case CJavaCodeAttribute::ldc2_w:
      case CJavaCodeAttribute::getstatic:
      case CJavaCodeAttribute::putstatic:
      case CJavaCodeAttribute::getfield:
      case CJavaCodeAttribute::putfield:
      case CJavaCodeAttribute::invokevirtual:
      case CJavaCodeAttribute::invokenonvirtual:
      case CJavaCodeAttribute::invokestatic:
      case CJavaCodeAttribute::op_new:
      case CJavaCodeAttribute::anewarray:
      case CJavaCodeAttribute::checkcast:
      case CJavaCodeAttribute::instanceof:
      case CJavaCodeAttribute::ret_w:
	AppendIndex(index, intoAttribute);
	break;
      case CJavaCodeAttribute::bipush:
	intoAttribute.AppendToCode(
			   (unsigned char)fInstructions[index].fArguments.s1);
	break;
      case CJavaCodeAttribute::ldc:
      case CJavaCodeAttribute::iload:
      case CJavaCodeAttribute::lload:
      case CJavaCodeAttribute::fload:
      case CJavaCodeAttribute::dload:
      case CJavaCodeAttribute::aload:
      case CJavaCodeAttribute::istore:
      case CJavaCodeAttribute::lstore:
      case CJavaCodeAttribute::fstore:
      case CJavaCodeAttribute::dstore:
      case CJavaCodeAttribute::astore:
      case CJavaCodeAttribute::ret:
      case CJavaCodeAttribute::wide:
      case CJavaCodeAttribute::newarray:
	intoAttribute.AppendToCode(
		       (unsigned char)fInstructions[index].fArguments.u4);
	break;
      case CJavaCodeAttribute::ifeq:
      case CJavaCodeAttribute::ifne:
      case CJavaCodeAttribute::iflt:
      case CJavaCodeAttribute::ifge:
      case CJavaCodeAttribute::ifgt:
      case CJavaCodeAttribute::ifle:
      case CJavaCodeAttribute::if_icmpeq:
      case CJavaCodeAttribute::if_icmpne:
      case CJavaCodeAttribute::if_icmplt:
      case CJavaCodeAttribute::if_icmpge:
      case CJavaCodeAttribute::if_icmpgt:
      case CJavaCodeAttribute::if_icmple:
      case CJavaCodeAttribute::if_acmpeq:
      case CJavaCodeAttribute::if_acmpne:
      case CJavaCodeAttribute::ifnull:
      case CJavaCodeAttribute::ifnonnull:
	if (fInstructions[index].fArguments.fIndexPair.u1 != 0) {
	  intoAttribute.AppendToCode(
	    fInstructions[index].fArguments.fIndexPair.index);
	} else {
	  unsigned long branchTo =
	    labelAddressMap[fInstructions[index].fArguments.fIndexPair.index];
	  short offset = branchTo - instructionLocation;
	  intoAttribute.AppendToCode((unsigned short)offset);
	}
	break;
      case CJavaCodeAttribute::op_goto:
      case CJavaCodeAttribute::jsr:
	{
	  unsigned long branchTo =
	    labelAddressMap[fInstructions[index].fArguments.u4];
	  short offset = branchTo - instructionLocation;
	  intoAttribute.AppendToCode((unsigned short)offset);
	}
	break;
      case CJavaCodeAttribute::sipush:
	intoAttribute.AppendToCode(
	  (unsigned short)fInstructions[index].fArguments.s2);
	break;
      case CJavaCodeAttribute::goto_w:
      case CJavaCodeAttribute::jsr_w:
	{
	  unsigned long branchTo =
	    labelAddressMap[fInstructions[index].fArguments.u4];
	  long offset = branchTo - instructionLocation;
	  intoAttribute.AppendToCode((unsigned long)offset);
	}
	break;
      case CJavaCodeAttribute::iinc:
	intoAttribute.AppendToCode(
	   (unsigned char)fInstructions[index].fArguments.fIndexPair.index);
	intoAttribute.AppendToCode(
	   (unsigned char)fInstructions[index].fArguments.fIndexPair.u1);
	break;
      case CJavaCodeAttribute::invokeinterface:
	intoAttribute.AppendToCode(
	  fInstructions[index].fArguments.fIndexPair.index);
	intoAttribute.AppendToCode(
	  (unsigned char)fInstructions[index].fArguments.fIndexPair.u1);
	intoAttribute.AppendToCode((unsigned char)0);
	break;
      case CJavaCodeAttribute::multianewarray:
	intoAttribute.AppendToCode(
	  fInstructions[index].fArguments.fIndexPair.index);
	intoAttribute.AppendToCode(
	  (unsigned char)fInstructions[index].fArguments.fIndexPair.u1);
	break;
      case CJavaCodeAttribute::tableswitch:
	for (int i = 0; i < (4 - (instructionLocation + 1) % 4) % 4; ++i) {
	  intoAttribute.AppendToCode((unsigned char)0);
	}
	{
	  unsigned long numKeys = fInstructions[index].fArguments.u4;
	  intoAttribute.AppendToCode(
		     labelAddressMap[fInstructions[++index].fArguments.u4] -
		     instructionLocation);
	  intoAttribute.AppendToCode(fInstructions[++index].fArguments.u4);
	  intoAttribute.AppendToCode(fInstructions[++index].fArguments.u4);
	  for (unsigned long key = 0; key < numKeys; ++key) {
	    intoAttribute.AppendToCode(
		    labelAddressMap[fInstructions[++index].fArguments.u4] -
		    instructionLocation);
	  }
	}
	break;
      case CJavaCodeAttribute::lookupswitch:
	for (int i = 0; i < (4 - (instructionLocation + 1) % 4) % 4; ++i) {
	  intoAttribute.AppendToCode((unsigned char)0);
	}
	{
	  intoAttribute.AppendToCode(
		    labelAddressMap[fInstructions[++index].fArguments.u4] -
		    instructionLocation);
	  unsigned long numKeys = fInstructions[++index].fArguments.u4;
	  intoAttribute.AppendToCode(numKeys);
	  for (unsigned long key = 0; key < numKeys; ++key) {
	    intoAttribute.AppendToCode(fInstructions[++index].fArguments.u4);
	    intoAttribute.AppendToCode(
		       labelAddressMap[fInstructions[++index].fArguments.u4] -
		       instructionLocation);
	  }
	}
	break;
      default:
	assert(0);
      }
    }
  }
}

//
//  Method name : AppendIndex
//  Description : This method takes the instruction at the provided location
//    and interprets its arguments as a two-byte index.  This index is 
//    translated into bytecode format and appended onto the provided code
//    attribute.
//
void
CCodeSequence::AppendIndex(unsigned long instruction,
			   CJavaCodeAttribute& onto) const
{
  onto.AppendToCode((unsigned short)fInstructions[instruction].fArguments.u4);
}

//
//  Method name : MatchRange
//  Description : This method is used by the code sequence to try to match an
//    array of opcodes against a range from this sequence.  It begins looking
//    for the match at 'startingAt,' and tries to match all 'length' characters
//    in 'pattern.'  This method returns the length of the resulting match if
//    a match is found or 0 if no match can be made.  The returned length will
//    either be equal to 'length' or greater since no-op codes in this
//    sequence are ignored during the match.
//    If 'crossBasicBlocks' is true, this method will allow matches to span
//    over a jump label, otherwise this method will return 0 if a match cannot
//    be performed in the basic block holding 'startingAt.'
//
unsigned long
CCodeSequence::MatchRange(const CJavaCodeAttribute::Opcode pattern[],
			  unsigned long patternLength,
			  unsigned long startingAt, bool crossBasicBlocks)
{
  unsigned long lengthLimit = size() - startingAt;
  if (!crossBasicBlocks) {
    unsigned long endBlock = 0;
    for (OffsetList::iterator i = fBasicBlockOffsets.begin();
       !(i == fBasicBlockOffsets.end()); ++i) {
      endBlock += *i;
      if (endBlock > startingAt) {
	lengthLimit = endBlock - startingAt;
	break;
      }
    }
  }
  unsigned long result = 0;
  if (lengthLimit >= patternLength &&
      fInstructions[startingAt].fOpcode == pattern[0]) {
    unsigned long patternIndex = 0;
    for (unsigned long i = startingAt; i < startingAt + lengthLimit; ++i) {
      if (fInstructions[i].fOpcode != CJavaCodeAttribute::nop) {
	if (fInstructions[i].fOpcode != pattern[patternIndex]) {
	  break;
	} else if (++patternIndex == patternLength) {
	  result = (i - startingAt) + 1;
	  break;
	}
      }
    }
  }
  return result;
}

//
//  Method name : ReplaceZeroCompare
//  Description : This method looks for an opportunity to replace a chunk of
//    code beginning at index 'startingAt' with a shorter sequence making use
//    of optimized operations for null and 0 comparison.
//    This method returns true if a replacement was made.
//
bool
CCodeSequence::ReplaceZeroCompare(unsigned long startingAt)
{
  static const CJavaCodeAttribute::Opcode kPattern1[] = {
    CJavaCodeAttribute::aconst_null,
    CJavaCodeAttribute::if_acmpeq
  };
  unsigned long matchLength =
    MatchRange(kPattern1, sizeof(kPattern1)/sizeof(kPattern1[0]), startingAt);
  if (matchLength != 0) {
    fInstructions[startingAt + matchLength - 1].fOpcode =
      CJavaCodeAttribute::ifnull;
  }
  if (matchLength == 0) {
    static const CJavaCodeAttribute::Opcode kPattern2[] = {
      CJavaCodeAttribute::aconst_null,
      CJavaCodeAttribute::if_acmpne
    };
    matchLength = MatchRange(kPattern2, sizeof(kPattern2)/sizeof(kPattern2[0]),
			     startingAt);
    if (matchLength != 0) {
      fInstructions[startingAt + matchLength - 1].fOpcode =
	CJavaCodeAttribute::ifnonnull;
    }
  }
  if (fInstructions[startingAt].fArguments.s2 == 0) {
    if (matchLength == 0) {
      static const CJavaCodeAttribute::Opcode kPattern3[] = {
	CJavaCodeAttribute::sipush,
	CJavaCodeAttribute::if_icmpeq
      };
      matchLength = MatchRange(kPattern3,
			sizeof(kPattern3)/sizeof(kPattern3[0]), startingAt);
      if (matchLength != 0) {
	fInstructions[startingAt + matchLength - 1].fOpcode =
	  CJavaCodeAttribute::ifeq;
      }
    }
    if (matchLength == 0) {
      static const CJavaCodeAttribute::Opcode kPattern4[] = {
	CJavaCodeAttribute::sipush,
	CJavaCodeAttribute::if_icmpne
      };
      matchLength = MatchRange(kPattern4,
		       sizeof(kPattern4)/sizeof(kPattern4[0]), startingAt);
      if (matchLength != 0) {
	fInstructions[startingAt + matchLength - 1].fOpcode =
	  CJavaCodeAttribute::ifne;
      }
    }
    if (matchLength == 0) {
      static const CJavaCodeAttribute::Opcode kPattern5[] = {
	CJavaCodeAttribute::sipush,
	CJavaCodeAttribute::if_icmplt
      };
      matchLength = MatchRange(kPattern5,
			sizeof(kPattern5)/sizeof(kPattern5[0]), startingAt);
      if (matchLength != 0) {
	fInstructions[startingAt + matchLength - 1].fOpcode =
	  CJavaCodeAttribute::iflt;
      }
    }
    if (matchLength == 0) {
      static const CJavaCodeAttribute::Opcode kPattern6[] = {
	CJavaCodeAttribute::sipush,
	CJavaCodeAttribute::if_icmpge
      };
      matchLength = MatchRange(kPattern6,
		      sizeof(kPattern6)/sizeof(kPattern6[0]), startingAt);
      if (matchLength != 0) {
	fInstructions[startingAt + matchLength - 1].fOpcode =
	  CJavaCodeAttribute::ifge;
      }
    }
    if (matchLength == 0) {
      static const CJavaCodeAttribute::Opcode kPattern7[] = {
	CJavaCodeAttribute::sipush,
	CJavaCodeAttribute::if_icmpgt
      };
      matchLength = MatchRange(kPattern7,
		       sizeof(kPattern7)/sizeof(kPattern7[0]), startingAt);
      if (matchLength != 0) {
	fInstructions[startingAt + matchLength - 1].fOpcode =
	  CJavaCodeAttribute::ifgt;
      }
    }
    if (matchLength == 0) {
      static const CJavaCodeAttribute::Opcode kPattern8[] = {
	CJavaCodeAttribute::sipush,
	CJavaCodeAttribute::if_icmple
      };
      matchLength = MatchRange(kPattern8,
		       sizeof(kPattern8)/sizeof(kPattern8[0]), startingAt);
      if (matchLength != 0) {
	fInstructions[startingAt + matchLength - 1].fOpcode =
	  CJavaCodeAttribute::ifle;
      }
    }
  }
  bool replaced = matchLength != 0;
  if (replaced) {
    Remove(startingAt, matchLength - 1);
  }
  return replaced;
}

//
//  Method name : ReplaceIncrement
//  Description : This method looks for an opportunity to replace a chunk of
//    code beginning at index 'startingAt' with a shorter sequence making use
//    of the 'iinc' operation.
//    This method returns true if a replacement was made.
//
bool
CCodeSequence::ReplaceIncrement(unsigned long startingAt)
{
  bool replaced = false;
  static const CJavaCodeAttribute::Opcode kPattern1[] = {
    CJavaCodeAttribute::iload,
    CJavaCodeAttribute::sipush,
    CJavaCodeAttribute::iadd,
    CJavaCodeAttribute::istore
  };
  unsigned long matchLength =
    MatchRange(kPattern1, sizeof(kPattern1)/sizeof(kPattern1[0]), startingAt);
  if (matchLength == 0) {
    static const CJavaCodeAttribute::Opcode kPattern2[] = {
      CJavaCodeAttribute::sipush,
      CJavaCodeAttribute::iload,
      CJavaCodeAttribute::iadd,
      CJavaCodeAttribute::istore
    };
    matchLength = MatchRange(kPattern2, sizeof(kPattern2)/sizeof(kPattern2[0]),
			     startingAt);
  }
  if (matchLength == 0) {
    static const CJavaCodeAttribute::Opcode kPattern3[] = {
      CJavaCodeAttribute::iload,
      CJavaCodeAttribute::sipush,
      CJavaCodeAttribute::isub,
      CJavaCodeAttribute::istore
    };
    matchLength = MatchRange(kPattern3, sizeof(kPattern3)/sizeof(kPattern3[0]),
			     startingAt);
  }
  if (matchLength == 0) {
    static const CJavaCodeAttribute::Opcode kPattern4[] = {
      CJavaCodeAttribute::iload,
      CJavaCodeAttribute::sipush,
      CJavaCodeAttribute::isub,
      CJavaCodeAttribute::dup,
      CJavaCodeAttribute::istore
    };
    matchLength = MatchRange(kPattern4, sizeof(kPattern4)/sizeof(kPattern4[0]),
			     startingAt);
  }
  if (matchLength == 0) {
    static const CJavaCodeAttribute::Opcode kPattern5[] = {
      CJavaCodeAttribute::iload,
      CJavaCodeAttribute::sipush,
      CJavaCodeAttribute::iadd,
      CJavaCodeAttribute::dup,
      CJavaCodeAttribute::istore
    };
    matchLength = MatchRange(kPattern5, sizeof(kPattern5)/sizeof(kPattern5[0]),
			     startingAt);
  }
  if (matchLength == 0) {
    static const CJavaCodeAttribute::Opcode kPattern6[] = {
      CJavaCodeAttribute::sipush,
      CJavaCodeAttribute::iload,
      CJavaCodeAttribute::iadd,
      CJavaCodeAttribute::dup,
      CJavaCodeAttribute::istore
    };
    matchLength = MatchRange(kPattern6, sizeof(kPattern6)/sizeof(kPattern6[0]),
			     startingAt);
  }
  if (matchLength != 0) {
    unsigned short loadIndex;
    unsigned short storeIndex;
    short incrementBy;
    bool hasDup = false;
    bool add;
    for (unsigned long i = startingAt; i < matchLength + startingAt; ++i) {
      switch (fInstructions[i].fOpcode) {
      case CJavaCodeAttribute::iload:
	loadIndex = fInstructions[i].fArguments.u4;
	break;
      case CJavaCodeAttribute::istore:
	storeIndex = fInstructions[i].fArguments.u4;
	break;
      case CJavaCodeAttribute::sipush:
	incrementBy = fInstructions[i].fArguments.s2;
	break;
      case CJavaCodeAttribute::iadd:
	add = true;
	break;
      case CJavaCodeAttribute::isub:
	add = false;
	break;
      case CJavaCodeAttribute::dup:
	hasDup = true;
	break;
      }
    }
    if (!add) {
      incrementBy = -incrementBy;
    }
    if (incrementBy >= SCHAR_MIN && incrementBy <= SCHAR_MAX &&
	loadIndex == storeIndex) {
      fInstructions[startingAt].fOpcode = CJavaCodeAttribute::iinc;
      fInstructions[startingAt].fArguments.fIndexPair.index = loadIndex;
      fInstructions[startingAt].fArguments.fIndexPair.u1 = incrementBy;
      if (hasDup) {
	fInstructions[startingAt + 1].fOpcode = CJavaCodeAttribute::iload;
	fInstructions[startingAt + 1].fArguments.u4 = loadIndex;
	Remove(startingAt + 2, matchLength - 2);
      } else {
	Remove(startingAt + 1, matchLength - 1);
      }      
      replaced = true;
    }
  }
  return replaced;
}

//
//  Method name : ReplaceBooleanBranch
//  Description : This method looks for an opportunity to replace a chunk of
//    code beginning at index 'startingAt' with a shorter sequence that
//    eliminates a verbose comparison and branch.  It replaces common
//    Java code that looks like this:     with this:
//         if_icmplt A                      B: if_icmplt X
//         iconst_0
//         goto B
//      A: iconst_1
//      B: ifeq X
//
//    This method returns true if a replacement was made.
//
//    XXXX Right now, this method is a little fast-and-loose.  It matches
//    XXXX across basic block boundaries, assuming that the basic blocks
//    XXXX starting at A and B are only accessed within this construct.  To
//    XXXX be more strict, this should check to make sure that no one else
//    XXXX is pointing at A or B.  I know this won't ever happen under the
//    XXXX current compilation, but keep it in mind for the future.
//
bool
CCodeSequence::ReplaceBooleanBranch(unsigned long startingAt)
{
  static CJavaCodeAttribute::Opcode pattern1[] = {
    CJavaCodeAttribute::nop,   // this element is successively replaced...
    CJavaCodeAttribute::iconst_0,
    CJavaCodeAttribute::op_goto,
    CJavaCodeAttribute::iconst_1,
    CJavaCodeAttribute::ifeq
  };
  static CJavaCodeAttribute::Opcode pattern2[] = {
    CJavaCodeAttribute::nop,   // this element is successively replaced...
    CJavaCodeAttribute::iconst_0,
    CJavaCodeAttribute::op_goto,
    CJavaCodeAttribute::iconst_1,
    CJavaCodeAttribute::ifne
  };
  unsigned long matchLength;
  pattern1[0] = pattern2[0] = CJavaCodeAttribute::if_icmpeq;
  const unsigned long patternLength = sizeof(pattern1)/sizeof(pattern1[0]);
  CJavaCodeAttribute::Opcode replacement = CJavaCodeAttribute::if_icmpne;
  matchLength = MatchRange(pattern1, patternLength, startingAt, true);
  if (matchLength == 0) {
    matchLength = MatchRange(pattern2, patternLength, startingAt, true);
    replacement = pattern2[0];
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::if_icmpne;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::if_icmpeq;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::if_icmplt;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::if_icmpge;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::if_icmpge;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::if_icmplt;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::if_icmpgt;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::if_icmple;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::if_icmple;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::if_icmpgt;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::if_acmpeq;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::if_acmpne;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::if_acmpne;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::if_acmpeq;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::ifeq;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::ifne;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::ifne;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::ifeq;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::iflt;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::ifge;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::ifge;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::iflt;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::ifgt;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::ifle;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::ifle;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::ifgt;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::ifnull;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::ifnonnull;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  if (matchLength == 0) {
    pattern1[0] = pattern2[0] = CJavaCodeAttribute::ifnonnull;
    matchLength = MatchRange(pattern1, patternLength, startingAt, true);
    if (matchLength > 0) {
      replacement = CJavaCodeAttribute::ifnull;
    } else {
      matchLength = MatchRange(pattern2, patternLength, startingAt, true);
      replacement = pattern2[0];
    }
  }
  bool replaced = matchLength != 0;
  if (replaced) {
    fInstructions[startingAt].fOpcode = replacement;
    fInstructions[startingAt].fArguments.u4 =
      fInstructions[startingAt + matchLength - 1].fArguments.u4;
    Remove(startingAt + 1, matchLength - 1);
  }
  return replaced;
}

//
//  Method name : ReplaceBranchToBranch
//  Description : This method looks for an opportunity to replace a chunk of
//    code beginning at index 'startingAt' with a shorter sequence that
//    eliminates a branch to another branch or a return statement.
//    This method returns true if a replacement was made.
//
bool
CCodeSequence::ReplaceBranchToBranch(unsigned long startingAt)
{
  bool replaced = false;
  unsigned long branchTo = startingAt;
  switch (fInstructions[startingAt].fOpcode) {
  case CJavaCodeAttribute::ifeq:
  case CJavaCodeAttribute::ifne:
  case CJavaCodeAttribute::if_acmpeq:
  case CJavaCodeAttribute::if_acmpne:
  case CJavaCodeAttribute::ifnull:
  case CJavaCodeAttribute::ifnonnull:
  case CJavaCodeAttribute::if_icmpeq:
  case CJavaCodeAttribute::if_icmpne:
  case CJavaCodeAttribute::if_icmplt:
  case CJavaCodeAttribute::if_icmple:
  case CJavaCodeAttribute::if_icmpge:
  case CJavaCodeAttribute::if_icmpgt:
  case CJavaCodeAttribute::iflt:
  case CJavaCodeAttribute::ifle:
  case CJavaCodeAttribute::ifgt:
  case CJavaCodeAttribute::ifge:
  case CJavaCodeAttribute::op_goto:
    branchTo = LabelToInstruction(fInstructions[startingAt].fArguments.u4);
    switch (fInstructions[branchTo].fOpcode) {
    case CJavaCodeAttribute::op_goto:
      if (branchTo != startingAt) {
	fInstructions[startingAt].fArguments.u4 =
	  fInstructions[branchTo].fArguments.u4;
	replaced = true;
      }
      break;
    case CJavaCodeAttribute::iconst_0:
      while (fInstructions[++branchTo].fOpcode == CJavaCodeAttribute::nop) ;
      while (fInstructions[branchTo].fOpcode ==
	     CJavaCodeAttribute::op_goto) {
	branchTo =
	  LabelToInstruction(fInstructions[branchTo].fArguments.u4);
      }
      switch (fInstructions[branchTo].fOpcode) {
      case CJavaCodeAttribute::ifeq:
	fInstructions[startingAt].fArguments.u4 =
	  fInstructions[branchTo].fArguments.u4;
	replaced = true;
	break;
      case CJavaCodeAttribute::ifne:
	replaced = InstructionToLabel(branchTo + 1,
				      fInstructions[startingAt].fArguments.u4);
      }
      break;
    case CJavaCodeAttribute::iconst_1:
      while (fInstructions[++branchTo].fOpcode == CJavaCodeAttribute::nop) ;
      while (fInstructions[branchTo].fOpcode ==
	     CJavaCodeAttribute::op_goto) {
	branchTo =
	  LabelToInstruction(fInstructions[branchTo].fArguments.u4);
      }
      switch (fInstructions[branchTo].fOpcode) {
      case CJavaCodeAttribute::ifne:
	fInstructions[startingAt].fArguments.u4 =
	  fInstructions[branchTo].fArguments.u4;
	replaced = true;
	break;
      case CJavaCodeAttribute::ifeq:
	replaced = InstructionToLabel(branchTo + 1,
				      fInstructions[startingAt].fArguments.u4);
      }
      break;
    }
    break;
  case CJavaCodeAttribute::iconst_0:
    while (fInstructions[++branchTo].fOpcode == CJavaCodeAttribute::nop) ;
    while (fInstructions[branchTo].fOpcode == CJavaCodeAttribute::op_goto) {
      branchTo = LabelToInstruction(fInstructions[branchTo].fArguments.u4);
    }
    switch (fInstructions[branchTo].fOpcode) {
    case CJavaCodeAttribute::ifeq:
      fInstructions[startingAt].fOpcode = CJavaCodeAttribute::op_goto;
      fInstructions[startingAt].fArguments.u4 =
	fInstructions[branchTo].fArguments.u4;
      replaced = true;
      break;
    case CJavaCodeAttribute::ifne:
      fInstructions[startingAt].fOpcode = CJavaCodeAttribute::op_goto;
      replaced = InstructionToLabel(branchTo + 1,
				    fInstructions[startingAt].fArguments.u4);
    }
    break;
  case CJavaCodeAttribute::iconst_1:
    while (fInstructions[++branchTo].fOpcode == CJavaCodeAttribute::nop) ;
    while (fInstructions[branchTo].fOpcode == CJavaCodeAttribute::op_goto) {
      branchTo = LabelToInstruction(fInstructions[branchTo].fArguments.u4);
    }
    switch (fInstructions[branchTo].fOpcode) {
    case CJavaCodeAttribute::ifne:
      fInstructions[startingAt].fOpcode = CJavaCodeAttribute::op_goto;
      fInstructions[startingAt].fArguments.u4 =
	fInstructions[branchTo].fArguments.u4;
      replaced = true;
      break;
    case CJavaCodeAttribute::ifeq:
      fInstructions[startingAt].fOpcode = CJavaCodeAttribute::op_goto;
      replaced = InstructionToLabel(branchTo + 1,
				    fInstructions[startingAt].fArguments.u4);
    }
    break;
  }
  if (fInstructions[startingAt].fOpcode == CJavaCodeAttribute::op_goto) {
    unsigned long branchTo =
      LabelToInstruction(fInstructions[startingAt].fArguments.u4);
    switch (fInstructions[branchTo].fOpcode) {
    case CJavaCodeAttribute::op_return:
    case CJavaCodeAttribute::ireturn:
    case CJavaCodeAttribute::lreturn:
    case CJavaCodeAttribute::freturn:
    case CJavaCodeAttribute::dreturn:
    case CJavaCodeAttribute::areturn:
      fInstructions[startingAt].fOpcode = fInstructions[branchTo].fOpcode;
      replaced = true;
    }
  }
  return replaced;
}

//
//  Method name : PeepholeOptimize
//  Description : This method tries to perform peephole optimizations over
//    this code sequence.  This operation should be performed before finalizing
//    the code, or else it will fail miserably.
//
void
CCodeSequence::PeepholeOptimize()
{
  // PrintShortDebug(cout);
  for (unsigned long i = 0; i < size();) {
    if (ReplaceIncrement(i)) continue;
    if (ReplaceZeroCompare(i)) continue;
    if (ReplaceBooleanBranch(i)) continue;
    if (ReplaceBranchToBranch(i)) continue;
    ++i;
  }
  // cout << "AFTER:" << endl;
  // PrintShortDebug(cout);
}

//
//  Method name : LabelToInstruction
//  Description : This method takes a label index returned by CreateBlockLabel
//    and returns the actual instruction index that this currently corresponds
//    to.
//
unsigned long
CCodeSequence::LabelToInstruction(unsigned long blockLabelIndex) const
{
  assert(blockLabelIndex < fBasicBlockOffsets.size());
  unsigned long total = 0;
  for (unsigned long i = 0; i <= blockLabelIndex; ++i) {
    total += fBasicBlockOffsets[i];
  }
  return total;
}

//
//  Method name : InstructionToLabel
//  Description : This method takes an instruction index and looks for the
//    jump label going to this location.  If a label is found, this returns
//    true and the provided label is set.  Otherwise, this returns false.
//
bool
CCodeSequence::InstructionToLabel(unsigned long instructionIndex,
				  unsigned long& label) const
{
  unsigned long total = 0;
  for (unsigned long i = 0; i < fBasicBlockOffsets.size(); ++i) {
    total += fBasicBlockOffsets[i];
    if (total == instructionIndex) {
      label = i;
      return true;
    } else if (total > instructionIndex) {
      break;
    }
  }
  return false;
}
