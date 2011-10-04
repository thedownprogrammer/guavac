// Copyright (c) 1996  David Engberg  All rights reserved
// $Id: CodeSequence.h,v 1.4 1996/07/12 20:43:40 geppetto Exp $
#ifndef _CodeSequence_h
#define _CodeSequence_h
#pragma interface

#include "JavaCodeAttribute.h"
#include <deque>
#include <vector>
class ostream;

//
//  Class name : CCodeSequence
//  Description : This class encapsulates a sequence of Java instructions under
//    construction.
//
class CCodeSequence {
public:
  CCodeSequence();
  CCodeSequence(const CCodeSequence& source);
  ~CCodeSequence();
  CCodeSequence& operator=(const CCodeSequence& source);

  //
  //  Struct name : CInstruction
  //  Description : This structure represents one instruction in the Java byte
  //    code sequence, along with any arguments it has.  It is meant to be
  //    small (8 bytes) and dumb to make it fast to copy and modify.
  //    As such, it doesn't present a very OO interface.  This is an
  //    intentional optimization, the alternative would rely on huge,
  //    heap-allocated polymorphism, which would be serious overkill for
  //    the job.
  //
  struct Instruction {
    CJavaCodeAttribute::Opcode fOpcode;
    union {
      unsigned long u4;
      unsigned long l4;
      signed short s2;
      signed char s1;
      struct {
	unsigned short index : 16;
	unsigned char u1 : 8;
      } fIndexPair;
    } fArguments;
  };

  typedef vector<Instruction> InstructionList;
  typedef vector<unsigned long> OffsetList;

  unsigned long size() const { return fInstructions.size(); }
  Instruction& operator[](unsigned long index) { return fInstructions[index]; }
  const Instruction& operator[](unsigned long index) const 
     { return fInstructions[index]; }
  InstructionList::const_iterator GetInstructionsBegin() const;
  InstructionList::const_iterator GetInstructionsEnd() const;
  void Insert(unsigned long index, const Instruction& value);
  void Append(const Instruction& value, unsigned long lineNumber = 0);
  void Append(CJavaCodeAttribute::Opcode opcode, unsigned long lineNumber = 0);
  void Append(CJavaCodeAttribute::Opcode opcode, unsigned long u4,
	      unsigned long lineNumber);
  void Remove(unsigned long index);
  void Remove(unsigned long index, unsigned long count);
  unsigned long CreateBlockLabel();
  // unsigned long PeekNextBlockLabel() const;
  unsigned long LabelToInstruction(unsigned long blockLabelIndex) const;
  bool InstructionToLabel(unsigned long blockLabelIndex,
			  unsigned long& label) const;

  void PeepholeOptimize();
  void EliminateDeadCode(CJavaCodeAttribute& inCodeAttribute);
  void Finalize(CJavaCodeAttribute& intoCodeAttribute);

  void PrintShortDebug(ostream& toStream) const;

protected:
  unsigned long FinalizeInstruction(unsigned long instructionIndex,
				    const OffsetList& offsetLabels);
  unsigned long InstructionSize(unsigned long instructionIndex,
				unsigned long startingAtPosition) const;
  void EmitCode(unsigned long instruction, unsigned long instructionLocation,
		const OffsetList& offsetLabels,
		CJavaCodeAttribute& into) const;
  void AppendIndex(unsigned long instruction, CJavaCodeAttribute& onto) const;

  unsigned long MatchRange(const CJavaCodeAttribute::Opcode pattern[],
			   unsigned long patternLength,
			   unsigned long startingAt,
			   bool crossBasicBlocks = false);
  bool ReplaceIncrement(unsigned long startingAt);
  bool ReplaceZeroCompare(unsigned long startingAt);
  bool ReplaceBooleanBranch(unsigned long startingAt);
  bool ReplaceBranchToBranch(unsigned long startingAt);
  void CreateBlockIfConditional(CJavaCodeAttribute::Opcode instruction);

  void MarkReachableFrom(unsigned long index, bool reachable[]);

private:
  void PrintInstruction(const Instruction& instruction,
			ostream& toStream) const;

  InstructionList fInstructions;
  OffsetList fBasicBlockOffsets;
  unsigned long fLastBasicBlock;
  vector<unsigned long> fLineNumbers;
  OffsetList fLineBeginnings;
  unsigned long fLastLine;
};

#endif
