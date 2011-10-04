// Copyright (c) 1995  David Engberg  All rights reserved
// $Id: JavaCodeAttribute.h,v 1.7 1996/07/12 20:42:34 geppetto Exp $
#ifndef _JavaCodeAttribute_h
#define _JavaCodeAttribute_h

#include "JavaAttribute.h"
class CJavaLineNumberTable;
class CJavaLocalVariableTable;
class CCompiler;
class CJavaClassFile;

//
//  Class name : CJavaCodeAttribute
//  Description : This is the attribute used in a method information
//    node to store the code for that method.
//
class CJavaCodeAttribute : public CJavaAttribute {
  DynamicCastDeclarations;
public:
  static CJavaCodeAttribute* ParseBuffer(string::const_iterator& javaBuffer,
					 const CJavaClassFile& classFile);
  virtual ~CJavaCodeAttribute();

  void Disassemble(ostream& toStream) const;
  string Compile(CJavaClassFile& inClass) const;

  typedef struct {
    unsigned short fStartPC;
    unsigned short fEndPC;
    unsigned short fHandlerPC;
    JavaConstantIndex fCatchType;
  } ExceptionInfo;

  typedef enum {
    nop = 0,
    aconst_null = 1,
    iconst_m1 = 2,
    iconst_0 = 3,
    iconst_1 = 4,
    iconst_2 = 5,
    iconst_3 = 6,
    iconst_4 = 7,
    iconst_5 = 8,
    lconst_0 = 9,
    lconst_1 = 10,
    fconst_0 = 11,
    fconst_1 = 12,
    fconst_2 = 13,
    dconst_0 = 14,
    dconst_1 = 15,
    bipush = 16,
    sipush = 17,
    ldc = 18,
    ldc2 = 19,
    ldc2_w = 20,
    iload = 21,
    lload = 22,
    fload = 23,
    dload = 24,
    aload = 25,
    iload_0 = 26,
    iload_1 = 27,
    iload_2 = 28,
    iload_3 = 29,
    lload_0 = 30,
    lload_1 = 31,
    lload_2 = 32,
    lload_3 = 33,
    fload_0 = 34,
    fload_1 = 35,
    fload_2 = 36,
    fload_3 = 37,
    dload_0 = 38,
    dload_1 = 39,
    dload_2 = 40,
    dload_3 = 41,
    aload_0 = 42,
    aload_1 = 43,
    aload_2 = 44,
    aload_3 = 45,
    iaload = 46,
    laload = 47,
    faload = 48,
    daload = 49,
    aaload = 50,
    baload = 51,
    caload = 52,
    saload = 53,
    istore = 54,
    lstore = 55,
    fstore = 56,
    dstore = 57,
    astore = 58,
    istore_0 = 59,
    istore_1 = 60,
    istore_2 = 61,
    istore_3 = 62,
    lstore_0 = 63,
    lstore_1 = 64,
    lstore_2 = 65,
    lstore_3 = 66,
    fstore_0 = 67,
    fstore_1 = 68,
    fstore_2 = 69,
    fstore_3 = 70,
    dstore_0 = 71,
    dstore_1 = 72,
    dstore_2 = 73,
    dstore_3 = 74,
    astore_0 = 75,
    astore_1 = 76,
    astore_2 = 77,
    astore_3 = 78,
    iastore = 79,
    lastore = 80,
    fastore = 81,
    dastore = 82,
    aastore = 83,
    bastore = 84,
    castore = 85,
    sastore = 86,
    pop = 87,
    pop2 = 88,
    dup = 89,
    dup_x1 = 90,
    dup_x2 = 91,
    dup2 = 92,
    dup2_x1 = 93,
    dup2_x2 = 94,
    swap = 95,
    iadd = 96,
    ladd = 97,
    fadd = 98,
    dadd = 99,
    isub = 100,
    lsub = 101,
    fsub = 102,
    dsub = 103,
    imul = 104,
    lmul = 105,
    fmul = 106,
    dmul = 107,
    idiv = 108,
    ldiv = 109,
    fdiv = 110,
    ddiv = 111,
    irem = 112,
    lrem = 113,
    frem = 114,
    drem = 115,
    ineg = 116,
    lneg = 117,
    fneg = 118,
    dneg = 119,
    ishl = 120,
    lshl = 121,
    ishr = 122,
    lshr = 123,
    iushr = 124,
    lushr = 125,
    iand = 126,
    land = 127,
    ior = 128,
    lor = 129,
    ixor = 130,
    lxor = 131,
    iinc = 132,
    i2l = 133,
    i2f = 134,
    i2d = 135,
    l2i = 136,
    l2f = 137,
    l2d = 138,
    f2i = 139,
    f2l = 140,
    f2d = 141,
    d2i = 142,
    d2l = 143,
    d2f = 144,
    int2byte = 145,
    int2char = 146,
    int2short = 147,
    lcmp = 148,
    fcmpl = 149,
    fcmpg = 150,
    dcmpl = 151,
    dcmpg = 152,
    ifeq = 153,
    ifne = 154,
    iflt = 155,
    ifge = 156,
    ifgt = 157,
    ifle = 158,
    if_icmpeq = 159,
    if_icmpne = 160,
    if_icmplt = 161,
    if_icmpge = 162,
    if_icmpgt = 163,
    if_icmple = 164,
    if_acmpeq = 165,
    if_acmpne = 166,
    op_goto = 167,      // heh.  'goto' is a C++ reserved word
    jsr = 168,
    ret = 169,
    tableswitch = 170,
    lookupswitch = 171,
    ireturn = 172,
    lreturn = 173,
    freturn = 174,
    dreturn = 175,
    areturn = 176,
    op_return = 177,    // 'return' is also a reserved word...
    getstatic = 178,
    putstatic = 179,
    getfield = 180,
    putfield = 181,
    invokevirtual = 182,
    invokenonvirtual = 183,
    invokestatic = 184,
    invokeinterface = 185,
    op_new = 187,       // as is 'new'
    newarray = 188,
    anewarray = 189,
    arraylength = 190,
    athrow = 191,
    checkcast = 192,
    instanceof = 193,
    monitorenter = 194,
    monitorexit = 195,
    wide = 196,
    multianewarray = 197,
    ifnull = 198,
    ifnonnull = 199,
    goto_w = 200,
    jsr_w = 201,
    breakpoint = 202,
    ret_w = 209
  } Opcode;

  typedef struct {
    const char* fName;
    short fOperands;
  } OpcodeInfo;

  static void PrintOpcodeName(Opcode opcode, ostream& toStream);
  static short GetOpcodeArguments(Opcode forOpcode);

  unsigned short AddLocalVariable(unsigned short width = 1);
  unsigned long CountLocalVariables() const { return fMaxLocals; }
  const string& GetCode() const { return fCode; }
  unsigned long GetCodeSize() const { return fCode.size(); }
  void AppendToCode(unsigned char byte) { fCode += byte; }
  void AppendToCode(unsigned short value);
  void AppendToCode(unsigned long value);

  void AddExceptionHandler(const ExceptionInfo& handlerInfo);
  typedef vector<ExceptionInfo> ExceptionTable;
  ExceptionTable::const_iterator ExceptionsBegin() const;
  ExceptionTable::const_iterator ExceptionsEnd() const;
  ExceptionTable::iterator ExceptionsBegin();
  ExceptionTable::iterator ExceptionsEnd();
  void RemoveExceptionHandler(const ExceptionTable::iterator& exception);
  void SetLineNumbers(CJavaLineNumberTable* adoptTable);
  unsigned long ClosestLineNumber(unsigned long toProgramCounter) const;

protected:  
  friend class CCompiler;
  CJavaCodeAttribute();
private:

  static const OpcodeInfo gOpcodeSpecs[];

  unsigned short fMaxStack;
  unsigned short fMaxLocals;
  string fCode;
  ExceptionTable fExceptionTable;
  CJavaLineNumberTable* fLineNumberTable;
  CJavaLocalVariableTable* fLocalVariableTable;
};

//
//  Class name : CJavaLineNumberTable
//  Description : This is the attribute used in a code attribute node to
//    store the line number table to convert bytecode instruction to source
//    code line.
//
class CJavaLineNumberTable : public CJavaAttribute {
  DynamicCastDeclarations;
public:
  static CJavaLineNumberTable* ParseBuffer(string::const_iterator& buffer);
  virtual ~CJavaLineNumberTable();
  string Compile(CJavaClassFile& inClass) const;

  typedef struct {
    unsigned short fStartPC;
    unsigned short fLineNumber;
  } LineInfo;
  typedef vector<LineInfo> LineVector;
  CJavaLineNumberTable(const LineVector& lineInfo);
  LineVector::const_iterator GetLinesBegin() const { return fTable.begin(); }
  LineVector::const_iterator GetLinesEnd() const { return fTable.end(); }
  unsigned long ClosestLineNumber(unsigned long toProgramCounter) const;

protected:
  friend class CCompiler;
  CJavaLineNumberTable() {}
private:
  LineVector fTable;
};


//
//  Class name : CJavaLocalVariableTable
//  Description : This is the attribute used in a code attribute node to
//    store the information needed to decode the name and signature of a
//    local variable at run-time.
//
class CJavaLocalVariableTable : public CJavaAttribute {
  DynamicCastDeclarations;
public:
  static CJavaLocalVariableTable* ParseBuffer(string::const_iterator& buffer);
  virtual ~CJavaLocalVariableTable();
  string Compile(CJavaClassFile& inClass) const;

  typedef struct {
    unsigned short fStartPC;
    unsigned short fLength;
    unsigned short fNameIndex;
    unsigned short fSignatureIndex;
    unsigned short fSlot;
  } VariableInfo;

protected:
  friend class CCompiler;
  CJavaLocalVariableTable() {}
private:
  vector<VariableInfo> fTable;
};

#endif
