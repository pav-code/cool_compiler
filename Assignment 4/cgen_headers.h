===============================================================================
============================== cgen.h =========================================
===============================================================================

#include <assert.h>
#include <stdio.h>
#include <stack>
#include <list>
#include <algorithm>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   std::stack<CgenNodeP> mystack;
   SymbolTable<Symbol,int>Environment;
   pCon context;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
   void code_class_nametab();
   void code_class_objtab();
   void code_dispatch_tab();
   void code_obj_proto();
   void code_obj_init();
   void code_method_defs();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

===============================================================================
============================== cgen_gc.h ======================================
===============================================================================
//
// See copyright.h for copyright notice and limitation of liability
// and disclaimer of warranty provisions.
//
#include "copyright.h"

//
// Garbage collection options
//

extern enum Memmgr { GC_NOGC, GC_GENGC, GC_SNCGC } cgen_Memmgr;

extern enum Memmgr_Test { GC_NORMAL, GC_TEST } cgen_Memmgr_Test;

extern enum Memmgr_Debug { GC_QUICK, GC_DEBUG } cgen_Memmgr_Debug;

===============================================================================
================================ emit.h =======================================
===============================================================================

///////////////////////////////////////////////////////////////////////
//
//  Assembly Code Naming Conventions:
//
//     Dispatch table            <classname>_dispTab
//     Method entry point        <classname>.<method>
//     Class init code           <classname>_init
//     Abort method entry        <classname>.<method>.Abort
//     Prototype object          <classname>_protObj
//     Integer constant          int_const<Symbol>
//     String constant           str_const<Symbol>
//
///////////////////////////////////////////////////////////////////////

#include "stringtab.h"

#define MAXINT  100000000    
#define WORD_SIZE    4
#define LOG_WORD_SIZE 2     // for logical shifts

// Global names
#define CLASSNAMETAB         "class_nameTab"
#define CLASSOBJTAB          "class_objTab"
#define INTTAG               "_int_tag"
#define BOOLTAG              "_bool_tag"
#define STRINGTAG            "_string_tag"
#define HEAP_START           "heap_start"

// Naming conventions
#define DISPTAB_SUFFIX       "_dispTab"
#define METHOD_SEP           "."
#define CLASSINIT_SUFFIX     "_init"
#define PROTOBJ_SUFFIX       "_protObj"
#define OBJECTPROTOBJ        "Object"PROTOBJ_SUFFIX
#define INTCONST_PREFIX      "int_const"
#define STRCONST_PREFIX      "str_const"
#define BOOLCONST_PREFIX     "bool_const"


#define EMPTYSLOT            0
#define LABEL                ":\n"

#define STRINGNAME (char *) "String"
#define INTNAME    (char *) "Int"
#define BOOLNAME   (char *) "Bool"
#define MAINNAME   (char *) "Main"

//
// information about object headers
//
#define DEFAULT_OBJFIELDS 3
#define TAG_OFFSET 0
#define SIZE_OFFSET 1
#define DISPTABLE_OFFSET 2

#define STRING_SLOTS      1
#define INT_SLOTS         1
#define BOOL_SLOTS        1

#define GLOBAL        "\t.globl\t"
#define ALIGN         "\t.align\t2\n"
#define WORD          "\t.word\t"

//
// register names
//
#define ZERO "$zero"		// Zero register 
#define ACC  "$a0"		// Accumulator 
#define A1   "$a1"		// For arguments to prim funcs 
#define SELF "$s0"		// Ptr to self (callee saves) 
#define T1   "$t1"		// Temporary 1 
#define T2   "$t2"		// Temporary 2 
#define T3   "$t3"		// Temporary 3 
#define SP   "$sp"		// Stack pointer 
#define FP   "$fp"		// Frame pointer 
#define RA   "$ra"		// Return address 

//
// Opcodes
//
#define JALR  "\tjalr\t"  
#define JAL   "\tjal\t"                 
#define RET   "\tjr\t"RA"\t"

#define SW    "\tsw\t"
#define LW    "\tlw\t"
#define LI    "\tli\t"
#define LA    "\tla\t"

#define MOVE  "\tmove\t"
#define NEG   "\tneg\t"
#define ADD   "\tadd\t"
#define ADDI  "\taddi\t"
#define ADDU  "\taddu\t"
#define ADDIU "\taddiu\t"
#define DIV   "\tdiv\t"
#define MUL   "\tmul\t"
#define SUB   "\tsub\t"
#define SLL   "\tsll\t"
#define BEQZ  "\tbeqz\t"
#define BRANCH   "\tb\t"
#define BEQ      "\tbeq\t"
#define BNE      "\tbne\t"
#define BLEQ     "\tble\t"
#define BLT      "\tblt\t"
#define BGT      "\tbgt\t"

===============================================================================
============================== cgen_supp.cc ===================================
===============================================================================

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "stringtab.h"

static int ascii = 0;

void ascii_mode(ostream& str)
{
  if (!ascii) 
    {
      str << "\t.ascii\t\"";
      ascii = 1;
    } 
}

void byte_mode(ostream& str)
{
  if (ascii) 
    {
      str << "\"\n";
      ascii = 0;
    }
}

void emit_string_constant(ostream& str, char* s)
{
  ascii = 0;

  while (*s) {
    switch (*s) {
    case '\n':
      ascii_mode(str);
      str << "\\n";
      break;
    case '\t':
      ascii_mode(str);
      str << "\\t";
      break;
    case '\\':
      byte_mode(str);
      str << "\t.byte\t" << (int) ((unsigned char) '\\') << endl;
      break;
    case '"' :
      ascii_mode(str);
      str << "\\\"";
      break;
    default:
      if (*s >= ' ' && ((unsigned char) *s) < 128) 
	{
	  ascii_mode(str);
	  str << *s;
	}
      else 
	{
	  byte_mode(str);
	  str << "\t.byte\t" << (int) ((unsigned char) *s) << endl;
	}
      break;
    }
    s++;
  }
  byte_mode(str);
  str << "\t.byte\t0\t" << endl;
}

===============================================================================
============================== cgen_phase.cc ==================================
===============================================================================

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "cool-io.h"  //includes iostream
#include "cool-tree.h"
#include "cgen_gc.h"

extern int optind;            // for option processing
extern char *out_filename;    // name of output assembly
extern Program ast_root;             // root of the abstract syntax tree
FILE *ast_file = stdin;       // we read the AST from standard input
extern int ast_yyparse(void); // entry point to the AST parser

int cool_yydebug;     // not used, but needed to link with handle_flags
int curr_lineno;
char *curr_filename;

void handle_flags(int argc, char *argv[]);

int main(int argc, char *argv[]) {
  int firstfile_index;

  handle_flags(argc,argv);
  firstfile_index = optind;

  if (!out_filename && optind < argc) {   // no -o option
      char *dot = strrchr(argv[optind], '.');
      if (dot) *dot = '\0'; // strip off file extension
      out_filename = new char[strlen(argv[optind])+8];
      strcpy(out_filename, argv[optind]);
      strcat(out_filename, ".s");
  }

  // 
  // Don't touch the output file until we know that earlier phases of the
  // compiler have succeeded.
  //
  ast_yyparse();

  if (out_filename) {
      ofstream s(out_filename);
      if (!s) {
	  cerr << "Cannot open output file " << out_filename << endl;
	  exit(1);
      }
      ast_root->cgen(s);
  } else {
      ast_root->cgen(cout);
  }
}

===============================================================================
============================= handle_flags.cc =================================
===============================================================================

//
// See copyright.h for copyright notice and limitation of liability
// and disclaimer of warranty provisions.
//
#include "copyright.h"

#include <stdio.h>
#include <stdlib.h>
#include "cool-io.h"
#include <unistd.h>
#include "cgen_gc.h"

//
// coolc provides a debugging switch for each phase of the compiler,
// switches to control garbage collection policy, and a switch to enable 
// optimization.  The optimization flag is ignored by the reference compiler.
//
// All flags that can be set on the command line should be defined here;
// otherwise, it is necessary to pollute test drivers for components of the
// compiler with declarations of extern'ed debugging flags to satisfy the
// linker.  The exceptions to this rule are yy_flex_debug and cool_yydebug, 
// which are defined in files generated by flex and bison.
//

extern int yy_flex_debug;       // for the lexer; prints recognized rules
extern int cool_yydebug;        // for the parser
       int lex_verbose;         // also for the lexer; prints tokens
       int semant_debug;        // for semantic analysis
       int cgen_debug;          // for code gen
       bool disable_reg_alloc;  // Don't do register allocation

       int cgen_optimize;       // optimize switch for code generator 
       char *out_filename;      // file name for generated code
       Memmgr cgen_Memmgr = GC_NOGC;      // enable/disable garbage collection
       Memmgr_Test cgen_Memmgr_Test = GC_NORMAL;  // normal/test GC
       Memmgr_Debug cgen_Memmgr_Debug = GC_QUICK; // check heap frequently

// used for option processing (man 3 getopt for more info)
extern int optind, opterr;
extern char *optarg;

void handle_flags(int argc, char *argv[]) {
  int c;
  int unknownopt = 0;

  // no debugging or optimization by default
  yy_flex_debug = 0;
  cool_yydebug = 0;
  lex_verbose  = 0;
  semant_debug = 0;
  cgen_debug = 0;
  cgen_optimize = 0;
  disable_reg_alloc = 0;
  

  while ((c = getopt(argc, argv, "lpscvrOo:gtT")) != -1) {
    switch (c) {
#ifdef DEBUG
    case 'l':
      yy_flex_debug = 1;
      break;
    case 'p':
      cool_yydebug = 1;
      break;
    case 's':
      semant_debug = 1;
      break;
    case 'c':
      cgen_debug = 1;
      break;
    case 'v':
      lex_verbose = 1;
      break;
    case 'r':
      disable_reg_alloc = 1;
      break;
#else
    case 'l':
    case 'p':
    case 's':
    case 'c': 
    case 'v':
    case 'r':
      cerr << "No debugging available\n";
      break;
#endif
    case 'g':  // enable garbage collection
      cgen_Memmgr = GC_GENGC;
      break;
    case 't':  // run garbage collection very frequently (on every allocation)
      cgen_Memmgr_Test = GC_TEST;
      break;
    case 'T':  // do even more pedantic tests in garbage collection
      cgen_Memmgr_Debug = GC_DEBUG;
      break;
    case 'o':  // set the name of the output file
      out_filename = optarg;
      break;
    case 'O':  // enable optimization
      cgen_optimize = 1;
      break;
    case '?':
      unknownopt = 1;
      break;
    case ':':
      unknownopt = 1;
      break;
    }
  }

  if (unknownopt) {
      cerr << "usage: " << argv[0] << 
#ifdef DEBUG
	  " [-lvpscOgtTr -o outname] [input-files]\n";
#else
      " [-OgtT -o outname] [input-files]\n";
#endif
      exit(1);
  }

}
