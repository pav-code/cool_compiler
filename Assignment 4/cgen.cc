
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bnez(char *source, int label, ostream &s)
{
  s << BNEZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Pop a the stack into a register
//
static void emit_pop(char *dest, ostream& str)
{
  emit_addiu(SP,SP,4,str);
  emit_load(dest,0,SP,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;

 /***** Add dispatch information for class String ******/
      emit_disptable_ref(idtable.lookup_string("String"), s);
 
      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/
      emit_disptable_ref(idtable.lookup_string("Int"), s);      
 
      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/
      emit_disptable_ref(idtable.lookup_string("Bool"), s);
 
      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  Context methods
//
//////////////////////////////////////////////////////////////////////////////
Context::Context() 
{
  current_class = NULL;
  Environment =  new SymbolTable<Symbol, SymbolTable< Symbol, int> >();
  FormalEnv =    new SymbolTable<Symbol, int>();
  LetEnv =       new SymbolTable<Symbol, int>();
  CaseEnv =      new SymbolTable<Symbol, int>();
  CaseChildren = new SymbolTable<Symbol, std::list<Symbol> >();
  CaseIndexes =  new SymbolTable<Symbol, std::list<int> >();
  Impl =         new SymbolTable<Symbol, SymbolTable< Symbol, int> >();
  ClassTags =    new SymbolTable<Symbol, int>();

  letPosition = 1;
}

void Context::set_current_class(Symbol setClass) 
{
  current_class = setClass;
}

Symbol Context::get_current_class()
{
  return (current_class);
}

void Context::set_no_expr(bool set_no_expr)
{
  no_expr = set_no_expr;
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_setup_context()
{
  CgenNodeP currNode, ancNode;
  int position = 3;
  Feature pFeature;
  std::stack<CgenNodeP>ancestors;
  SymbolTable<Symbol, int>*classAttrMap;
  context->Environment->enterscope();
  
  ////////////////////////////////////////////
  // Set up the Environment of the context  //
  ////////////////////////////////////////////
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    mystack.push(l->hd());
  }
  
  while (!mystack.empty()) {
    currNode = mystack.top();
    mystack.pop();
    
    ancNode = currNode;
    classAttrMap = new SymbolTable<Symbol, int>();
    classAttrMap->enterscope();
    
    while (ancNode->get_name() != No_class ) {
      ancestors.push(ancNode);
      ancNode = ancNode->get_parentnd();
    }

    while (!ancestors.empty()){
      ancNode = ancestors.top();
      ancestors.pop();
      
      for(int i = ancNode->features->first(); ancNode->features->more(i); i = ancNode->features->next(i)) {
        pFeature = ancNode->features->nth(i);
        if (pFeature->get_proto() != NULL) {    
          classAttrMap->addid(pFeature->get_name(), new int (position));
          position++;
        }
      }   
    }
    context->Environment->addid(currNode->get_name(), classAttrMap);
    position = 3;
  }
  
  /////////////////////////////////////////////////
  // Set up the Environment for Case Statements  //
  /////////////////////////////////////////////////
  build_case_tree();
}

void CgenClassTable::code_obj_init()
{
  //
  // CgenClassTable::code_obj_init()
  // V)  move self object from previous function ($a0) into this self ($s0) 
  //     [$s0 is preserved across function calls]
  // VI) move the resulting self object into accumulator $s0 -> $a0
  //
  // *Note: must construct context's Environment first (variables can be used before being declared)
  
  CgenNodeP currNode, parNode, ancNode;
  std::stack<CgenNodeP>ancestors;
  Feature pFeature;

  
  ////////////////////////////////////////////
  // Generate Code for [Attr]s              //
  ////////////////////////////////////////////
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    mystack.push(l->hd());
  }
  
  while (!mystack.empty()) {
    currNode = mystack.top();
    mystack.pop();
    
    emit_init_ref(currNode->get_name(), str); str << LABEL;
    emit_push( FP, str );   /* I) push old $fp */
    emit_push( SELF, str ); /* II) push 'self' object */    
    emit_push( RA, str );   /* III) push return address */   

    emit_addiu( FP, SP, 4, str); /* IV) FP must point to top of frame (return address) */
    emit_move( SELF, ACC, str ); /* V) */
    
    /* code for parent's label call */
    parNode = currNode->get_parentnd();
    if ( parNode->get_name() != No_class ) {
      str << JAL << parNode->get_name()->get_string() << CLASSINIT_SUFFIX << endl;
    }
    
    /* code for any [attr] initializations */ 
    
    ancNode = currNode;

    context->set_current_class(currNode->get_name());
    
//    while (ancNode->get_name() != No_class ) {
//      ancestors.push(ancNode);
//      ancNode = ancNode->get_parentnd();
//    }

//    while (!ancestors.empty()){
//      ancNode = ancestors.top();
//      ancestors.pop();
      
      for(int i = ancNode->features->first(); ancNode->features->more(i); i = ancNode->features->next(i)) {
        pFeature = ancNode->features->nth(i);
        context->set_no_expr(false);
        if (pFeature->get_proto() != NULL) {
          //we have an [attr]: 1) push name(symbol) 2) increase position
           //QRcout << "class: " << currNode->get_name()->get_string()\
           << "[attr]:" << pFeature->get_name()->get_string() << endl;
           
          pFeature->get_init()->code(str, context);
          // result has to be in ACC
          if ( !context->is_no_expr() ) {
            int* p_attr = context->Environment->lookup(context->get_current_class())\
                          ->lookup(pFeature->get_name());
            emit_store( ACC, *p_attr, SELF, str );
          }
        }
      }
      //Environment is built (we have variable locations for current class)
      
//    }

    emit_move( ACC, SELF, str); /* VI) */

    emit_pop( RA, str );  /* */
    emit_pop( SELF, str ); /* */
    emit_pop( FP, str );   /* */
    
    emit_return( str ); /* */

  }
  //for (List<CgenNode> *l = nds; l; l = l->tl()) {
  //  cout << l->hd()->get_name()->get_string()<<endl;
  //  context->Environment->lookup(l->hd()->get_name())->dump();
  //}
}

void CgenClassTable::code_method_defs()
{
  //
  //
  //
  CgenNodeP currNode;
  Feature pFeature;
  Formal pFormal;
  Symbol nodeName;
  int numArgs, argInAR;
  //SymbolTable<Symbol, int>*classFormalMap; /* Following the standard attr addition */

  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    nodeName = l->hd()->get_name();
    if ( nodeName != Object &&
         nodeName != IO     &&
         nodeName != Int    &&
         nodeName != Bool   && 
         nodeName != Str    ) {
      mystack.push(l->hd());
    }
  }
  
  while (!mystack.empty()) {
    currNode = mystack.top();
    mystack.pop();
    
    context->set_current_class(currNode->get_name());
    //classFormalMap = new SymbolTable<Symbol, int>();
    //classFormalMap->enterscope();
    
    for(int i = currNode->features->first(); currNode->features->more(i); i = currNode->features->next(i)) {
      // name, formals, return_type, expr
      //  ID( formal* ) : TYPE { expr }
      pFeature = currNode->features->nth(i);
      if ( pFeature->get_disp() ) {     
        emit_method_ref( currNode->get_name(), pFeature->get_name(), str );
        str << ":" << endl;
        
        emit_push( FP, str );   /* I) push old $fp */
        emit_push( SELF, str ); /* II) push 'self' object */    
        emit_push( RA, str );   /* III) push return address */ 
        
        emit_addiu( FP, SP, 4, str); /* IV) FP must point to top of frame (return address) */
        emit_move( SELF, ACC, str ); /* V) */
        
        //emit code for formals loading here (SymbolTable->enterscope() into additions)
        context->FormalEnv = new SymbolTable<Symbol, int>();
        context->FormalEnv->enterscope();
        
        numArgs = 0;
        for (int i = pFeature->get_formals()->first(); 
             pFeature->get_formals()->more(i); 
             i = pFeature->get_formals()->next(i)) {
          numArgs++;
        }
        
        argInAR = numArgs;
        
        for (int i = pFeature->get_formals()->first(); 
             pFeature->get_formals()->more(i); 
             i = pFeature->get_formals()->next(i)) {
          pFormal = pFeature->get_formals()->nth(i);
          context->FormalEnv->addid( pFormal->get_name(), new int ( numArgs + 2 ) );
          numArgs--;
        }   

        //classInnerMap->addid( true, classFormalMap );
        //context->Environment->addid( currNode->get_name(), classInnerMap );
        
        // NOTE: CANNOT add another symbol under the same NODE (but each node has
        //       formals and attributes so we need another way to do this!
        // must then change all exprs that produce code for IDs (assign...)
        pFeature->get_expr()->code( str, context );
        
        // emit_move( ACC, SELF, str); /* VI) */ Seems we do not need this 
        // $a0 remains the last value of the final expression in the method body

        emit_pop( RA, str );  /* */
        emit_pop( SELF, str ); /* */
        emit_pop( FP, str );   /* */
    
        if ( argInAR != 0 ) {
          emit_addiu( SP, SP, argInAR*WORD_SIZE, str );
        }
    
        emit_return( str ); /* */
      }
    }
  }
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::code_class_nametab()
{
  //
  // Construct class name table
  // Uses a stack to reverse the nds order, stack is empty @ end
  // Uses StringEntry's 'code_ref' to emit string names
  // Also uses stringtable to find the nds given string
  //
  StringEntryP strEnt = NULL;
  str << CLASSNAMETAB << LABEL;
  
  for(List<CgenNode> *l = nds; l; l = l->tl()) {
    mystack.push(l->hd());
  }
  while (!mystack.empty()) {    
    strEnt = stringtable.lookup_string(
               mystack.top()->get_name()->get_string() );
    str << WORD; strEnt->code_ref(str);
    str << endl;
    mystack.pop();
  }
}

void CgenClassTable::code_class_objtab()
{
  //
  // Constuct the class object table
  //
  Symbol sym = NULL;
  str << CLASSOBJTAB << LABEL;
  
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    mystack.push(l->hd());
  }
  while (!mystack.empty()) {
    sym = stringtable.lookup_string(
	           mystack.top()->get_name()->get_string() );
    mystack.pop();
	
    str << WORD; emit_protobj_ref(sym, str); str << endl;
    str << WORD; emit_init_ref(sym, str);    str << endl;
  }
}

void CgenClassTable::code_dispatch_tab()
{
  //
  // Construct the dispatch tables
  // 
  // 1) Dump all classes (declared and basic) into 'mystack'
  // 2) Go through 'mystack'
  //  2a) -set the current display node 'dispNode' to top of stack
  //      -clear 'overload' list : used for looping over ancestors
  //      -clear 'method_overloads' : used to place overloaded method NAMES
  //  2b) -start: dispNode end: top ancestor (child->parent)
  //       stacking all 'dispNode's parents onto 'ancestors' stack
  //  2c) -copy all 'ancestors' into 'overload' list. 
  //   3) Go through 'ancestors'
  //     3a) -'currNode' : top of ancestors stack
  //     4) Go through 'currNode' [methods] ['sym' holds method]
  //       5) Go through 'overload' [methods] ['symOverload' holds method]
  //         6) Go through 'method_overloads'
  //           6a) -if we find 'sym' in the overloaded methods for this
  //                object, 'currNode', then we do not emit an entry.
  //         5a) -if we are allowed to print AND 'sym' = 'symOverload'
  //              means: the method we aiming to print ('sym'): from 'dispNode's
  //              greatest->dispNode. And 'symOverload' from the ancestor classes
  //              dispNode->object match. 
  //              note*: remember we are going down from child->object in 'symOverload'
  //              while in 'sym' we do object->child. What this does is it prints
  //              greatest overloaded method first. 
  //         5b) -we add the method into 'method_overloads' so it is never emited
  //              again into the _dispTab of this particular object class.
  //
  
  CgenNodeP pNode, dispNode, currNode;
  Symbol sym, symOverload;
  Feature pFeature, pFeatureOverload;
  bool printed = false;
  std::stack<CgenNodeP>ancestors;
  std::stack<CgenNodeP>dummy;
  std::list<CgenNodeP>overload;
  std::list<Symbol>method_overloads;
  
  SymbolTable<Symbol, int>*classMethodMap;
  context->Impl->enterscope();
  int position = 0;
  
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    mystack.push(l->hd());
  }
  while (!mystack.empty()) {
    dispNode = mystack.top();
    pNode    = mystack.top();
    mystack.pop();
    overload.clear();
    method_overloads.clear();

    classMethodMap = new SymbolTable<Symbol, int>();
    classMethodMap->enterscope();
	
    emit_disptable_ref(dispNode->get_name(), str); str << LABEL;
      
    while ( pNode->get_name() != No_class ) {
      ancestors.push(pNode);
      dummy.push(pNode);
      pNode = pNode->get_parentnd();
    }
    while (!dummy.empty()) {
      overload.push_front(dummy.top());
      dummy.pop();
    }
	
    while (!ancestors.empty()) {
      currNode = ancestors.top();
      ancestors.pop();

      for(int i = currNode->features->first(); currNode->features->more(i); i = currNode->features->next(i)) {
        printed  = false;
        pFeature = currNode->features->nth(i);
        sym = pFeature->get_disp();
        if ( sym != NULL ) {
          for (std::list<CgenNodeP>::iterator it=overload.begin(); it != overload.end(); ++it ) { 
            for(int j = (*it)->features->first(); (*it)->features->more(j); j = (*it)->features->next(j)) {         
              pFeatureOverload = (*it)->features->nth(j);
              symOverload = pFeatureOverload->get_disp();              
              if ( symOverload != NULL ) {
                for ( std::list<Symbol>::iterator ovFind = method_overloads.begin(); 
                      ovFind != method_overloads.end(); 
                      ovFind++ ) {
                  if ((*ovFind) == sym ) {
                    printed = true;
                  }
                }
                if (!printed && sym == symOverload) {
                  str << WORD; emit_method_ref((*it)->get_name(), sym, str); str << endl;
                  printed = true;
                  if ((*it)->get_name() != currNode->get_name()) {
                     method_overloads.push_front(sym);
                  }
                  classMethodMap->addid( sym, new int (position) );
                  //cout << "Method Name: " << sym->get_string() << " position: " << position << endl;
                  position++;
     
                }
              }
            }
          } 
        }
      }
    }
    //cout << "Class Name: " << dispNode->get_name()->get_string() << endl;
    context->Impl->addid( dispNode->get_name(), classMethodMap );
    position = 0;
  }  

}

void CgenClassTable::code_obj_proto()
{
//
// Construct Object Prototypes
// 
// 1) 'mystack' is filled with all classes
// 2) go through 'mystack'
//  2a) currNode = top of 'mystack'
//  2b) gather ancestors of 'currNode' into: 'ancestors' stack
//   3) loop through all [attr]s of 'currNode' (inc. hierarchy) starting from
//      Object->currNode.
//    3a) push them all into 'listType'
//    3b) increment 'size' (object size)
// 4) emit '-1'
// 5) emit 'size' + 3 (tag, size, dispTab)
// 6) emit dispTab reference
// 7) loop through 'listType'
//   7a) if String: emit the "" string reference
//   7b) if Int: emit the "0" int reference
//   7c) if Bool: emit "bool_const0" [always defined]
//   7d) else: emit "0" pointer (void)
//
  std::stack<CgenNodeP>ancestors;
  std::list<Symbol>listType;
  CgenNodeP currNode, loopNode;
  StringEntryP strEnt = NULL;
  IntEntryP intEnt = NULL;
  Feature pFeature;
  Symbol type;
  context->ClassTags->enterscope();
  int size=0, classtag=0;

  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    mystack.push(l->hd());
  }
  while (!mystack.empty()) {
    currNode = mystack.top();
    loopNode = currNode;    
    mystack.pop();
    
    context->ClassTags->addid( currNode->get_name(), new int (classtag) );
    
    while ( loopNode->get_name() != No_class ) {
      ancestors.push(loopNode);
      loopNode = loopNode->get_parentnd();
    }
  
    str << WORD << "-1" << endl;   

    emit_protobj_ref(currNode->get_name(), str); str << LABEL;
    
    while (!ancestors.empty()) {
      loopNode = ancestors.top();
      ancestors.pop();
      
      for(int i = loopNode->features->first(); loopNode->features->more(i); i = loopNode->features->next(i)) {
        pFeature = loopNode->features->nth(i);
        type = pFeature->get_proto();
        if (type != NULL) {
          listType.push_back(type);
          size++;
        }
      }
    }
    str << WORD << classtag << endl; /* tag */
    str << WORD << 3+size << endl;   /* size */
    str << WORD;   emit_disptable_ref(currNode->get_name() ,str); str << endl; /* dispTable */
    for (std::list<Symbol>::iterator it=listType.begin(); it != listType.end(); ++it ) { 
      if ( *it == Str ) {
        /* String - emit str<Index> for "" */
        strEnt = stringtable.lookup_string("");
        str << WORD; strEnt->code_ref(str); str << endl;
      } else if ( *it == Int ) {
        /* Int - emit str<Index> for "0" */
        intEnt = inttable.lookup_string("0");
        str << WORD; intEnt->code_ref(str); str << endl;
      } else if ( *it == Bool ) {
      /* Bool - emit 'bool_const0' - always defined by default [check] */
        str << WORD << "bool_const0" << endl;
      } else {
      /* All else - emit .word 0 (void)*/
         str << WORD << "0" << endl;
      }
   
    }
    size = 0;
    classtag++;
    listType.clear();
  }
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   stringclasstag = 4 /* Change to your String class tag here */;
   intclasstag =    2 /* Change to your Int class tag here */;
   boolclasstag =   3 /* Change to your Bool class tag here */;

   context = new Context(); 

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   code();

   for (int i = idtable.first(); idtable.more(i); i=idtable.next(i) ) {
     //QRcout << "#: " << i << " Entry: " << idtable.lookup(i)->get_string() << endl;
   }
//QRcout << endl;
   for (int i = stringtable.first(); stringtable.more(i); i=stringtable.next(i) ) {
     //QRcout << "#: " << i << " String: " << stringtable.lookup(i)->get_string() << endl;
     if (i == 1) {
       // emit_load_string( ACC, stringtable.lookup(i), s);
     }
   }
//QRcout << endl;
   for (int i = inttable.first(); inttable.more(i); i=inttable.next(i) ) {
     //QRcout << "#: " << i << " Int: " << inttable.lookup(i)->get_string() << endl;
   }
//QRcout << endl;

   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
	  
  for(List<CgenNode> *l = nds; l; l = l->tl())
      int i; //QRcout
      //QRcout << "nds: " << l->hd()->get_name()->get_string() << endl;
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

void CgenClassTable::build_case_tree()
{
  // context
  //
  //
  List<CgenNode> *children;
  std::list<Symbol> *child_list;
  
  context->CaseChildren->enterscope();
  
  for(List<CgenNode> *l = nds; l; l = l->tl()) {
    //int* iPrint = context->ClassTags->lookup( l->hd()->get_name() );
    //cout << "Class Name: " << l->hd()->get_name()->get_string() << endl;
    //cout << " tag: " << *iPrint << endl;
  
    children = l->hd()->get_children();
    child_list = new std::list<Symbol>;
    //QRcout << "Class Of Children: " << l->hd()->get_name()->get_string() << endl;
    for (List<CgenNode> *cl = children; cl; cl = cl->tl() ) {
      //QRcout << "  child: " << cl->hd()->get_name()->get_string() << endl;
      child_list->push_front( cl->hd()->get_name() );
    }
    context->CaseChildren->addid( l->hd()->get_name(), child_list );
  }
}

void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();
  
//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
 
  if (cgen_debug) cout << "coding class name table" << endl;
  code_class_nametab();
  
  if (cgen_debug) cout << "coding class object table" << endl;
  code_class_objtab();
  
  if (cgen_debug) cout << "coding dispatch tables" << endl;
  code_dispatch_tab();
  
  if (cgen_debug) cout << "coding object prototypes" << endl;
  code_obj_proto();

  if (cgen_debug) cout << "coding global text" << endl;
  code_global_text();

//                 Add your code to emit
//                   - object initializer
//                   - the class methods
//                   - etc...
  if (cgen_debug) cout << "constructing context" << endl;
  code_setup_context();

  if (cgen_debug) cout << "coding object initializer" << endl;
  code_obj_init();
  
  if (cgen_debug) cout << "coding method definitions" << endl;
  code_method_defs();
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &s, pCon context) {
  // name and expr
  // to assign an expression either it was:
  //  1) init before if it had type Bool, String or Int
  //  2) or it was created by the 'new' operator
  // need to load addr of object, then save it into the proper location
  
  expr->code(s,context);
  
  if ( context->CaseEnv->lookup( name ) != NULL && context->LetEnv->lookup( name ) != NULL ) {
    int* casePosition = context->CaseEnv->lookup( name );
    int* letPosition  = context->LetEnv->lookup( name );
      
    if (*casePosition < *letPosition) {
      emit_load( ACC, *casePosition, FP, s );       
    } else {
      emit_load( ACC, *letPosition, FP, s );  
    }
  } else if ( context->CaseEnv->lookup( name ) != NULL ) { /* we have a case var */
      int* position = context->CaseEnv->lookup( name );
      emit_load( ACC, *position, FP, s );
  } else if ( context->LetEnv->lookup( name ) != NULL ) { /* we have a formal var! */
    int* position = context->LetEnv->lookup( name );
    emit_store( ACC, *position, FP, s );
  } else if ( context->FormalEnv->lookup(name) != NULL  ) {
    int* position = context->FormalEnv->lookup(name);
    emit_store( ACC, *position, FP, s );
  } else {
//cout << "current class" << context->get_current_class()->get_string()\
//<< " name: " << name->get_string() << endl;
//context->Environment->lookup(context->get_current_class())->lookup(false)->dump();
    int* position = context->Environment->lookup(context->get_current_class())\
                    ->lookup(name);
    emit_store( ACC, *position, SELF, s ); 

  }

}

void static_dispatch_class::code(ostream &s, pCon context) {
  //
  // 
  // expr(expr), type_name(sym), name(sym), actual(exprs)

  Expression eN;
  int* index;
  int not_void;
  
  not_void = context->labelNum++;
  
  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    eN = actual->nth(i);
    eN->code( s, context );
    
    emit_push( ACC, s );
  }
  
  expr->code( s, context ); /* v0 = ACC */
  
  emit_bnez( ACC, not_void, s ); /* check ACC != void (0) */

  //emit_load_string(ACC,"str_const0",s); /* load filename in ACC */
  emit_partial_load_address( ACC, s ); /* load filename in ACC */
  s << STRCONST_PREFIX << 0 << endl;
  emit_load_imm( T1, 1, s ); /* load file number (1) into T1 */
  emit_jal( "_dispatch_abort", s ); /* call abort on void and leave */

  emit_label_def( not_void, s ); /* label if we didnt' call on null */   
  
//QRcout<<"[staticDispatch] @ type: " << type_name->get_string() << endl;
  index = context->Impl->lookup( type_name )->lookup( name ); /* impl(T,f) [@T] */

  //emit_load( T1, 2, ACC, s ); /* load X_dispTab (from $a0 + 8) into $t1 */  
  emit_partial_load_address( T1, s ); 
  emit_disptable_ref( type_name, s ); s << endl;
  emit_load( T1, *index, T1, s ); /* load X.f into T1 (from Environment) */
  emit_jalr( T1, s );            /* call X.f */  
}

void dispatch_class::code(ostream &s, pCon context) {
  // expr (e0), name (f), actual (e1, ... , en)
  // 1) loop through the actuals (e1->en) and evaluate each expression
  //    and push it onto the stack (value in ACC).
  // 2) evaluate e0 (in ACC)
  // 3) v0 = X(a1=l1, ... ,am=lm) : context->Environment contains this
  // 4) implementation(X,f) = (x1,...,xn, e_body) : x1->xn contained in
  //    method_class (formals). e_body is also held in the definition.
  //    This essentially calls the functions definition.
  // 5) lxi = newloc(Sn2), i=1->n : locations stored on stack in order
  //    of declarations. (ArgN is on top of stack, Arg1 is at bottom)
  // 6) Sn3 = Sn2[v1/lx1, ... ,vn/lxn] : values of formals (the values
  //    of the expressions are evaluated in 1) and pushed into stack.
  // 7) v0,Sn3,[v0's attrs, formals] |- e_body : Vn1, Sn4 : Vn1 and
  //    Sn4 are the final values of the dispatch expression. v0 is the
  //    self object used (**the evaluation object of e0!!!**)
  //
  
  Expression eN;
  int* index;
  int not_void;
  
  not_void = context->labelNum++;
  
  for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
    eN = actual->nth(i);
    eN->code( s, context );
    
    emit_push( ACC, s );
  }
  
  expr->code( s, context ); /* v0 = ACC */
  
  emit_bnez( ACC, not_void, s ); /* check ACC != void (0) */

  //emit_load_string(ACC,"str_const0",s); /* load filename in ACC */
  emit_partial_load_address( ACC, s ); /* load filename in ACC */
  s << STRCONST_PREFIX << 0 << endl;
  emit_load_imm( T1, 1, s ); /* load file number (1) into T1 */
  emit_jal( "_dispatch_abort", s ); /* call abort on void and leave */

  emit_label_def( not_void, s ); /* label if we didnt' call on null */ 
  
 //QRcout << "get_type(): " << expr->get_type()->get_string() << endl;
//cout<< "get current class: " << context->get_current_class()->get_string();
  // Think to make sure this will in fact work!
  if ( expr->get_type() == SELF_TYPE ) {
    index = context->Impl->lookup( context->get_current_class() )->lookup( name );
  } else {
    index = context->Impl->lookup( expr->get_type() )->lookup( name );
  }
 
  emit_load( T1, 2, ACC, s ); /* load X_dispTab (from $a0 + 8) into $t1 */
  emit_load( T1, *index, T1, s ); /* load X.f into T1 (from Environment) */
  emit_jalr( T1, s );            /* call X.f */
  
  
  //cout << "dispatch class f: " << name->get_string() <<\
  //" and the type!: " <<  expr->get_type()->get_string() << endl;
}

void cond_class::code(ostream &s, pCon context) {
  //
  // If statements (needs testing)
  //
  int false_b, end_if;
  pred->code( s, context );
  
  emit_load( T1, 3, ACC, s ); /* bool's 0 or 1 from ACC -> T1 */
  
  false_b = context->labelNum++;
  emit_beqz( T1, false_b, s );
  
  // True branch
  then_exp->code( s, context );
  end_if = context->labelNum++;
  emit_branch( end_if, s ); /* end_if label */
  
  // False branch
  emit_label_def( false_b, s ); /* emit false_branch label */
  else_exp->code( s, context ); 
  emit_label_def( end_if, s );  /* emit end_if label*/
   
}

void loop_class::code(ostream &s, pCon context) {
  //
  //
  // pred (Expression), body (Expression)
  
  int start_loop, end_loop;
  
  start_loop = context->labelNum++;
  end_loop   = context->labelNum++;

  emit_label_def( start_loop, s );  
  pred->code( s, context );
  
  emit_load( T1, 3, ACC, s ); /* from the boolean predicate 0 or 1 */
  
  emit_beqz( T1, end_loop, s );
  
  //True Instance
  body->code( s, context );
  emit_branch( start_loop, s );
  
  //False Instance
  emit_label_def( end_loop, s );
  emit_move( ACC, ZERO, s );
}

static void recurChildren( std::list<int>* pOutputList, Symbol node, std::list<Symbol> *pCaseList, pCon context )
{
  // arg1: pOutputList - final list of < Class, List<ints>Indexes >
  // arg2: node        - current Node being processed
  // arg3: pCaseList   - list of classes in Case expr < List<Symbol> >
  // arg4: context     - the context
  //
  // Recursive Function to construct a case-statement runtime class indexes
  // which will be used in the generated code to direct the runtime behaviour
  // of the case statement.
  //
  std::list<Symbol> *pChild_list;
  std::list<int>::iterator it;
  std::list<Symbol>::iterator it1;
  int iNode = *context->ClassTags->lookup( node );

  it = std::find(pOutputList->begin(), pOutputList->end(), iNode );
  if ( it != pOutputList->end()  ) { /* Node is in output list already: call each child */
    pChild_list = context->CaseChildren->lookup(node);
    for (std::list<Symbol>::iterator childNode = pChild_list->begin(); childNode!=pChild_list->end(); childNode++) {
      recurChildren( pOutputList, *childNode, pCaseList, context );
    }
    return;
  }

  it1 = std::find(pCaseList->begin(), pCaseList->end(), node );
  if ( it1 != pCaseList->end() ) { /* Node is in Current Case: do not call children [Terminal] */
    return;
  }

    /* It's not in Current Case, and it's not in Output List: add to Output List and call Children */
  pOutputList->push_front( iNode );
  pChild_list = context->CaseChildren->lookup(node);
  for (std::list<Symbol>::iterator childNode = pChild_list->begin(); childNode!=pChild_list->end(); childNode++) {
    recurChildren( pOutputList, *childNode, pCaseList, context );
  }
  return;  
}

void typcase_class::code(ostream &s, pCon context) {
  //
  // 2 Parts:
  //   1) Building of CaseIndexes (using recursive calls to recurChildren)
  //      it goes through every branch of case, and based on its type gathers
  //      all its ancestor nodes - in relation to the current case branches
  //      and the tree formed by the class hierarchies. 
  //   2) Emiting code for the branches
  //      -Produce code for e0
  //      -Get the runtime TAG of e0
  //      -call ::code() for each case statement
  //      -emit code runtime error on no match to case expression  
  //
  // expr(Expressions), cases (Cases)
  // so,S1,E |- case e0 of Id1:T1=>e1;...;IdN:TN=>eN; esac : v1,S4
  Case caseN;
  //std::list<Symbol> caseList;
  int after_case, void_case;
  std::list<Symbol> *pCaseList = new std::list<Symbol>();
  std::list<int> *pOutputList = new std::list<int>();
  std::list<int>::iterator it;
  int* ipCurrNode;
  
  context->CaseIndexes->enterscope();
  context->set_case_end(context->labelNum++);
 
  expr->code( s, context );
  
  /* Emit error code for branch on empty case ( ACC = 0 : _case_abort2 ) */
  void_case = context->labelNum++;
  s << "\tbnez\t" << ACC << " label" << void_case << endl;
  emit_load_address( ACC, "str_const0", s );
  emit_load_imm( T1, 1, s );
  emit_jal( "_case_abort2", s );
  
  emit_label_def( void_case, s );
  emit_load( T3, 0, ACC, s );
  
  //cout << "[case] Type of e0: " << expr->get_type()->get_string() << endl; 

  for (int i = cases->first(); cases->more(i); i = cases->next(i) ) {
    caseN = cases->nth(i); /* Gather Temp List of Current Case */
    pCaseList->push_front( caseN->get_type() );
  }  
  
  for (std::list<Symbol>::iterator it1 = pCaseList->begin(); it1!=pCaseList->end(); ++it1) {
    ipCurrNode = context->ClassTags->lookup(*it1);
    pOutputList->push_front( *ipCurrNode );
    recurChildren( pOutputList, *it1, pCaseList, context );
    context->CaseIndexes->addid( *it1, pOutputList);
    pOutputList = new std::list<int>();
  }
  

/*  cout<<"Case Indexes coming out!" << endl;
  std::list<int> *casePrint;
  std::list<int>::iterator it2;
  for (std::list<Symbol>::iterator it1 = pCaseList->begin(); it1!=pCaseList->end(); ++it1) {
    casePrint = context->CaseIndexes->lookup(*it1);
    cout << "Node: " << (*it1)->get_string() << endl;
    for (it2 = casePrint->begin(); it2!=casePrint->end(); it2++){
      cout << "  Ancestor: " << *it2 << endl;
    }
  }
*/
  /* Call each branch */
  for (int i = cases->first(); cases->more(i); i = cases->next(i) ) {
    caseN = cases->nth(i);
    caseN->code( s, context );
  }    
  
  /* Emit error for case no match: _case_abort */
  emit_jal( "_case_abort", s );
  
  emit_label_def( context->get_case_end(), s );
  
  context->CaseIndexes->exitscope();
}

void branch_class::code(ostream &s, pCon context) {
  //
  // Produces code for each branch statement
  //   -this_branch,next_branch labels for code references
  //   -emit beq on each known index in case the runtime TAG
  //    is of that type.
  //   -emit b to next branch if TAG didn't match to current branch
  //   -emit label definition for the current branch (this_branch)
  //   -emit code for branch expression (EXPR)
  //   -emit branch for exiting the branch (context->get_case_end())
  //   -emit definition for next branch (next_branch)
  // name (Symbol), type_decl (Symbol), expr (Expression)
  
  int this_branch, next_branch;
  std::list<int> *branchIndexes;
  
  this_branch = context->labelNum++; /* set next branch number */
  next_branch = context->labelNum++; /* set this branch number */ 

  branchIndexes = context->CaseIndexes->lookup( type_decl );
  
  for (std::list<int>::iterator it = branchIndexes->begin(); it!=branchIndexes->end(); ++it) {
    char cInt[40];
    sprintf( cInt, "%d", *it );
    
    emit_beq( T3, cInt, this_branch, s );
  }
  emit_branch( next_branch, s );
  
  emit_label_def( this_branch, s );
  
  /* code for <expri> bounded to e0 (ACC) */  
  context->CaseEnv->enterscope();
  context->CaseEnv->addid( name, new int (context->let_pos()) );
  context->inc_let_pos();
  emit_push( ACC, s );
  
  expr->code( s, context );
  
  emit_pop( ZERO, s );
  context->CaseEnv->exitscope();
  context->dec_let_pos();       /* - the let posisiton */
  
  emit_branch( context->get_case_end(), s );
  
  emit_label_def( next_branch, s );
  
}

void block_class::code(ostream &s, pCon context) {
  //
  // Blocks evaluated from first expr to last expr. Result is the last expr.
  //  [left in ACC]
  //  Expressions : body  
  //

  Expression eN;
  
  for (int i = body->first(); body->more(i); i = body->next(i) ) {
    eN = body->nth(i);
    eN->code( s, context );
  }
}

void let_class::code(ostream &s, pCon context) {
  //
  //
  // identifier(symbol), type_decl(symbol), init(expr), body(expr)
  // so,S1,E |- let Id : T1 <- e1 in e2 : v2, S4
  
  init->code( s, context ); /* so,S1,E |- e1:v1,S2 */
  
  if ( context->is_no_expr() ) { /* In case of an un-initialized variable */
    if ( type_decl == Str ) { /* uninit String */
      emit_load_string(ACC,stringtable.lookup_string( "" ),s);      
    } else if ( type_decl == Int ) { /* uninit Int */ 
      emit_load_int(ACC,inttable.lookup_string("0"),s);
    } else if ( type_decl == Bool ) { /* uninit Bool */
      emit_load_bool(ACC, BoolConst(0), s);
    } else { /* uninit non-primitive Object */
      emit_move(ACC, ZERO, s);
    }
    context->set_no_expr(false);
  }
  
  emit_push( ACC, s ); /* save ACC on stack since it needs to be used again */
  
  context->LetEnv->enterscope();                      /* Each nested let has it's own scope */
  context->LetEnv->addid( identifier, new int (context->let_pos()) );     /* add ID->LetEnv */
  context->inc_let_pos();                                             /* + the let position */
  
  //QRcout<<"[let],@enter, let_position: "<<context->let_pos()<<endl;

  body->code( s, context );
  
  emit_addiu( SP, SP, 4, s );   /* Pop let variable */ 
  context->LetEnv->exitscope(); /* Exit the nested scope of this let */
  context->dec_let_pos();       /* - the let posisiton */

}

void plus_class::code(ostream &s, pCon context) {
  //
  //
  // e1(Expression), e2(Expression)
  
  e1->code( s, context );
  emit_push( ACC, s);    /* e1:v1 goes onto stack */
  
  e2->code( s, context );
  
  emit_jal( "Object.copy", s );
  emit_move( T3, ACC, s );      /* put new obj in T3 */
  
  emit_load( T2, 3, ACC, s );   /* e2:v2->T2 */
  
  emit_pop( T1, s ); /* pop e1->T1 */
  emit_load( T1, 3, T1, s ); /* v1->T1 */
  
  emit_add( T1, T1, T2, s );    /* e1+e2 */
  
  emit_store( T1, 3, T3, s );  /* store val in new var */
  emit_move( ACC, T3, s );     /* move result in ACC */
}

void sub_class::code(ostream &s, pCon context) {
  //
  // [Arith]
  // e1(Expression), e2(Expression)
  
  e1->code( s, context );
  emit_push( ACC, s);    /* e1:v1 goes onto stack */
  
  e2->code( s, context );
  
  emit_jal( "Object.copy", s );
  emit_move( T3, ACC, s );      /* put new obj in T3 */
  
  emit_load( T2, 3, ACC, s );   /* e2:v2->T2 */
  
  emit_pop( T1, s ); /* pop e1->T1 */
  emit_load( T1, 3, T1, s ); /* v1->T1 */
  
  emit_sub( T1, T1, T2, s );    /* e1+e2 */
  
  emit_store( T1, 3, T3, s );  /* store val in new var */
  emit_move( ACC, T3, s );     /* move result in ACC */
}

void mul_class::code(ostream &s, pCon context) {
  //
  // [Arith]
  // e1(Expression), e2(Expression)
  
  e1->code( s, context );
  emit_push( ACC, s);    /* e1:v1 goes onto stack */
  
  e2->code( s, context );
  
  emit_jal( "Object.copy", s );
  emit_move( T3, ACC, s );      /* put new obj in T3 */
  
  emit_load( T2, 3, ACC, s );   /* e2:v2->T2 */
  
  emit_pop( T1, s ); /* pop e1->T1 */
  emit_load( T1, 3, T1, s ); /* v1->T1 */
  
  emit_mul( T1, T1, T2, s );    /* e1+e2 */
  
  emit_store( T1, 3, T3, s );  /* store val in new var */
  emit_move( ACC, T3, s );     /* move result in ACC */
}

void divide_class::code(ostream &s, pCon context) {  
  //
  // [Arith]
  // e1(Expression), e2(Expression)
  
  e1->code( s, context );
  emit_push( ACC, s);    /* e1:v1 goes onto stack */
  
  e2->code( s, context );
  
  emit_jal( "Object.copy", s );
  emit_move( T3, ACC, s );      /* put new obj in T3 */
  
  emit_load( T2, 3, ACC, s );   /* e2:v2->T2 */
  
  emit_pop( T1, s ); /* pop e1->T1 */
  emit_load( T1, 3, T1, s ); /* v1->T1 */
  
  emit_div( T1, T1, T2, s );    /* e1+e2 */
  
  emit_store( T1, 3, T3, s );  /* store val in new var */
  emit_move( ACC, T3, s );     /* move result in ACC */
}

void neg_class::code(ostream &s, pCon context) {
  //
  //
  // e1 (Expression)
  
  e1->code( s, context );
  //emit_load( "$s1", 3, ACC, s );

  //emit_load_address( ACC, "Int_protObj", s );
  emit_jal( "Object.copy", s );
  emit_load( T1, 3, ACC, s );
  emit_neg( T1, T1, s );
  emit_store( T1, 3, ACC, s );
  
}

void lt_class::code(ostream &s, pCon context) { 
  //
  // Part of [Comp] in 13.4 from cool_manual.pdf
  // e1 (Expression), e2 (Expression)
  int true_ins, end_comp;
  
  true_ins  = context->labelNum++;
  end_comp  = context->labelNum++; 
  
  e1->code( s, context );
  //emit_load( T1, 3, ACC, s );
  emit_push( ACC, s );       //new
  
  e2->code( s, context );
  emit_load( T2, 3, ACC, s );
  
  emit_pop( T1, s );         //new
  emit_load( T1, 3, T1, s ); //new
  
  emit_blt( T1, T2, true_ins, s ); /* Branch on less than */

  //False Instance
  emit_load_address( ACC, "bool_const0", s ); 
  emit_branch( end_comp, s);

  emit_label_def( true_ins, s );
  emit_load_address( ACC, "bool_const1", s );
  
  emit_label_def( end_comp, s );
}

void eq_class::code(ostream &s, pCon context) { 
  //
  // 1) Evaluate e1 and then e2
  //   2) if pointers are the same then they equal
  //      *void is not equal to any object except itself
  //   3) if String, Bool or Int their values are compared
  // 
  //  equality_test: - Input two objects in $t1 and $t2
  //                   Output initial value of $a0 if equal
  //                          initial value of $a1 if not
  //
  // e1(Expression), e2(Expression)
  int end_comp;
  
  end_comp = context->labelNum++;   
    
  e1->code( s, context );
  emit_push( ACC, s );
  
  e2->code( s, context );
  emit_move( T2, ACC, s );  /* T2 <- e1 */
  emit_pop( T1, s );        /* T1 <- e2 */
  
  //Test on non Basic types
  emit_load_address( ACC, "bool_const1", s ); /* load TRUE in $a0 */
  emit_beq( T1, T2, end_comp, s );   /* if they are equal exit cmp with TRUE in ACC */ 

  emit_load_address( A1, "bool_const0", s );  /* load FALSE in $a1 */
  
  emit_jal( "equality_test", s ); /* call cmp routine */
 
  //End of [cmp]
  emit_label_def( end_comp, s );
}

void leq_class::code(ostream &s, pCon context) {
  //
  // Part of [Comp] in 13.4 from cool_manual.pdf
  // e1 (Expression), e2 (Expression)
  int true_ins, end_comp;
  
  true_ins  = context->labelNum++;
  end_comp  = context->labelNum++; 
  
  e1->code( s, context );
  //emit_load( T1, 3, ACC, s );
  emit_push( ACC, s );        //new
  
  e2->code( s, context );
  emit_load( T2, 3, ACC, s );
  
  emit_pop( T1, s );         //new
  emit_load( T1, 3, T1, s ); //new
  
  emit_bleq( T1, T2, true_ins, s ); /* Branch on less than or equal to */

  //False Instance
  emit_load_address( ACC, "bool_const0", s ); 
  emit_branch( end_comp, s);

  emit_label_def( true_ins, s );
  emit_load_address( ACC, "bool_const1", s );
  
  emit_label_def( end_comp, s );
}

void comp_class::code(ostream &s, pCon context) {
  //
  //
  // e1 (Expression)
  int false_ins, end_not;
  
  false_ins = context->labelNum++;
  end_not   = context->labelNum++;  
  
  e1->code( s, context );
  
  emit_load( T1, 3, ACC, s ); 
  emit_beqz( T1, false_ins, s );
  
  //True Instance
  emit_load_address( ACC, "bool_const0", s ); 
  emit_branch( end_not, s);
  
  //False Instance
  emit_label_def( false_ins, s );
  emit_load_address( ACC, "bool_const1", s );   
  
  emit_label_def( end_not, s );
}

void int_const_class::code(ostream& s, pCon context)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, pCon context)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, pCon context)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, pCon context) {
//
// Symbol: type_name
//  *Note: Uses current_class (must be set by code_obj_init() and code_method_def() )
// 
//
//
  Symbol currClass;

  if ( type_name == SELF_TYPE ) {
    //currClass = context->get_current_class();
    // look @ tag of self object (S0)
    // load class_objTab, multiply (tag*2)*WordSize = _protObj
    //  and tag*2 + 1 = _init
    emit_load_address( T1, "class_objTab", s );
    
    emit_load( T2, 0, SELF, s );
    emit_sll( T2, T2, 3, s ); /* multiply by 8 (4(wordsize) * 2(index in table) */
    
    emit_addu( T1, T1, T2, s );
    
    emit_push( T1, s );
    
    emit_load( ACC, 0, T1, s );
    emit_jal( "Object.copy", s );
    
    emit_pop( T1, s );
    //emit_addiu( T1, T1, 4, s );
    emit_load( T1, 1, T1, s );
    emit_jalr( T1, s );
    
  } else {
    currClass = type_name;
    emit_partial_load_address( ACC, s );
    emit_protobj_ref( currClass, s); s << endl;
  
    emit_jal( "Object.copy", s );
  
    s << JAL;
    emit_init_ref(currClass, s); s << endl;
  }
}

void isvoid_class::code(ostream &s, pCon context) {
  //
  //
  // e1 (Expression)
  int true_isvoid, end_isvoid;
  
  true_isvoid = context->labelNum++;
  end_isvoid   = context->labelNum++;  
  
  e1->code( s, context );
  
  emit_beqz( ACC, true_isvoid, s );
  
  // False Instance
  emit_load_address( ACC, "bool_const0", s ); /* load BOOL False */
  emit_branch( end_isvoid, s );
  
  // True Instance
  emit_label_def( true_isvoid, s );
  emit_load_address( ACC, "bool_const1", s); /* load BOOL True */
  
  emit_label_def( end_isvoid, s ); 
}

void no_expr_class::code(ostream &s, pCon context) {
  context->set_no_expr(true);
}

void object_class::code(ostream &s, pCon context) { 
  // Must find object in environment (it's position in SELF) then load value (from store)
  //  into the ACC
  // If self, must return the self object (in ACC)
  // Must then check if we are emitting a Formal or an Attribute
  // Note: We are giving Case's priority over Let, Formals, and Environment! Meaning
  //       that a Case's variable hides ANY definition of any nested Lets!! That is
  //       probably not as intended by COOL but I cannot see a way to properly nest
  //       these variable declarations.

  if ( name == self ) {
    emit_move( ACC, SELF, s );
  } else {
  //Note: Changing the formals environment!!
    if ( context->CaseEnv->lookup( name ) != NULL && context->LetEnv->lookup( name ) != NULL ) {
      int* casePosition = context->CaseEnv->lookup( name );
      int* letPosition  = context->LetEnv->lookup( name );
      
      if (*casePosition < *letPosition) {
        emit_load( ACC, *casePosition, FP, s );       
      } else {
        emit_load( ACC, *letPosition, FP, s );  
      }
    } else if ( context->CaseEnv->lookup( name ) != NULL ) { /* we have a case var */
      int* position = context->CaseEnv->lookup( name );
      emit_load( ACC, *position, FP, s );
    } else if ( context->LetEnv->lookup( name ) != NULL ) { /* we have a let var! */
      int* position = context->LetEnv->lookup( name );
      emit_load( ACC, *position, FP, s );
    } else if ( context->FormalEnv->lookup( name ) != NULL ) { /* we have a formal! */
      int* position = context->FormalEnv->lookup( name );
      emit_load( ACC, *position, FP, s );
      
    } else {
      int* position = context->Environment->lookup( context->get_current_class() )\
                      ->lookup( name );
      emit_load( ACC, *position, SELF, s ); 
    }    
  }
}
