===============================================================================
============================ stringtab.h ======================================
===============================================================================
// -*-Mode: C++;-*-
//
// See copyright.h for copyright notice and limitation of liability
// and disclaimer of warranty provisions.
//
#include "copyright.h"


#ifndef _STRINGTAB_H_
#define _STRINGTAB_H_

#include <assert.h>
#include <string.h>
#include "list.h"    // list template
#include "cool-io.h"

class Entry;
typedef Entry* Symbol;

extern ostream& operator<<(ostream& s, const Entry& sym);
extern ostream& operator<<(ostream& s, Symbol sym);

/////////////////////////////////////////////////////////////////////////
//
//  String Table Entries
//
/////////////////////////////////////////////////////////////////////////

class Entry {
protected:
  char *str;     // the string
  int  len;      // the length of the string (without trailing \0)
  int index;     // a unique index for each string
public:
  Entry(char *s, int l, int i);

  // is string argument equal to the str of this Entry?
  int equal_string(char *s, int len) const;  
                         
  // is the integer argument equal to the index of this Entry?
  bool equal_index(int ind) const           { return ind == index; }

  ostream& print(ostream& s) const;

  // Return the str and len components of the Entry.
  char *get_string() const;
  int get_len() const;
};

//
// There are three kinds of string table entries:
//   a true string, an string representation of an identifier, and 
//   a string representation of an integer.
//
// Having separate tables is convenient for code generation.  Different
// data definitions are generated for string constants (StringEntry) and 
// integer  constants (IntEntry).  Identifiers (IdEntry) don't produce
// static data definitions.
//
// code_def and code_ref are used by the code to produce definitions and
// references (respectively) to constants.  
//
class StringEntry : public Entry {
public:
  void code_def(ostream& str, int stringclasstag);
  void code_ref(ostream& str);
  StringEntry(char *s, int l, int i);
};

class IdEntry : public Entry {
public:
  IdEntry(char *s, int l, int i);
};

class IntEntry: public Entry {
public:
  void code_def(ostream& str, int intclasstag);
  void code_ref(ostream& str);
  IntEntry(char *s, int l, int i);
};

typedef StringEntry *StringEntryP;
typedef IdEntry *IdEntryP;
typedef IntEntry *IntEntryP;

//////////////////////////////////////////////////////////////////////////
//
//  String Tables
//
//////////////////////////////////////////////////////////////////////////

template <class Elem> 
class StringTable
{
protected:
   List<Elem> *tbl;   // a string table is a list
   int index;         // the current index
public:
   StringTable(): tbl((List<Elem> *) NULL), index(0) { }   // an empty table
   // The following methods each add a string to the string table.  
   // Only one copy of each string is maintained.  
   // Returns a pointer to the string table entry with the string.

   // add the prefix of s of length maxchars
   Elem *add_string(char *s, int maxchars);

   // add the (null terminated) string s
   Elem *add_string(char *s);

   // add the string representation of an integer
   Elem *add_int(int i);


   // An iterator.
   int first();       // first index
   int more(int i);   // are there more indices?
   int next(int i);   // next index

   Elem *lookup(int index);      // lookup an element using its index
   Elem *lookup_string(char *s); // lookup an element using its string

   void print();  // print the entire table; for debugging

};

class IdTable : public StringTable<IdEntry> { };

class StrTable : public StringTable<StringEntry>
{
public: 
   void code_string_table(ostream&, int classtag);
};

class IntTable : public StringTable<IntEntry>
{
public:
   void code_string_table(ostream&, int classtag);
};

extern IdTable idtable;
extern IntTable inttable;
extern StrTable stringtable;
#endif

===============================================================================
============================ stringtab.cc =====================================
===============================================================================

//
// See copyright.h for copyright notice and limitation of liability
// and disclaimer of warranty provisions.
//
#include "copyright.h"

#include <assert.h>
#include "stringtab_functions.h"
#include "stringtab.h"

extern char *pad(int n);

//
// Explicit template instantiations.
// Comment out for versions of g++ prior to 2.7
//
template class StringTable<IdEntry>;
template class StringTable<StringEntry>;
template class StringTable<IntEntry>;

Entry::Entry(char *s, int l, int i) : len(l), index(i) {
  str = new char [len+1];
  strncpy(str, s, len);
  str[len] = '\0';
}

int Entry::equal_string(char *string, int length) const
{
  return (len == length) && (strncmp(str,string,len) == 0);
}

ostream& Entry::print(ostream& s) const
{
  return s << "{" << str << ", " << len << ", " << index << "}\n";
}

ostream& operator<<(ostream& s, const Entry& sym) 
{
  return s << sym.get_string();
}


ostream& operator<<(ostream& s, Symbol sym)
{
  return s << *sym;
}

char *Entry::get_string() const
{
  return str;
}

int Entry::get_len() const
{
  return len;
}

// A Symbol is a pointer to an Entry.  Symbols are stored directly
// as nodes of the abstract syntax tree defined by the cool-tree.aps.
// The APS package requires that copy and print (called dump) functions
// be defined for components of the abstract syntax tree.
//
Symbol copy_Symbol(const Symbol s)
{
  return s;
}

void dump_Symbol(ostream& s, int n, Symbol sym)
{
  s << pad(n) << sym << endl;
}

StringEntry::StringEntry(char *s, int l, int i) : Entry(s,l,i) { }
IdEntry::IdEntry(char *s, int l, int i) : Entry(s,l,i) { }
IntEntry::IntEntry(char *s, int l, int i) : Entry(s,l,i) { }

IdTable idtable;
IntTable inttable;
StrTable stringtable;


===============================================================================
=============================== list.h ========================================
===============================================================================
/* -*-Mode: C++;-*- */
//
// See copyright.h for copyright notice and limitation of liability
// and disclaimer of warranty provisions.
//
#include "copyright.h"


//////////////////////////////////////////////////////////////////////
//  
//  list.h
//
//  This file implements a list template.
//  Adapted from similar templates written by Kathy Yelick and 
//  Paul Hilfinger.
//
//////////////////////////////////////////////////////////////////////

#ifndef _LIST_H_
#define _LIST_H_

#include "cool-io.h"  //includes iostream
#include <stdlib.h>

template <class T>
class List {
private:
  T *head;
  List<T>* tail;
public:
  List(T *h,List<T>* t = NULL): head(h), tail(t) { }

  T *hd() const       { return head; }  
  List<T>* tl() const { return tail; }
};

/////////////////////////////////////////////////////////////////////////
// 
// list function templates
//
// To avoid potential problems with mutliple definitions of 
// the List<> class members, the list functions are not members of the
// list class.
//
/////////////////////////////////////////////////////////////////////////

//
// Map a function for its side effect over a list.
//
template <class T>
void list_map(void f(T*), List<T> *l)
{
  for (l; l != NULL; l = l->tl())
    f(l->hd());
}

//
// Print the given list on the standard output.
// Requires that "<<" be defined for the element type.
//
template <class S, class T>
void list_print(S &str, List<T> *l)
{
   str << "[\n";
   for(; l != NULL; l = l->tl())
	str << *(l->hd()) << " ";
   str << "]\n";
}

//
// Compute the length of a list.
//
template <class T>
int list_length(List<T> *l)
{
  int i = 0;
  for (; l != NULL; l = l->tl())
    i++;
  return i;
}

#endif

===============================================================================
============================ cool-io.h ========================================
===============================================================================
//
// See copyright.h for copyright notice and limitation of liability
// and disclaimer of warranty provisions.
//
#include "copyright.h"

#ifndef COOL_IO_H
#define COOL_IO_H

//
// Cool files include this header to use the standard library's
// IO streams.  
//

//By default use the ANSI standard iostream, etc.
#ifndef  COOL_USE_OLD_HEADERS

# include <iostream>

using std::ostream;
using std::cout;
using std::cerr;
using std::endl;

# include <fstream>

using std::ofstream;

# include <iomanip>

using std::oct;
using std::dec;
using std::setw;
using std::setfill;

//Including the entire std namespace doesn't work well because of conflicts
//between e.g. std::plus and the plus AST node.
//using namespace std;

#else  
// COOL_USE_OLD_HEADERS is defined

// I haven't actually tested this, but it ought to work
# include <iostream.h>
# include <fstream.h>
# include <iomanip.h>

#endif // COOL_USE_OLD_HEADERS


#endif //COOL_IO_H

===============================================================================
============================== cool.h =========================================
===============================================================================

//
// See copyright.h for copyright notice and limitation of liability
// and disclaimer of warranty provisions.
//
#include "copyright.h"

#ifndef _COOL_H_
#define _COOL_H_

#include "cool-io.h"

/* a type renaming */
typedef int Boolean;
class Entry;
typedef Entry *Symbol;

Boolean copy_Boolean(Boolean);
void assert_Boolean(Boolean);
void dump_Boolean(ostream &,int,Boolean);

Symbol copy_Symbol(Symbol);
void assert_Symbol(Symbol);
void dump_Symbol(ostream &,int,Symbol);

#endif

===============================================================================
============================ symtab.h =========================================
===============================================================================

//
// See copyright.h for copyright notice and limitation of liability
// and disclaimer of warranty provisions.
//
#include "copyright.h"

// The symbol table package.
//
// Create a symbol table with :
// SymbolTable<thing to look up on, info to store> name();
//
// You must enter a scope before adding anything to the symbol table.

#ifndef _SYMTAB_H_
#define _SYMTAB_H_

#include "list.h"

//
// SymtabEnty<SYM,DAT> defines the entry for a symbol table that associates
//    symbols of type `SYM' with data of type `DAT *'.  
//

template <class SYM, class DAT>
class SymtabEntry {
private:
  SYM id;        // the key field
  DAT *info;     // associated information for the symbol
public:
  SymtabEntry(SYM x, DAT *y) : id(x), info(y) { }
  SYM get_id() const    { return id; }
  DAT *get_info() const { return info; }
};

//
// SymbolTable<SYM,DAT> describes a symbol table mapping symbols of
//    type `SYM' to data of type `DAT *'.  It is implemented as a
//    list of lists of `SymtabEntry<SYM,DAT> *'.  The inner list is
//    a scope, a mapping from symbols to data, and the outer list is
//    a list of scopes. 
//
//    `tbl' points to the current top scope.
//
//    `enterscope' makes the table point to a new scope whose parent
//       is the scope it pointed to previously.
//
//    `exitscope' makes the table point to the parent scope of the
//        current scope.  Note that the old child scope is not
//        deallocated.  One may save the state of a symbol table
//        at a given point by copying it with `operator ='
//
//    `addid(s,i)' adds a symbol table entry to the current scope of
//        the symbol table mapping symbol `s' to data `d'.  The old
//        top scope isn't modified; a new scope is created whose
//        entry list is the new entry followed by the old entry list,
//        and whose tail is the old top scope's parent.  The table
//        is made to point to this new scope.
//
//    `lookup(s)' looks for the symbol `s', starting at the top scope
//        and proceeding down the list of scopes until either an
//        entry is found whose `get_id()' equals `s', or the end of
//        the root scope is reached.  It returns the data item
//        associated with the entry, or NULL if no such entry exists.
//
//    
//    `probe(s)' checks the top scope for an entry whose `get_id()'
//        equals `s', and returns the entry's `get_info()' if
//        found, and NULL otherwise.
//
//    `dump()' prints the symbols in the symbol table.
//

template <class SYM, class DAT>
class SymbolTable
{
   typedef SymtabEntry<SYM,DAT> ScopeEntry;
   typedef List<ScopeEntry> Scope;
   typedef List<Scope> ScopeList;
private:
   ScopeList  *tbl;
public:
   SymbolTable(): tbl(NULL) { }     // create a new symbol table

   // Create pointer to current symbol table.
   SymbolTable &operator =(const SymbolTable &s) { tbl = s.tbl; return *this; }

   void fatal_error(char * msg)
   {
     cerr << msg << "\n";
     exit(1);
   } 

   // Enter a new scope.  A symbol table is organized as a list of
   // lists.  The head of the list is the innermost scope, the tail
   // holds the outer scopes.  A scope must be entered before anything
   // can be added to the table.

   void enterscope()
   {
       // The cast of NULL is required for template instantiation to work
       // correctly.
       tbl = new ScopeList((Scope *) NULL, tbl);
   }

   // Pop the first scope off of the symbol table.
   void exitscope()
   {
       // It is an error to exit a scope that doesn't exist.
       if (tbl == NULL) {
	   fatal_error("exitscope: Can't remove scope from an empty symbol table.");
       }
       tbl = tbl->tl();
   }

   // Add an item to the symbol table.
   ScopeEntry *addid(SYM s, DAT *i)
   {
       // There must be at least one scope to add a symbol.
       if (tbl == NULL) fatal_error("addid: Can't add a symbol without a scope.");
       ScopeEntry * se = new ScopeEntry(s,i);
       tbl = new ScopeList(new Scope(se, tbl->hd()), tbl->tl());
       return(se);
   }
   
   // Lookup an item through all scopes of the symbol table.  If found
   // it returns the associated information field, if not it returns
   // NULL.

   DAT * lookup(SYM s)
   {
       for(ScopeList *i = tbl; i != NULL; i=i->tl()) {
	   for( Scope *j = i->hd(); j != NULL; j = j->tl()) {
	       if (s == j->hd()->get_id()) {
		   return (j->hd()->get_info());
	       }
	   }
       }
       return NULL;
   }

   // probe the symbol table.  Check the top scope (only) for the item
   // 's'.  If found, return the information field.  If not return NULL.
   DAT *probe(SYM s)
   {
       if (tbl == NULL) {
	   fatal_error("probe: No scope in symbol table.");
       }
       for(Scope *i = tbl->hd(); i != NULL; i = i->tl()) {
	   if (s == i->hd()->get_id()) {
	       return(i->hd()->get_info());
	   }
       }
       return(NULL);
   }

   // Prints out the contents of the symbol table  
   void dump()
   {
      for(ScopeList *i = tbl; i != NULL; i = i->tl()) {
         cerr << "\nScope: \n";
         for(Scope *j = i->hd(); j != NULL; j = j->tl()) {
            cerr << "  " << j->hd()->get_id() << endl;
         }
      }
   }
 
};

#endif

