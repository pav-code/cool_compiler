=========== stringtab.cc ====================
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

=========== stringtab.h ===================
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

=========== stringtab_functions.h =============
//
// See copyright.h for copyright notice and limitation of liability
// and disclaimer of warranty provisions.
//
#include "copyright.h"

#include "cool-io.h"
#define MAXSIZE 1000000
#define min(a,b) (a > b ? b : a)

#include "stringtab.h"
#include <stdio.h>

//
// A string table is implemented a linked list of Entrys.  Each Entry
// in the list has a unique string.
//

template <class Elem>
Elem *StringTable<Elem>::add_string(char *s)
{
 return add_string(s,MAXSIZE);
}

//
// Add a string requires two steps.  First, the list is searched; if the
// string is found, a pointer to the existing Entry for that string is 
// returned.  If the string is not found, a new Entry is created and added
// to the list.
//
template <class Elem>
Elem *StringTable<Elem>::add_string(char *s, int maxchars)
{
  int len = min((int) strlen(s),maxchars);
  for(List<Elem> *l = tbl; l; l = l->tl())
    if (l->hd()->equal_string(s,len))
      return l->hd();

  Elem *e = new Elem(s,len,index++);
  tbl = new List<Elem>(e, tbl);
  return e;
}

//
// To look up a string, the list is scanned until a matching Entry is located.
// If no such entry is found, an assertion failure occurs.  Thus, this function
// is used only for strings that one expects to find in the table.
//
template <class Elem>
Elem *StringTable<Elem>::lookup_string(char *s)
{
  int len = strlen(s);
  for(List<Elem> *l = tbl; l; l = l->tl())
    if (l->hd()->equal_string(s,len))
      return l->hd();
  assert(0);   // fail if string is not found
  return NULL; // to avoid compiler warning
}

//
// lookup is similar to lookup_string, but uses the index of the string
// as the key.
//
template <class Elem>
Elem *StringTable<Elem>::lookup(int ind)
{
  for(List<Elem> *l = tbl; l; l = l->tl())
    if (l->hd()->equal_index(ind))
      return l->hd();
  assert(0);   // fail if string is not found
  return NULL; // to avoid compiler warning
}

//
// add_int adds the string representation of an integer to the list.
//
template <class Elem>
Elem *StringTable<Elem>::add_int(int i)
{
  static char *buf = new char[20];
  snprintf(buf, 20, "%d", i);
  return add_string(buf);
}
template <class Elem>
int StringTable<Elem>::first()
{
  return 0;
}

template <class Elem>
int StringTable<Elem>::more(int i)
{
  return i < index;
}

template <class Elem>
int StringTable<Elem>::next(int i)
{
  assert(i < index);
  return i+1;
}

template <class Elem>
void StringTable<Elem>::print()
{
  list_print(cerr,tbl);
}


=============== list.h ==========================
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

============ cool-io.h ===============================
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