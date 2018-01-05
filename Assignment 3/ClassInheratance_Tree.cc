============================================================
============= tree.h =====================
============================================================
//
// See copyright.h for copyright notice and limitation of liability
// and disclaimer of warranty provisions.
//
#include "copyright.h"

#ifndef TREE_H
#define TREE_H
///////////////////////////////////////////////////////////////////////////
//
// file: tree.h
//
// This file defines the basic class of tree node and list
//
///////////////////////////////////////////////////////////////////////////
 

#include "stringtab.h"
#include "cool-io.h"

/////////////////////////////////////////////////////////////////////
//
//  tree_node
//
//   All APS nodes are derived from tree_node.  There is a
//   protected field:
//       int line_number     line in the source file from which this node came;
//                           this is read from a global variable when the
//                           node is created.
//      
//
//
//   The public methods are:
//       tree_node()
//         builds a new tree_node.  The type field is NULL, the
//         line_number is set to the value of the global yylineno.
//
//       void dump(ostream& s,int n); 
//         dump is a pretty printer for tree nodes.  The ostream argument
//         is the output stream on which the node is to be printed; n is
//         the number of spaces to indent the output.
//
//       int get_line_number();  return the line number
//       Symbol get_type();      return the type 
//
//       tree_node *set(tree_node *t)
//           sets the line number and type of "this" to the values in
//           the argument tree_node.  Returns "this".
//
//
////////////////////////////////////////////////////////////////////////////
class tree_node {
protected:
    int line_number;            // stash the line number when node is made
public:
    tree_node();
    virtual tree_node *copy() = 0;
    virtual ~tree_node() { }
    virtual void dump(ostream& stream, int n) = 0;
    int get_line_number();
    tree_node *set(tree_node *);
};

///////////////////////////////////////////////////////////////////
//
//  Lists of APS objects are implemented by the "list_node"
//  template.  List elements have type Elem.  The interface is:
//
//     tree_node *copy()
//     list_node<Elem> *copy_list()
//
//     These functions have identical behavior; they return a deep
//     copy of the list (i.e., all elements of the list are copied).
//     When possible, the second function should be used, as it
//     has a more accurate result type.  The "copy" function is for
//     copying an entire APS tree of which a list is just one component
//     (see the definition of copy() in class tree_node).
//
//     Elem nth(int n);
//     returns the nth element of a list.  If the list has fewer than n
//     elements, an error is generated.
//
//     int first();
//     int next(int n);
//     int more(int n);
//       These three functions define a simple iterator for stepping through
//     list elements in order.  If l is a list, a typical use would be:
//
//     for(int i = l->first(); l->more(i); i = l->next(i))
//         ... operate on l->nth(i) ...
//
//      
//     int len()
//     returns the length of the list
//
//     nth_length(int n, int &len);
//     Returns the nth element of the list or NULL if there are not n elements.
//     "len" is set to the length of the list.  This method is used internally
//     by the APS package to efficiently traverse the list representation.  
//
//     static list_node<Elem> *nil();
//     static list_node<Elem> *single(Elem);
//     static list_node<Elem> *append(list_node<Elem> *, list_node<Elem> *);
//
//     These three functions construct an empty list, a list of one element,
//     and append two lists, respectively.  Note that the functions are static;
//     there is no "this" parameter.  Example uses:
//
//     list_node<Elem>::nil();
//     list_node<Elem>::single(e);     where "e" has type Elem
//     list_node<Elem>::append(l1,l2);
//
//////////////////////////////////////////////////////////////////////////////

template <class Elem> class list_node : public tree_node {
public:
    tree_node *copy()            { return copy_list(); }
    Elem nth(int n);
    //
    // The next three define a simple iterator.
    //
    int first()      { return 0; }
    int next(int n)  { return n + 1; }
    int more(int n)  { return (n < len()); }

    virtual list_node<Elem> *copy_list() = 0;
    virtual ~list_node() { }
    virtual int len() = 0;
    virtual Elem nth_length(int n, int &len) = 0;

    static list_node<Elem> *nil();
    static list_node<Elem> *single(Elem);
    static list_node<Elem> *append(list_node<Elem> *l1,list_node<Elem> *l2);
};

char *pad(int n);

extern int info_size;

template <class Elem> class nil_node : public list_node<Elem> {
public:
    list_node<Elem> *copy_list();
    int len();
    Elem nth_length(int n, int &len);
    void dump(ostream& stream, int n);
};

template <class Elem> class single_list_node : public list_node<Elem> {
    Elem elem;
public:
    single_list_node(Elem t) {
	elem = t;
    }
    list_node<Elem> *copy_list();
    int len();
    Elem nth_length(int n, int &len);
    void dump(ostream& stream, int n);
};


template <class Elem> class append_node : public list_node<Elem> {
private:
    list_node<Elem> *some, *rest;
public:
    append_node(list_node<Elem> *l1, list_node<Elem> *l2) {
	some = l1;
	rest = l2;
    }
    list_node<Elem> *copy_list();
    int len();
    Elem nth(int n);
    Elem nth_length(int n, int &len);
    void dump(ostream& stream, int n);
};


template <class Elem> single_list_node<Elem> *list(Elem x);
template <class Elem> append_node<Elem> *cons(Elem x, list_node<Elem> *l);
template <class Elem> append_node<Elem> *xcons(list_node<Elem> *l, Elem x);


template <class Elem> list_node<Elem> *list_node<Elem>::nil() { return new nil_node<Elem>(); }
template <class Elem> list_node<Elem> *list_node<Elem>::single(Elem e) { return new single_list_node<Elem>(e); }
template <class Elem> list_node<Elem> *list_node<Elem>::append(list_node<Elem> *l1,list_node<Elem> *l2) {
   return new append_node<Elem>(l1,l2);
}


///////////////////////////////////////////////////////////////////////////
//
// list_node::nth
//
// function to find the nth element of the list
//
///////////////////////////////////////////////////////////////////////////

template <class Elem> Elem list_node<Elem>::nth(int n)
{
    int len;
    Elem tmp = nth_length(n ,len);

    if (tmp)
	return tmp;
    else {
	cerr << "error: outside the range of the list\n";
	exit(1);
    }
}

// added 10/30/06 cgs
template <class Elem> Elem append_node<Elem>::nth(int n)
{
    int len;
    Elem tmp = nth_length(n ,len);

    if (tmp)
	return tmp;
    else {
	cerr << "error: outside the range of the list\n";
	exit(1);
    }
}

///////////////////////////////////////////////////////////////////////////
//
// nil_node::copy_list
//
// return the deep copy of the nil_node
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> list_node<Elem> *nil_node<Elem>::copy_list()
{
    return new nil_node<Elem>();
}


///////////////////////////////////////////////////////////////////////////
//
// nil_node::len
//
// return the length of the nil_node
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> int nil_node<Elem>::len()
{
    return 0;
}



///////////////////////////////////////////////////////////////////////////
//
// nil_node::nth_length
//
// return the nth element on the list
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> Elem nil_node<Elem>::nth_length(int, int &len)
{
    len = 0;
    return NULL;
}


///////////////////////////////////////////////////////////////////////////
//
// nil_node::dump
//
// dump for list node
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> void nil_node<Elem>::dump(ostream& stream, int n)
{
    stream << pad(n) << "(nil)\n";
}


///////////////////////////////////////////////////////////////////////////
//
// single_list_node::copy_list
//
// return the deep copy of the single_list_node
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> list_node<Elem> *single_list_node<Elem>::copy_list()
{
    return new single_list_node<Elem>((Elem) elem->copy());
}


///////////////////////////////////////////////////////////////////////////
//
// single_list_node::len
//
// return the length of the single_list_node
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> int single_list_node<Elem>::len()
{
    return 1;
}


///////////////////////////////////////////////////////////////////////////
//
// single_list_node::nth_length
//
// return the nth element on the list
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> Elem single_list_node<Elem>::nth_length(int n, int &len)
{
    len = 1;
    if (n)
	return NULL;
    else
	return elem;
}


///////////////////////////////////////////////////////////////////////////
//
// single_list_node::dump
//
// dump for list node
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> void single_list_node<Elem>::dump(ostream& stream, int n)
{
    elem->dump(stream, n);
}


///////////////////////////////////////////////////////////////////////////
//
// append_node::copy_list
//
// return the deep copy of the append_node
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> list_node<Elem> *append_node<Elem>::copy_list()
{
    return new append_node<Elem>(some->copy_list(), rest->copy_list());
}


///////////////////////////////////////////////////////////////////////////
//
// append_node::len
//
// return the length of the append_node
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> int append_node<Elem>::len()
{
    return some->len() + rest->len();
}


///////////////////////////////////////////////////////////////////////////
//
// append_node::nth_length
//
// return the nth element on the list
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> Elem append_node<Elem>::nth_length(int n, int &len)
{
    int rlen;
    Elem tmp = some->nth_length(n, len);

    if (!tmp) {
	tmp = rest->nth_length(n-len, rlen);
	len += rlen;
    }
    return tmp;
}


///////////////////////////////////////////////////////////////////////////
//
// append_node::dump
//
// dump for list node
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> void append_node<Elem>::dump(ostream& stream, int n)
{
    int i, size;

    size = len();
    stream << pad(n) << "list\n";
    for (i = 0; i < size; i++)
      nth(i)->dump(stream, n+2);
    stream << pad(n) << "(end_of_list)\n";
}


///////////////////////////////////////////////////////////////////////////
//
// list
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> single_list_node<Elem> *list(Elem x)
{
    return new single_list_node<Elem>(x);
}


///////////////////////////////////////////////////////////////////////////
//
// cons
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> append_node<Elem> *cons(Elem x, list_node<Elem> *l)
{
    return new append_node<Elem>(list(x), l);
}


///////////////////////////////////////////////////////////////////////////
//
// xcons
//
///////////////////////////////////////////////////////////////////////////
template <class Elem> append_node<Elem> *xcons(list_node<Elem> *l, Elem x)
{
    return new append_node<Elem>(l, list(x));
}

#endif /* TREE_H */

============================================================
============== cool-tree.h =====================
============================================================

#ifndef COOL_TREE_H
#define COOL_TREE_H
//////////////////////////////////////////////////////////
//
// file: cool-tree.h
//
// This file defines classes for each phylum and constructor
//
//////////////////////////////////////////////////////////

#include "tree.h"
#include "cool-tree.handcode.h"
#include "custom-header.h"

// define the class for phylum
// define simple phylum - Program
typedef class Program_class *Program;

class Program_class : public tree_node {
public:
   tree_node *copy()		 { return copy_Program(); }
   virtual Program copy_Program() = 0;

#ifdef Program_EXTRAS
   Program_EXTRAS
#endif
};


// define simple phylum - Class_
typedef class Class__class *Class_;

class Class__class : public tree_node {
public:
   tree_node *copy()		 { return copy_Class_(); }
   virtual Class_ copy_Class_() = 0;

   virtual Symbol get_parent() = 0;
   virtual Symbol get_name() = 0;

#ifdef Class__EXTRAS
   Class__EXTRAS
#endif
};


// define simple phylum - Feature
typedef class Feature_class *Feature;

class Feature_class : public tree_node {
public:
   tree_node *copy()		 { return copy_Feature(); }
   virtual Feature copy_Feature() = 0;

#ifdef Feature_EXTRAS
   Feature_EXTRAS
#endif
};


// define simple phylum - Formal
typedef class Formal_class *Formal;

class Formal_class : public tree_node {
public:
   tree_node *copy()		 { return copy_Formal(); }
   virtual Formal copy_Formal() = 0;

#ifdef Formal_EXTRAS
   Formal_EXTRAS
#endif
};


// define simple phylum - Expression
typedef class Expression_class *Expression;

class Expression_class : public tree_node {
public:
   tree_node *copy()		 { return copy_Expression(); }
   virtual Expression copy_Expression() = 0;

#ifdef Expression_EXTRAS
   Expression_EXTRAS
#endif
};


// define simple phylum - Case
typedef class Case_class *Case;

class Case_class : public tree_node {
public:
   tree_node *copy()		 { return copy_Case(); }
   virtual Case copy_Case() = 0;

#ifdef Case_EXTRAS
   Case_EXTRAS
#endif
};


// define the class for phylum - LIST
// define list phlyum - Classes
typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;


// define list phlyum - Features
typedef list_node<Feature> Features_class;
typedef Features_class *Features;


// define list phlyum - Formals
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;


// define list phlyum - Expressions
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;


// define list phlyum - Cases
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;


// define the class for constructors
// define constructor - program
class program_class : public Program_class {
protected:
   Classes classes;
public:
   program_class(Classes a1) {
      classes = a1;
   }
   Program copy_Program();
   void dump(ostream& stream, int n);

#ifdef Program_SHARED_EXTRAS
   Program_SHARED_EXTRAS
#endif
#ifdef program_EXTRAS
   program_EXTRAS
#endif
};


// define constructor - class_
class class__class : public Class__class {
protected:
   Symbol name;
   Symbol parent;
   Features features;
   Symbol filename;
public:
   class__class(Symbol a1, Symbol a2, Features a3, Symbol a4) {
      name = a1;
      parent = a2;
      features = a3;
      filename = a4;
   }
   Class_ copy_Class_();
   void dump(ostream& stream, int n);

   Symbol get_parent() {
      return (parent);
   }
   Symbol get_name() {
      return (name);
   }

#ifdef Class__SHARED_EXTRAS
   Class__SHARED_EXTRAS
#endif
#ifdef class__EXTRAS
   class__EXTRAS
#endif
};


// define constructor - method
class method_class : public Feature_class {
protected:
   Symbol name;
   Formals formals;
   Symbol return_type;
   Expression expr;
public:
   method_class(Symbol a1, Formals a2, Symbol a3, Expression a4) {
      name = a1;
      formals = a2;
      return_type = a3;
      expr = a4;
   }
   Feature copy_Feature();
   void dump(ostream& stream, int n);

#ifdef Feature_SHARED_EXTRAS
   Feature_SHARED_EXTRAS
#endif
#ifdef method_EXTRAS
   method_EXTRAS
#endif
};


// define constructor - attr
class attr_class : public Feature_class {
protected:
   Symbol name;
   Symbol type_decl;
   Expression init;
public:
   attr_class(Symbol a1, Symbol a2, Expression a3) {
      name = a1;
      type_decl = a2;
      init = a3;
   }
   Feature copy_Feature();
   void dump(ostream& stream, int n);

#ifdef Feature_SHARED_EXTRAS
   Feature_SHARED_EXTRAS
#endif
#ifdef attr_EXTRAS
   attr_EXTRAS
#endif
};


// define constructor - formal
class formal_class : public Formal_class {
protected:
   Symbol name;
   Symbol type_decl;
public:
   formal_class(Symbol a1, Symbol a2) {
      name = a1;
      type_decl = a2;
   }
   Formal copy_Formal();
   void dump(ostream& stream, int n);

   Symbol get_name() {
      return (name);
   }
   Symbol get_type() {
      return (type_decl);
   }

#ifdef Formal_SHARED_EXTRAS
   Formal_SHARED_EXTRAS
#endif
#ifdef formal_EXTRAS
   formal_EXTRAS
#endif
};


// define constructor - branch
class branch_class : public Case_class {
protected:
   Symbol name;
   Symbol type_decl;
   Expression expr;
public:
   branch_class(Symbol a1, Symbol a2, Expression a3) {
      name = a1;
      type_decl = a2;
      expr = a3;
   }
   Case copy_Case();
   void dump(ostream& stream, int n);

#ifdef Case_SHARED_EXTRAS
   Case_SHARED_EXTRAS
#endif
#ifdef branch_EXTRAS
   branch_EXTRAS
#endif
};


// define constructor - assign
class assign_class : public Expression_class {
protected:
   Symbol name;
   Expression expr;
public:
   assign_class(Symbol a1, Expression a2) {
      name = a1;
      expr = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef assign_EXTRAS
   assign_EXTRAS
#endif
};


// define constructor - static_dispatch
class static_dispatch_class : public Expression_class {
protected:
   Expression expr;
   Symbol type_name;
   Symbol name;
   Expressions actual;
public:
   static_dispatch_class(Expression a1, Symbol a2, Symbol a3, Expressions a4) {
      expr = a1;
      type_name = a2;
      name = a3;
      actual = a4;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef static_dispatch_EXTRAS
   static_dispatch_EXTRAS
#endif
};


// define constructor - dispatch
class dispatch_class : public Expression_class {
protected:
   Expression expr;
   Symbol name;
   Expressions actual;
public:
   dispatch_class(Expression a1, Symbol a2, Expressions a3) {
      expr = a1;
      name = a2;
      actual = a3;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef dispatch_EXTRAS
   dispatch_EXTRAS
#endif
};


// define constructor - cond
class cond_class : public Expression_class {
protected:
   Expression pred;
   Expression then_exp;
   Expression else_exp;
public:
   cond_class(Expression a1, Expression a2, Expression a3) {
      pred = a1;
      then_exp = a2;
      else_exp = a3;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef cond_EXTRAS
   cond_EXTRAS
#endif
};


// define constructor - loop
class loop_class : public Expression_class {
protected:
   Expression pred;
   Expression body;
public:
   loop_class(Expression a1, Expression a2) {
      pred = a1;
      body = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef loop_EXTRAS
   loop_EXTRAS
#endif
};


// define constructor - typcase
class typcase_class : public Expression_class {
protected:
   Expression expr;
   Cases cases;
public:
   typcase_class(Expression a1, Cases a2) {
      expr = a1;
      cases = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef typcase_EXTRAS
   typcase_EXTRAS
#endif
};


// define constructor - block
class block_class : public Expression_class {
protected:
   Expressions body;
public:
   block_class(Expressions a1) {
      body = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef block_EXTRAS
   block_EXTRAS
#endif
};


// define constructor - let
class let_class : public Expression_class {
protected:
   Symbol identifier;
   Symbol type_decl;
   Expression init;
   Expression body;
public:
   let_class(Symbol a1, Symbol a2, Expression a3, Expression a4) {
      identifier = a1;
      type_decl = a2;
      init = a3;
      body = a4;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef let_EXTRAS
   let_EXTRAS
#endif
};


// define constructor - plus
class plus_class : public Expression_class {
protected:
   Expression e1;
   Expression e2;
public:
   plus_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef plus_EXTRAS
   plus_EXTRAS
#endif
};


// define constructor - sub
class sub_class : public Expression_class {
protected:
   Expression e1;
   Expression e2;
public:
   sub_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef sub_EXTRAS
   sub_EXTRAS
#endif
};


// define constructor - mul
class mul_class : public Expression_class {
protected:
   Expression e1;
   Expression e2;
public:
   mul_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef mul_EXTRAS
   mul_EXTRAS
#endif
};


// define constructor - divide
class divide_class : public Expression_class {
protected:
   Expression e1;
   Expression e2;
public:
   divide_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef divide_EXTRAS
   divide_EXTRAS
#endif
};


// define constructor - neg
class neg_class : public Expression_class {
protected:
   Expression e1;
public:
   neg_class(Expression a1) {
      e1 = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef neg_EXTRAS
   neg_EXTRAS
#endif
};


// define constructor - lt
class lt_class : public Expression_class {
protected:
   Expression e1;
   Expression e2;
public:
   lt_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef lt_EXTRAS
   lt_EXTRAS
#endif
};


// define constructor - eq
class eq_class : public Expression_class {
protected:
   Expression e1;
   Expression e2;
public:
   eq_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef eq_EXTRAS
   eq_EXTRAS
#endif
};


// define constructor - leq
class leq_class : public Expression_class {
protected:
   Expression e1;
   Expression e2;
public:
   leq_class(Expression a1, Expression a2) {
      e1 = a1;
      e2 = a2;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef leq_EXTRAS
   leq_EXTRAS
#endif
};


// define constructor - comp
class comp_class : public Expression_class {
protected:
   Expression e1;
public:
   comp_class(Expression a1) {
      e1 = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef comp_EXTRAS
   comp_EXTRAS
#endif
};


// define constructor - int_const
class int_const_class : public Expression_class {
protected:
   Symbol token;
public:
   int_const_class(Symbol a1) {
      token = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef int_const_EXTRAS
   int_const_EXTRAS
#endif
};


// define constructor - bool_const
class bool_const_class : public Expression_class {
protected:
   Boolean val;
public:
   bool_const_class(Boolean a1) {
      val = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef bool_const_EXTRAS
   bool_const_EXTRAS
#endif
};


// define constructor - string_const
class string_const_class : public Expression_class {
protected:
   Symbol token;
public:
   string_const_class(Symbol a1) {
      token = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef string_const_EXTRAS
   string_const_EXTRAS
#endif
};


// define constructor - new_
class new__class : public Expression_class {
protected:
   Symbol type_name;
public:
   new__class(Symbol a1) {
      type_name = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef new__EXTRAS
   new__EXTRAS
#endif
};


// define constructor - isvoid
class isvoid_class : public Expression_class {
protected:
   Expression e1;
public:
   isvoid_class(Expression a1) {
      e1 = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef isvoid_EXTRAS
   isvoid_EXTRAS
#endif
};


// define constructor - no_expr
class no_expr_class : public Expression_class {
protected:
public:
   no_expr_class() {
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef no_expr_EXTRAS
   no_expr_EXTRAS
#endif
};


// define constructor - object
class object_class : public Expression_class {
protected:
   Symbol name;
public:
   object_class(Symbol a1) {
      name = a1;
   }
   Expression copy_Expression();
   void dump(ostream& stream, int n);

#ifdef Expression_SHARED_EXTRAS
   Expression_SHARED_EXTRAS
#endif
#ifdef object_EXTRAS
   object_EXTRAS
#endif
};


// define the prototypes of the interface
Classes nil_Classes();
Classes single_Classes(Class_);
Classes append_Classes(Classes, Classes);
Features nil_Features();
Features single_Features(Feature);
Features append_Features(Features, Features);
Formals nil_Formals();
Formals single_Formals(Formal);
Formals append_Formals(Formals, Formals);
Expressions nil_Expressions();
Expressions single_Expressions(Expression);
Expressions append_Expressions(Expressions, Expressions);
Cases nil_Cases();
Cases single_Cases(Case);
Cases append_Cases(Cases, Cases);
Program program(Classes);
Class_ class_(Symbol, Symbol, Features, Symbol);
Feature method(Symbol, Formals, Symbol, Expression);
Feature attr(Symbol, Symbol, Expression);
Formal formal(Symbol, Symbol);
Case branch(Symbol, Symbol, Expression);
Expression assign(Symbol, Expression);
Expression static_dispatch(Expression, Symbol, Symbol, Expressions);
Expression dispatch(Expression, Symbol, Expressions);
Expression cond(Expression, Expression, Expression);
Expression loop(Expression, Expression);
Expression typcase(Expression, Cases);
Expression block(Expressions);
Expression let(Symbol, Symbol, Expression, Expression);
Expression plus(Expression, Expression);
Expression sub(Expression, Expression);
Expression mul(Expression, Expression);
Expression divide(Expression, Expression);
Expression neg(Expression);
Expression lt(Expression, Expression);
Expression eq(Expression, Expression);
Expression leq(Expression, Expression);
Expression comp(Expression);
Expression int_const(Symbol);
Expression bool_const(Boolean);
Expression string_const(Symbol);
Expression new_(Symbol);
Expression isvoid(Expression);
Expression no_expr();
Expression object(Symbol);


#endif


============================================================
============ cool-tree.handcode.h ==========================
============================================================
//
// The following include files must come first.

#ifndef COOL_TREE_HANDCODE_H
#define COOL_TREE_HANDCODE_H

#include <iostream>
#include "tree.h"
#include "cool.h"
#include "stringtab.h"

#define yylineno curr_lineno;
extern int yylineno;

inline Boolean copy_Boolean(Boolean b) {return b; }
inline void assert_Boolean(Boolean) {}
inline void dump_Boolean(ostream& stream, int padding, Boolean b)
	{ stream << pad(padding) << (int) b << "\n"; }

void dump_Symbol(ostream& stream, int padding, Symbol b);
void assert_Symbol(Symbol b);
Symbol copy_Symbol(Symbol b);

class Program_class;
typedef Program_class *Program;
class Class__class;
typedef Class__class *Class_;
class Feature_class;
typedef Feature_class *Feature;
class Formal_class;
typedef Formal_class *Formal;
class Expression_class;
typedef Expression_class *Expression;
class Case_class;
typedef Case_class *Case;

typedef list_node<Class_> Classes_class;
typedef Classes_class *Classes;
typedef list_node<Feature> Features_class;
typedef Features_class *Features;
typedef list_node<Formal> Formals_class;
typedef Formals_class *Formals;
typedef list_node<Expression> Expressions_class;
typedef Expressions_class *Expressions;
typedef list_node<Case> Cases_class;
typedef Cases_class *Cases;

#define Program_EXTRAS                           \
virtual void semant() = 0;			  \
virtual void dump_with_types(ostream&, int) = 0;   \
virtual void print_hierarchy(Class_, ostream&, Environment) = 0; \
virtual int get_parent(Class_ ) = 0;

#define program_EXTRAS                         \
void semant();     				\
void dump_with_types(ostream&, int);             \
void print_hierarchy(Class_, ostream&, Environment);          \
virtual int get_parent(Class_ );

#define Class__EXTRAS                             \
virtual Symbol get_filename() = 0;                 \
virtual void dump_with_types(ostream&,int) = 0;     \
virtual void dump_test(ostream& , Environment) = 0;


#define class__EXTRAS                           \
Symbol get_filename() { return filename; }       \
void dump_with_types(ostream&,int);               \
void dump_test(ostream& , Environment);

#define Feature_EXTRAS                                      \
virtual void dump_with_types(ostream&,int) = 0;              \
virtual void dump_test(ostream& , Environment) = 0;



#define Feature_SHARED_EXTRAS                          \
void dump_with_types(ostream&,int);    

#define method_EXTRAS                                 \
void dump_test(ostream& , Environment);

#define attr_EXTRAS                                 \
void dump_test(ostream& , Environment);


#define Formal_EXTRAS                              \
virtual void dump_with_types(ostream&,int) = 0;     \
virtual void dump_test(ostream& , Environment) = 0;


#define formal_EXTRAS                           \
void dump_with_types(ostream&,int);              \
void dump_test(ostream& , Environment);


#define Case_EXTRAS                             \
virtual void dump_with_types(ostream& ,int) = 0;


#define branch_EXTRAS                                   \
void dump_with_types(ostream& ,int);


#define Expression_EXTRAS                    \
Symbol type;                                 \
Symbol get_type() { return type; }           \
Expression set_type(Symbol s) { type = s; return this; } \
virtual void dump_with_types(ostream&,int) = 0;  \
void dump_type(ostream&, int);               \
Expression_class() { type = (Symbol) NULL; }

#define Expression_SHARED_EXTRAS           \
void dump_with_types(ostream&,int); 

#endif

