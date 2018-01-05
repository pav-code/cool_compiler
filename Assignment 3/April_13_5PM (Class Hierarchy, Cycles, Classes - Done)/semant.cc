#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <vector>
#include <algorithm>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
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

//////////////////////////////////////////////////////////////////////
//
//  int  get_index(char* ) : global function for idtable entries.
//////////////////////////////////////////////////////////////////////

int get_index(char* name)
{
   char* fromTable;
   for (int i = idtable.first(); idtable.more(i); i=idtable.next(i) ) {
      fromTable = idtable.lookup(i)->get_string();
      if ( strncmp(fromTable, name, 100) == 0 ) {
         return (i);
      }
   }
   return (NULL);
}

int get_parent(int C, Environment pEnv)
{
   if ( (pEnv->pInhTree).empty() ) {
      return -1;
   }
   Symbol sC = idtable.lookup(C);
   std::map<int, Graph_Node>::iterator it;

   it = (pEnv->pInhTree).find( get_index(sC->get_string()) );
   if ( it != (pEnv->pInhTree).end() ) {
      return( it->second.get_parent_index() );
   } else {
      return(-1);
   }
   return -1;
}

void get_ancestor_list(std::vector<int> &list, int C, Environment pEnv)
{
   int classNode = C;

   if ( classNode ==  get_index("Object") ) {
      return;
   }
   while ( true ) {
      list.push_back( classNode );
      classNode = get_parent( classNode, pEnv );
      if ( classNode == get_index("Object") ) {
         list.push_back( classNode );
         return;
      }
   }
}

bool subclass(int C, int P, Environment pEnv)
{
   int classNode = C;
   int iSelfType = get_index("SELF_TYPE");

   if ( P == get_index("Object") ) { 
      return (true);
   }
   if ( C == iSelfType && P == iSelfType ) {
      return (true);
   } else if ( P == iSelfType ) {
      return (false);
   } else if ( C == iSelfType ) {
      return ( subclass(pEnv->get_current_class(), P, pEnv) );
   }
   
   while (true) {
      if ( classNode == P ) {
         return (true);
      } else if (classNode == get_index("Object") ) {
         return (false);
      }
      classNode = get_parent(classNode, pEnv);
   }
}

int lub(int C1, int C2, Environment pEnv)
{
   int iSelfType = get_index("SELF_TYPE");
   if ( C1 == iSelfType && C2 == iSelfType ) {
      return ( iSelfType );
   } else if ( C1 == iSelfType ) {
      return ( lub(C2, pEnv->get_current_class(), pEnv) );
   } else if ( C2 == iSelfType ) {
      return ( lub(C1, pEnv->get_current_class(), pEnv) );
   }

   std::vector<int> L1;
   std::vector<int> L2;
   get_ancestor_list(L1, C1, pEnv);
   get_ancestor_list(L2, C2, pEnv);
   int dummyNode = -1;
   
   for (std::vector<int>::iterator it1 = L1.begin(); it1!=L1.end(); it1++) {
      dummyNode = *it1;
      for (std::vector<int>::iterator it2 = L2.begin(); it2!=L2.end(); it2++) {
         if (dummyNode == *it2) {
            return (dummyNode);
         }
      }
   } 
   return (get_index("Object")); 
}


//////////////////////////////////////////////////////////////////////
//
// ClassTable
// std::map<char*, Graph_Node, char_cmp>: used for constructing the graph
// inheritance. Graph_Node is defined in semant.h, and is used for defining
// nodes with name, parent and ptr to ancestor node. 
//   First Loop: Runs through all classes of the program and adds them to our
//   inheritance graph (unless they are already added in which case it gives
//   2 types of errors: 1) Redefinition of a basic class 2) A redefinition of
//   any other class.
//
//   Second Loop: Runs through all classes and identifies each classes' ancestor
//   node. It also gives an error when a class is not defined.
//
//   Third Loop: This loops checks for cycling inheritance. It goes through a map
//   of <Name, Graph_Node> which includes all classes in the file, plus the added
//   basic classes (Int, Bool, Object, IO, and String). It checks if the current
//   node is a Basic Class, if it is it skips it, if not it builds a secondary map
//   <Name, Parent> (of types: char*) with which we will follow the path of current
//   node, up the hierarchy adding every parent node. If we get to IO, Object we move
//   on since this is legal, if we get to Int, String or Bool we have an illegal 
//   inheritance, and finally if we find the current element's parent in the tree
//   (we started checking parent since the beggining of the small loop) then we
//   have a case of cyclic inheritance.
//
//   bCompError - flag to stop further processing after a class associated 
//                compilation error
//
//////////////////////////////////////////////////////////////////////

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    /* Fill this in */
    int test;
    bool bNext, bCompError = false;
    char* aString;
    Graph_Node* nPrint, *nTemp;
    Symbol symClass;
    Class_ classObj;

    test = classes->len();

    ofstream myfile;
    myfile.open("example.txt");
    myfile << ("Length of var classes is: ");
    myfile << ("%i", test) << endl;

    for(int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        myfile << ("#") << i << " ";
        classObj = classes->nth(i);

        aString = classObj->get_name();
        myfile << ("Class: ") << aString;

        aString = classObj->get_parent();
        myfile << (". Its parent is: ") << aString << endl;
    }

    std::map<int, Graph_Node>::iterator it;
    std::map<int, Graph_Node>::iterator dummyit;
    std::map<int, int> CycleTree;
    std::map<int, int>::iterator cit;

    std::map<int, int>::iterator cittest;
  
    int iObject = get_index("Object");
    InhTree.insert(std::pair<int,Graph_Node>(iObject,Graph_Node(iObject, get_index("_no_class"))));
    InhTree.insert(std::pair<int,Graph_Node>(get_index("IO"),Graph_Node(get_index("IO"), iObject, (InhTree.find(iObject)->second).Get_Parent())));
    InhTree.insert(std::pair<int,Graph_Node>(get_index("Int"),Graph_Node(get_index("Int"), iObject, (InhTree.find(iObject)->second).Get_Parent())));
    InhTree.insert(std::pair<int,Graph_Node>(get_index("String"),Graph_Node(get_index("String"), iObject, (InhTree.find(iObject)->second).Get_Parent())));
    InhTree.insert(std::pair<int,Graph_Node>(get_index("Bool"),Graph_Node(get_index("Bool"), iObject, (InhTree.find(iObject)->second).Get_Parent())));

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        classObj = classes->nth(i);
		if ( get_index(classObj->get_name())==get_index("SELF_TYPE") ) { 
           semant_error(classObj) << "SELF_TYPE redeclared as a class name."<<endl;
           myfile << "Error: SELF_TYPE used as a class name." << endl;
           bCompError = true; 		   
		}
        it = InhTree.find(get_index(classObj->get_name()));
        if (it == InhTree.end()) {
           InhTree.insert(std::pair<int,Graph_Node>(
                   get_index(classObj->get_name()),
                   Graph_Node(get_index(classObj->get_name()), get_index(classObj->get_parent()))));
        } else if ( (it->first == get_index("IO"))     || 
                    (it->first == get_index("Int"))    || 
                    (it->first == get_index("String")) ||
                    (it->first == get_index("Bool"))    ) {        
           semant_error(classObj) << "Trying to redefine a Basic Class: " << idtable.lookup(it->first) << "." << endl;
           myfile << "Error: Basic class redefinition." << endl;
           bCompError = true; 
           break;       
        } else {
           semant_error(classObj) << "Trying to redefine a Class: " << idtable.lookup(it->first) << "." << endl;
           myfile << "Error: Class redefinition." << endl;
           bCompError = true;
           break;
        }
    }

    if (!bCompError && InhTree.find(get_index("Main")) == InhTree.end()) {
        //TODO: Error - Class Main not declared
        semant_error() << "Class Main is not defined." << endl;
        myfile << "Error: Class Main not defined." << endl;
        bCompError = true;
    }
    if (!bCompError) {
      for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        classObj = classes->nth(i);
        it = InhTree.find( get_index(classObj->get_parent()) );       
        if (it != InhTree.end()) {
           nTemp = &InhTree.find( get_index(classObj->get_name()) )->second;
           nTemp->Set_Parent(it->second.Get_Parent());
        } else {
           semant_error(classObj) << "Definition is missing for Class: " << classObj->get_parent() << endl;
           myfile << "Error: Class definition is missing" << endl;
           bCompError = true;
           break;
        }
      }
    }

    if (!bCompError) {    
      for (it=InhTree.begin(); it!=InhTree.end(); ++it) {
       bNext = false;
       if ( (it->first == get_index("IO"))     || 
            (it->first == get_index("Int"))    ||
            (it->first == get_index("String")) ||
            (it->first == get_index("Bool"))   ||
            (it->first == get_index("Object"))  ) {
          bNext = true;
       }
       if (!bNext) {
         CycleTree.clear();
         CycleTree.insert(std::pair<int,int>(it->second.get_name_index(),it->second.get_parent_index()));
         cit = CycleTree.begin();
         while (true) {  
             if ( (cit->second == get_index("IO"))     || 
                  (cit->second == get_index("Object")) ) {
                 break;
             }
             if ( (cit->second == get_index("Int"))    || 
                  (cit->second == get_index("String")) ||
                  (cit->second == get_index("Bool"))   ) {
                 semant_error(classObj) << "Class " << idtable.lookup(cit->first) << " is trying to inherit from Int, String or Bool." << endl;
                 myfile << "Error: Attempt to inherit Int, String or Bool" << endl;
                 bCompError = true;
                 break;
             }
             if (CycleTree.find(cit->second) == CycleTree.end()) {
                 dummyit = InhTree.find(cit->second);
                 CycleTree.insert(std::pair<int,int>(dummyit->second.get_name_index(),dummyit->second.get_parent_index()));  
                 cit = CycleTree.find(dummyit->second.get_name_index());   
             } else {
                 //TODO: Error - Cyclic Inheritance
                 semant_error() << "Cyclic inheritance on Class: " << idtable.lookup(cit->first) << " inheriting: " << idtable.lookup(cit->second) << "." << endl;
                 myfile << "Error: Cyclic inheritance on class" << idtable.lookup(cit->second) << endl;
                 bCompError = true;
                 break;
             }

             for (cittest=CycleTree.begin(); cittest!=CycleTree.end(); ++cittest)
             {
                 myfile << "#" << test << " :";
                 myfile << "Name: " << idtable.lookup(cittest->first);
                 myfile << " Parent: " << idtable.lookup(cittest->second) << endl;
             }
         }
        }
      }
    }



    for (it=InhTree.begin(); it!=InhTree.end(); ++it)
    {
       myfile << "Object Name: " << idtable.lookup(it->first);
       myfile << " The parent is: " << idtable.lookup(it->second.get_parent_index()) << endl;
    }
    myfile.close();

}

std::map<int, Graph_Node> ClassTable::get_inh_tree()
{
   std::map<int, Graph_Node> pInhTree = InhTree;
   return (pInhTree);
}

Graph_Node* Graph_Node::Get_Parent()
{
    return nodeParent;
}

Graph_Node* Graph_Node::Copy()
{
    return new Graph_Node(*this);
}

int Graph_Node::get_name_index()
{
    return iName;
}

int Graph_Node::get_parent_index()
{
    return iParent;
}

void Graph_Node::Set_Parent(Graph_Node* parent)
{
    nodeParent = parent;
}

void Environment_class::add_formal(char* type)
{
    int iIndex = get_index(type);
    formalList->formals[formalList->index] = iIndex;
    formalList->index++;
}

void Environment_class::add_method_return(char* type)
{
    int iIndex = get_index(type);
    formalList->formals[0] = iIndex;
}

int Environment_class::add_method(char* name)
{
   int iMethod = get_index(name);
   FormalContainer* methodCheck = innerMethodMap->lookup(iMethod);

   if ( methodCheck != NULL ) {
      //TODO: error method redefinition
      return(iMethod);
      //cout << "Adding Method. Check. Method already in: " << iMethod << endl;
   } else {
      innerMethodMap->addid(iMethod, formalList);
      return(-1);
   }
}

int* Environment_class::return_signature(int Class, int Method)
{
   int schClass = Class;
   std::map<int, Graph_Node>::iterator it;
   int* signature = methodEnv->lookup(Class)->lookup(Method)->formals;
   
   while (signature == NULL) {
      it = pInhTree.find(schClass);
      schClass = it->second.get_parent_index();

      signature = methodEnv->lookup(schClass)->lookup(Method)->formals;

      if ( signature == NULL && schClass == get_index("Object") ) {
         //TODO: Error cannot find method in pEnv->get_current_class
         signature = NULL;
         break;
      }
   }

   return (signature);
}

bool Environment_class::m_main_check()
{
   bool check;
   FormalContainer* pMain = methodEnv->lookup(get_index("Main"))->lookup(get_index("main"));
   if (pMain == NULL) {
      check = true;
   } else {
      check = false;
   }
   return check;
}

void Environment_class::m_overload_check(int C, int* f)
{
   std::vector<int> L;
   std::vector<int>::iterator it;
   int *child_method, *parent_method;
   SymbolTable<int, FormalContainer>*m_class = methodEnv->lookup(C);
	
   get_ancestor_list(L, C, this);

   for(it=L.begin(); it<L.end(); it++) {
      for (int i = idtable.first(); idtable.more(i); i=idtable.next(i) ) {
         child_method = methodEnv->lookup(C)->lookup( get_index( idtable.lookup(i)->get_string() ))->formals;
         if (child_method != NULL) {
            parent_method = methodEnv->lookup(*it)->lookup( get_index(idtable.lookup(i)->get_string()) )->formals;
            if (parent_method != NULL) {
               for (int j=0; j<20; j++) {
                  if (child_method[j] != parent_method[j]) {
                     *f=i;
                     return;
                  }
               }
            }
        }
      }
   }

   *f=-1;
   return;		
}

int Environment_class::m_declare_check(int* c_C, int* m_M)
{
   int *method;
   SymbolTable<int, FormalContainer>*m_class; 
   std::map<int, Graph_Node>::iterator it;
   
   for (int i = idtable.first(); idtable.more(i); i=idtable.next(i) ) {
      m_class = methodEnv->lookup(i); /*looks for a class, from the whole ID Table*/
      if ( m_class != NULL ) {
         for (int j = idtable.first(); idtable.more(j); j=idtable.next(j) ) {
            method = m_class->lookup(j)->formals; /*looks for a method, in the class*/
            if ( method != NULL && method[0] != get_index("SELF_TYPE") ) {;
               it = pInhTree.find(method[0]); /*looks for return-type only*/
               if ( it == pInhTree.end() ) {
                  *c_C = i;
                  *m_M = method[0];
                  return (0);
               }
            }
            method = NULL;
         }
         m_class = NULL;
       }
    }
    return (-1);   
}

bool Environment_class::b_check()
{
   return branch_error;
}
 
void Environment_class::b_clear()
{
   branch_check.clear();
   branch_error = false;
}
 
void Environment_class::b_add(int iType)
{
   if (std::find(branch_check.begin(), branch_check.end(), iType) != branch_check.end()) {
      branch_error = true;
   } else {
      branch_check.push_back(iType);
   }
}

void Environment_class::print_by_class(ostream& myfile)
{ 
   int index;
   SymbolTable<int, FormalContainer>*singleMethod;
   SymbolTable<int, SymbolTable<int, FormalContainer > >*theMethodEnv;
   pFormal formalArgs;

   for (int i = 0; i<500; i++) {
      singleMethod = methodEnv->lookup(i);
      if ( singleMethod != NULL ) {
         cout << "Class: " << idtable.lookup(i) << endl;
         for (int j = 0; j<500; j++) {
            formalArgs = singleMethod->lookup(j);
            if (formalArgs != NULL) {
              cout << "  Method: " << idtable.lookup(j) << " ";
              for (int k = 0; k<20; k++) {
                 if (formalArgs->formals[k] != -1) {
                    index = formalArgs->formals[k];
                    cout << "    type" << k << ": " << idtable.lookup(index) << endl;
                 }
              }
            }
         }
      }
   }
}

int Environment_class::add_object(int name, int* type)
{
   int* test = objectEnv->probe(name);
   if ( test == NULL ) {
      objectEnv->addid(name, type);
      return(0);
   } else {
      //TODO: Error redefining an attr in the current scope
      return(-1);
   }
}

int Environment_class::lookup_object(int name)
{
   int* type = objectEnv->lookup(name);
   if (type==NULL) {
      //TODO: Error variable(object) not in objectEnv.
      return (-1);
   } else {
      return (*type);
   }
}

void Environment_class::enter_object_scope()
{
   objectEnv->enterscope();
}

void Environment_class::exit_object_scope()
{
   objectEnv->exitscope();
}

void Environment_class::clear_object_map()
{
   objectEnv = new SymbolTable<int, int>();
   objectEnv->enterscope();
}

void Environment_class::dump_objects()
{
   objectEnv->dump();
 cout << "Object Env of class: " << get_current_class() << endl;
for (int i = 0; i<50; i++) {
   int* type = objectEnv->lookup(i);
   if (type != NULL)
       cout<<"Variable name: "<<idtable.lookup(i)<<" Type: "<<idtable.lookup(*type)<<endl;
}

}

void Environment_class::add_class_methods(char* className)
{
   int iClass = get_index(className);
   methodEnv->addid(iClass, innerMethodMap);
   clear_inner_map();
}

void Environment_class::clear_inner_map()
{
   innerMethodMap = new SymbolTable<int, FormalContainer>();
   innerMethodMap->enterscope();
}

void Environment_class::clear_formals()
{
    formalList = new FormalContainer;
}

void ClassTable::install_basic_classes(Environment pEnv) {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
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
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
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
	       filename);

   ofstream myfile;
   myfile.open("dummy.txt");

   Classes basic_classes;
   Class_ classObj;
   Feature pFeature;

   basic_classes = single_Classes(Object_class);
   basic_classes = append_Classes(basic_classes, single_Classes(IO_class));
   basic_classes = append_Classes(basic_classes , single_Classes(Int_class));
   basic_classes = append_Classes(basic_classes , single_Classes(Bool_class));
   basic_classes = append_Classes(basic_classes , single_Classes(Str_class));

   pEnv->set_method_mode();
   for(int i = basic_classes->first(); basic_classes->more(i); i = basic_classes->next(i)) {
      classObj = basic_classes->nth(i);
      classObj->dump_test(myfile,pEnv,this);
   }
   pEnv->set_normal_mode();
   myfile.close();
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

void program_class::print_hierarchy(Class_ objClass, ostream& myfile, Environment pEnv, ClassTable* classtable)
{
    objClass->dump_test(myfile, pEnv, classtable);
    myfile << "  Higher Hierarchies " << endl;
    Class_ pClass = objClass;

    while (true) {

        if ( (get_index(pClass->get_parent()) == get_index("IO")) ||
             (get_index(pClass->get_parent()) == get_index("Object")) ) {
           myfile << "  Terminal Class IO or Object " << endl;
           break;
        } else {
           myfile << "  Higher Hierarchies " << endl;
           pClass = classes->nth(get_parent(pClass));
           pClass->dump_test(myfile, pEnv, classtable);
        }
    }
}

void program_class::construct_method_environment(ostream& myfile, Environment pEnv, ClassTable* classtable)
{
   Class_ classObj;
   for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
      classObj = classes->nth(i);
      classObj->dump_test(myfile, pEnv, classtable);
   }
   if (pEnv->m_main_check()) {
       //TODO: Error method main not defined in Class Main.
	   //classtable->get_filename(), this
       classtable->semant_error() << "Error method main not defined in Class Main."<<endl;
   }
   int C; int *f; int* c_C; int *c_M;

   f = new int(-1); 
   c_C = new int(-1); 
   c_M = new int(-1);
   
   for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
      classObj = classes->nth(i);
      C = get_index(classObj->get_name());
      pEnv->set_current_class( get_index(classObj->get_name()) );
      classtable->set_filename(classObj->get_filename());
      pEnv->m_overload_check(C,f);
      if (*f != -1) {
         //TODO: Error method f improperly redefined in class C
         classtable->semant_error(classtable->get_filename(), this)<<\
		     "Error Method: "<<idtable.lookup(*f)->get_string()<<\
			 " in Class: "<<idtable.lookup(C)->get_string()<<\
			 " is improperly overloaded (signature must match)."<<endl;
      }
      int  r_check = pEnv->m_declare_check(c_C, c_M);
      if ( r_check!=(-1) ) {
         classtable->semant_error(classtable->get_filename(), this)<<\
          "Error: in Class: "<<idtable.lookup(*c_C)->get_string()<<\
          ". [method] return type: "<<idtable.lookup(*c_M)->get_string()<<\
	      " in not defined." <<endl;	       
      }
   }	  
}

void program_class::construct_object_environment(ostream& myfile, Environment pEnv, ClassTable* classtable) 
{
   int currentClass = pEnv->get_current_class();
   int iClass;
   Class_ classObj;

   while (true) {
      for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
         classObj = classes->nth(i);
         iClass = get_index(classObj->get_name());
         if ( iClass == currentClass ) {
              classObj->dump_test(myfile, pEnv, classtable);
              break;
         }
      }
      currentClass = get_index(classObj->get_parent());
      if (currentClass == get_index("IO") || currentClass == get_index("Object")) {
         //pEnv->dump_objects();
         return;
      }
   }
}

int program_class::get_parent(Class_ pClass)
{
    Class_ classObj;
    char* parent = pClass->get_parent();
    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        classObj = classes->nth(i);
        if (strcmp( 
                   classObj->get_name(),
                   parent 
                  ) == 0 ) {
            return (i);
        }
    }
    return (NULL);
}

void class__class::dump_test(ostream& myfile, Environment pEnv, ClassTable* classtable)
{
    Feature pFeature;
    if (pEnv->is_method_mode()) {
       pEnv->clear_inner_map();
    }

    myfile << "Class " << name->get_string() << endl;
    for(int i = features->first(); features->more(i); i = features->next(i)) {
        pFeature = features->nth(i);
        pFeature->dump_test(myfile, pEnv, classtable);
    }
    if (pEnv->is_method_mode()) {
       pEnv->add_class_methods(name->get_string());
    }   
}

int class__class::type_check(Environment pEnv, ClassTable* classtable)
{
    Feature pFeature;

    for(int i = features->first(); features->more(i); i = features->next(i)) {
        pFeature = features->nth(i);
        pFeature->type_check(pEnv, classtable);
    }
    return (-1);
   //attr, method and class return values are never counted on
   //therefore i think they can be -1 (for safety).
}

void attr_class::dump_test(ostream& myfile, Environment pEnv, ClassTable* classtable)
{
    myfile << "  attr - " << name->get_string() << " : " << type_decl->get_string() << endl;
    if (pEnv->is_object_mode()) {
       int iName = get_index( name->get_string() );

       int* pType = new int ( get_index(type_decl->get_string()) );
	   
	   if ( iName==get_index("self") ) {
          classtable->semant_error(classtable->get_filename(), this)<< \
           "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
           ". [attr] Trying to add a variable named self."<<endl;	   
	   }
	   
       int attr_check = pEnv->add_object(iName, pType);
       if ( attr_check==(-1) ) {
          classtable->semant_error(classtable->get_filename(), this)<< \
           "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
           ". [attr] trying to add a variable in the current scope: " << name->get_string() <<endl;
       } 
    }
}

int attr_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int T0 = get_index( (type_decl->get_string()) );
   pEnv->enter_object_scope();
   int* pType = new int(get_index("SELF_TYPE"));
   
   int attr_check = pEnv->add_object( get_index("self"), pType );
   if ( attr_check==(-1) ) {
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [attr] trying to add a variable in the current scope: self" <<endl;
   }   
   
   int T1 = init->type_check(pEnv, classtable);

   if ( T1 == -1 ) { /* [attr-no-init] */
      return (-1);
   } else {            /* [attr-init] */
      bool check = subclass(T1,T0,pEnv);
      pEnv->exit_object_scope();
      if (!check) {
         //TODO: error attr expression improperly assigned to parent->child
         classtable->semant_error(classtable->get_filename(), this)<< \
          "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
          ". [attr] attribute improperly assigned: " << name->get_string() <<endl;
      }
      return (-1); //attr, method and class return values are never counted on
      //therefore i think they can be -1 (for safety).
   }
}

void method_class::dump_test(ostream& myfile, Environment pEnv, ClassTable* classtable)
{
    Formal pFormal;
    if (pEnv->is_method_mode()) {
       pEnv->clear_formals();
    }

    myfile << "  method - " << name->get_string() << " ( " ;
    for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
        pFormal = formals->nth(i);
        pFormal->dump_test(myfile, pEnv, classtable);
    }
    myfile << " ) : " << return_type->get_string() << endl;

    if (pEnv->is_method_mode()) {
       pEnv->add_method_return(return_type->get_string());

       int n_check = pEnv->add_method(name->get_string());
       if (n_check != -1) {
          classtable->semant_error()<<"Error: re-adding method: "<<idtable.lookup(n_check) \
            <<" in Class: "<<idtable.lookup(pEnv->get_current_class())<<endl;
       }
    }
}

int method_class::type_check(Environment pEnv, ClassTable* classtable)
{
   Formal pFormal;
   pEnv->enter_object_scope();
   int* pType = new int(get_index("SELF_TYPE"));
   
   int attr_check = pEnv->add_object( get_index("self"), pType );
   if ( attr_check==(-1) ) {
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [method] trying to add a variable in the current scope: self." <<endl;
   }    
   
   for(int i = formals->first(); formals->more(i); i = formals->next(i)) {      
       pFormal = formals->nth(i);
       pFormal->type_check(pEnv, classtable);
   }

   int T0prime = expr->type_check(pEnv, classtable); 
  
   int T0 = get_index( return_type->get_string() );
   bool check;
   pEnv->exit_object_scope();
   if (T0 == get_index("SELF_TYPE")) {
      check = subclass(T0prime, get_index("SELF_TYPE"), pEnv);
   } else {
      check = subclass(T0prime, T0 ,pEnv);
   }
   if (!check) {
      //TODO: Error T0prime is not a subclass of T0. aka the method's body doesn't return a subtype of the declared return
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [method] Method's body does not return a subclass of the declared return."<<endl;
   }
   return (-1); 
   //attr, method and class return values are never counted on
   //therefore i think they can be -1 (for safety).
}

void formal_class::dump_test(ostream& myfile, Environment pEnv, ClassTable* classtable)
{
    myfile << name->get_string() << " : " << type_decl->get_string();
    if (pEnv->is_method_mode()) {
       pEnv->add_formal(type_decl->get_string());
    }
}

int formal_class::type_check(Environment pEnv, ClassTable* classtable)
{
   if ( get_index(type_decl->get_string())==get_index("SELF_TYPE") ) {
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [formal] SELF_TYPE cannot be used as a type in a formal parameter."<<endl;      
   }
   
   int* pType = new int ( get_index(type_decl->get_string()) );
   
   int attr_check = pEnv->add_object( get_index(name->get_string()), pType );
   if ( attr_check==(-1) ) {
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [formal] trying to add a variable in the current scope: " << name->get_string() <<endl;
   }   

   return (-1);
   //attr, method and class return values are never counted on
   //therefore i think they can be -1 (for safety). Also Formal
   //isn't counted on to return a value.
}

//Expressions begin (each have a "type" that must be set)
int assign_class::type_check(Environment pEnv, ClassTable* classtable)
{
   if ( get_index(name->get_string())==get_index("self") ) {
       classtable->semant_error(classtable->get_filename(), this)<< \
        "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
        ". [assign] Illegal assignment to 'self'." <<endl;      
   }

   int T = pEnv->lookup_object( get_index(name->get_string()) );
   if ( T==(-1) ){
       classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [assign] cannot find variable: " <<name->get_string()<<" in Object Environment."<<endl;
	   T = get_index("Object");
   }
   
   int Tprime = expr->type_check(pEnv, classtable);
   bool check = subclass(Tprime, T, pEnv);
   if (!check) {
       //TODO: Error [assign] expression does not return a subtype that fits into the declared varaible
       classtable->semant_error(classtable->get_filename(), this)<< \
        "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
        ". [assign] does not return a subtype that fits into variable: " << name->get_string() <<endl;
	   set_type(idtable.lookup_string("Object"));
	   return get_index("Object");
   } else {
       set_type(idtable.lookup(Tprime)); //TODO: careful with this make sure it's setting it right
       return Tprime;
   }
}

int bool_const_class::type_check(Environment pEnv, ClassTable* classtable)
{
   set_type(idtable.lookup_string("Bool"));
   return get_index("Bool");
}

int int_const_class::type_check(Environment pEnv, ClassTable* classtable)
{
   set_type(idtable.lookup_string("Int"));
   return get_index("Int");
}

int string_const_class::type_check(Environment pEnv, ClassTable* classtable)
{
   set_type(idtable.lookup_string("String"));
   return get_index("String");
}

int new__class::type_check(Environment pEnv, ClassTable* classtable)
{
   int Tprime;
   if ( get_index(type_name->get_string()) == get_index("SELF_TYPE") ) {
      Tprime = get_index("SELF_TYPE");
   } else {
      Tprime = get_index(type_name->get_string());
   }
   set_type(idtable.lookup(Tprime));
   return Tprime;
}

int dispatch_class::type_check(Environment pEnv, ClassTable* classtable)
{
   Expression expression;
   int T0prime, returnPosition, Tn_p1;
   int* args = new int [20];
   int* signature;
   bool check;
   
   memset (args,-1, 20*sizeof(int) ); //TODO: be very careful and test this

   int T0 = expr->type_check(pEnv, classtable);

   if ( T0 == get_index("SELF_TYPE") ) {
      T0prime = pEnv->get_current_class();
   } else {
      T0prime = T0;
   }
   for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
        expression = actual->nth(i);
        args[i+1] = expression->type_check(pEnv, classtable);
   }

   signature = pEnv->return_signature( T0prime, get_index(name->get_string()) );
   if (signature != NULL) {
     for (int i = 1; i<20; i++) {
         if (args[1]==(-1) && signature[1]==(-1)) {
            break; /*function has no arguments */
         } else if (args[i]==(-1) && signature[i]==(-1)) {
            break; /*function's signature and call match on arguments */
         } if (args[i]==(-1) || signature[i]==(-1)) {
            //TODO: Error dispatch called with wrong number of arguments
             classtable->semant_error(classtable->get_filename(), this)<< \
              "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
              ". [dispatch] method: " << name->get_string()<<" called with wrong number of arguments (too many, or not enough)." <<endl;
            break;
         }
         check = subclass(args[i], signature[i], pEnv);
  	     if (!check) {
  	       //TODO: Error arg[i] does not conform to signature[i]
           classtable->semant_error(classtable->get_filename(), this)<< \
            "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
            ". [dispatch] Argument used in call does not conform to the signature's type."<<endl;
         }
     }
     for (int i=1; i<20; i++) {
        if (args[i] == -1) {
           returnPosition = i-1;
           break;
        }
     }

     if ( signature[0] == get_index("SELF_TYPE") ) {
        Tn_p1 = T0;
     } else {
        Tn_p1 = signature[0];
     }

     set_type(idtable.lookup(Tn_p1));
     return Tn_p1;
   } else {
     classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [dispatch] cannot find Method: " << name->get_string() <<endl;
     set_type(idtable.lookup(get_index("Object")));
     return get_index("Object");
   }
}

int static_dispatch_class::type_check(Environment pEnv, ClassTable* classtable)
{
   Expression expression;
   int T0prime, returnPosition, Tn_p1;
   int* args = new int [20];
   int* signature;
   bool check;

   memset (args,-1, 20*sizeof(int) ); //TODO: be very careful and test this
   
   int T0 = expr->type_check(pEnv, classtable);
   check = subclass( T0, get_index(type_name->get_string()) , pEnv);
   if (!check) {
      //TODO: Error expression (e0) is not a subclass of type (@Type) called.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [static-dispatch] Expression's type is not a subtype of '@TYPE'."<<endl;
   }
   for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
        expression = actual->nth(i);
        args[i+1] = expression->type_check(pEnv, classtable);
   }

   signature = pEnv->return_signature( get_index(type_name->get_string()),
                                       get_index(name->get_string()) );
   if (signature != NULL) {
      for (int i = 1; i<20; i++) {
          if (args[1]==(-1) && signature[1]==(-1)) {
             break; /*function has no arguments */
          } else if (args[i]==(-1) && signature[i]==(-1)) {
             break; /*function's signature and call match on arguments */
          } else if (args[i]==(-1) || signature[i]==(-1)) {
             //TODO: Error dispatch called with wrong number of arguments (too many, or not enough)
             classtable->semant_error(classtable->get_filename(), this)<< \
              "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
              ". [static-dispatch] method: " << name->get_string()<<" called with wrong number of arguments (too many, or not enough)." <<endl;
             break;
          }
          check = subclass(args[i], signature[i], pEnv);
             if (!check) {
                //TODO: Error arg[i] does not conform to signature[i]
                classtable->semant_error(classtable->get_filename(), this)<< \
                 "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
                 ". [static-dispatch] Argument used in call does not conform to the signature's type."<<endl;
          }
      }
      if ( signature[0] == get_index("SELF_TYPE") ) {
         Tn_p1 = T0;
      } else {
         Tn_p1 = signature[0];
      }
      set_type(idtable.lookup(Tn_p1));
      return Tn_p1;
   } else {
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [static-dispatch] cannot find Method: " << name->get_string() <<endl;
      set_type(idtable.lookup(get_index("Object")));
      return get_index("Object");
   }
}

int cond_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int boolean = pred->type_check(pEnv, classtable);
   if (boolean != get_index("Bool")) {
      //TODO: Error if statement predicate is not of type bool.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [if] If statement's predicate is not of type BOOL."<<endl;
   }
   int T2 = then_exp->type_check(pEnv, classtable);
   int T3 = else_exp->type_check(pEnv, classtable);
   int lub_result = lub(T2, T3, pEnv);
   set_type(idtable.lookup(lub_result));
   return lub_result;
}

int block_class::type_check(Environment pEnv, ClassTable* classtable)
{
   Expression pExpr;
   int Tn;
   for(int i = body->first(); body->more(i); i = body->next(i)) {
       pExpr = body->nth(i);
       Tn = pExpr->type_check(pEnv, classtable);
   }
   set_type(idtable.lookup(Tn));
   return Tn;
}

int let_class::type_check(Environment pEnv, ClassTable* classtable)
{
   if ( get_index(identifier->get_string())==get_index("self") ) {
         classtable->semant_error(classtable->get_filename(), this)<< \
          "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
          ". [let] Identifier in let has an illegal name 'self'." <<endl;       
   }

   int T0 = get_index( (type_decl->get_string()) );
   int T0prime, attr_check;
   if (T0 == get_index("SELF_TYPE")) {
      T0prime = get_index("SELF_TYPE");
   } else {
      T0prime = T0;
   }
  
   int T1 = init->type_check(pEnv, classtable); 

   //this could return no_expr. Therefore this is the [let-no-init] branch
   if (T1 == (-1))  { /* -1 is the return for the _no_expr expression */
      pEnv->enter_object_scope();
      int* pType = new int(T0prime);
	  
      attr_check = pEnv->add_object( get_index(identifier->get_string()), pType );
      if ( attr_check==(-1) ) {
         classtable->semant_error(classtable->get_filename(), this)<< \
          "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
          ". [let-no-init] trying to add a variable in the current scope: " << identifier->get_string() <<endl;
      }
	  
	  T1 = body->type_check(pEnv, classtable);
	  pEnv->exit_object_scope();
	  
	  set_type(idtable.lookup(T1));
	  return T1;
   } else {
      bool check = subclass(T1, T0prime, pEnv);
	  if (!check) {
	     //TODO: Error let expression's initializer isn't a subclass of the declared type
         classtable->semant_error(classtable->get_filename(), this)<< \
          "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
          ". [let-init] Let expression's initializer isn't a subclass of the declared type."<<endl;		 		 
	  }
      pEnv->enter_object_scope();
      int* pType = new int(T0prime);
	  
      attr_check = pEnv->add_object( get_index(identifier->get_string()), pType );
      if ( attr_check==(-1) ) {
         classtable->semant_error(classtable->get_filename(), this)<< \
          "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
          ". [let-init] trying to add a variable in the current scope: " << identifier->get_string() <<endl;
      } 	  
	  
      int T2 = body->type_check(pEnv, classtable);
      pEnv->exit_object_scope();
	  
      set_type(idtable.lookup(T2));
      return T2;	  
   }
}

int typcase_class::type_check(Environment pEnv, ClassTable* classtable)
{
   Case pCase;
   int Tn, Tn_accumulate;
   int T0 = expr->type_check(pEnv, classtable);

  
   for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
       pCase = cases->nth(i);
       Tn = pCase->type_check(pEnv, classtable);
       if (i == cases->first()) {
          Tn_accumulate = Tn;
       }
       Tn_accumulate = lub(Tn,Tn_accumulate,pEnv);
	   
	   if ( pEnv->b_check() ) {
          classtable->semant_error(classtable->get_filename(), this)<< \
           "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
           ". [branch] branch of the case statement contains duplicate Type: " << idtable.lookup(Tn)->get_string() <<endl;	   
	   }
   }
   pEnv->b_clear();
   
   set_type(idtable.lookup(Tn_accumulate));
   return Tn_accumulate;
}

int branch_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int Tn, attr_check;
   
   pEnv->enter_object_scope();
   int* pType = new int( get_index(type_decl->get_string()) );
   
   attr_check = pEnv->add_object( get_index(name->get_string()), pType );
   if ( attr_check==(-1) ) {
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [branch] trying to add a variable in the current scope: " << name->get_string() <<endl;
   }
   pEnv->b_add( get_index(type_decl->get_string()) );   
   
   Tn = expr->type_check(pEnv, classtable);
   pEnv->exit_object_scope();

   return Tn;   
}

int loop_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int T_pred = pred->type_check(pEnv, classtable);
   if ( T_pred != get_index("Bool") ) {
      //TODO: Error look predicate is not a boolean
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [loop] The loop's predicate must be of type BOOL."<<endl;
   }
   int T2 = body->type_check(pEnv, classtable);
   set_type(idtable.lookup_string("Object"));
   return get_index("Object");
}

int isvoid_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int T1 = e1->type_check(pEnv, classtable);
   set_type(idtable.lookup_string("Bool"));
   return get_index("Bool");
}

//TODO: Note the comp_class always returns Bool (we know this, so it only reports errors).
int comp_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int T = e1->type_check(pEnv, classtable);
   if ( T != get_index("Bool") ) {
      //TODO: Error NOT expression not used on a BOOL
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [comp] Not expression must be used on a type BOOL."<<endl;
   }
   set_type(idtable.lookup_string("Bool"));
   return get_index("Bool");
}

int lt_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int T1 = e1->type_check(pEnv, classtable);
   if ( T1 != get_index("Int") ) {
      //TODO: Error left side of [less than] operator is not Type Int.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [lt] Left side of less-than operator is not type INT."<<endl;
   }
   int T2 = e2->type_check(pEnv, classtable);
   if ( T2 != get_index("Int") ) {
      //TODO: Error right side of [less than] operator is not Type Int.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [lt] Right side of less-than operator is not type INT."<<endl;
   }
   set_type(idtable.lookup_string("Bool"));
   return get_index("Bool");
}

int leq_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int T1 = e1->type_check(pEnv, classtable);
   if ( T1 != get_index("Int") ) {
      //TODO: Error left side of [less than equal to] operator is not Type Int.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [leq] Left side of less-than-equal-to operator is not type INT."<<endl;
   }
   int T2 = e2->type_check(pEnv, classtable);
   if ( T2 != get_index("Int") ) {
      //TODO: Error right side of [less than equal to] operator is not Type Int.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [leq] Right side of less-than-equal-to operator is not type INT."<<endl;
   }
   set_type(idtable.lookup_string("Bool"));
   return get_index("Bool");   
}

int neg_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int T1 = e1->type_check(pEnv, classtable);
   if ( T1 != get_index("Int") ) {
      //TODO: Error [not] operator is not Type Int.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [complement] Complement operator is not used on a type INT."<<endl;
   }
   set_type(idtable.lookup_string("Int"));
   return get_index("Int"); 
}

int plus_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int T1 = e1->type_check(pEnv, classtable);
   if ( T1 != get_index("Int") ) {
      //TODO: Error left side of [plus] operator is not Type Int.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [plus] Left side of addition operator is not type INT."<<endl;
   }
   int T2 = e2->type_check(pEnv, classtable);
   if ( T2 != get_index("Int") ) {
      //TODO: Error right side of [plus] operator is not Type Int.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [plus] Right side of addition operator is not type INT."<<endl;
   }
   set_type(idtable.lookup_string("Int"));
   return get_index("Int");    
}

int sub_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int T1 = e1->type_check(pEnv, classtable);
   if ( T1 != get_index("Int") ) {
      //TODO: Error left side of [minus] operator is not Type Int.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [subtract] Left side of subtract operator is not type INT."<<endl;
   }
   int T2 = e2->type_check(pEnv, classtable);
   if ( T2 != get_index("Int") ) {
      //TODO: Error right side of [minus] operator is not Type Int.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [subtract] Right side of subtract operator is not type INT."<<endl;
   }
   set_type(idtable.lookup_string("Int"));
   return get_index("Int");  
}

int mul_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int T1 = e1->type_check(pEnv, classtable);
   if ( T1 != get_index("Int") ) {
      //TODO: Error left side of [multiply] operator is not Type Int.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [multiply] Left side of multiply operator is not type INT."<<endl;
   }
   int T2 = e2->type_check(pEnv, classtable);
   if ( T2 != get_index("Int") ) {
      //TODO: Error right side of [multiply] operator is not Type Int.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [multiply] Right side of multiply operator is not type INT."<<endl;
   }
   set_type(idtable.lookup_string("Int"));
   return get_index("Int");
}

int divide_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int T1 = e1->type_check(pEnv, classtable);
   if ( T1 != get_index("Int") ) {
      //TODO: Error left side of [divide] operator is not Type Int.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [divide] Left side of divide operator is not type INT."<<endl;
   }
   int T2 = e2->type_check(pEnv, classtable);
   if ( T2 != get_index("Int") ) {
      //TODO: Error right side of [divide] operator is not Type Int.
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [divide] Right side of divide operator is not type INT."<<endl;
   }
   set_type(idtable.lookup_string("Int"));
   return get_index("Int");
}

int eq_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int T1 = e1->type_check(pEnv, classtable);
   int T2 = e2->type_check(pEnv, classtable);
   
   if ( T1 == get_index("String") || T2 == get_index("String") ) {
      if (T1 != get_index("String") || T2 != get_index("String")) {
	     //TODO: Error when using [compare] both variables must be String.
	     classtable->semant_error(classtable->get_filename(), this)<< \
          "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
          ". [compare] When using compare both variables must be STRING."<<endl;
      }
   }
   if ( T1 == get_index("Int") || T2 == get_index("Int") ) {
      if (T1 != get_index("Int") || T2 != get_index("Int")) {
         //TODO: Error when using [compare] both variables must be Int.
	     classtable->semant_error(classtable->get_filename(), this)<< \
          "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
          ". [compare] When using compare both variables must be INT."<<endl;
	  }
   }
   if ( T1 == get_index("Bool") || T2 == get_index("Bool") ) {
      if (T1 != get_index("Bool") || T2 != get_index("Bool")) {
         //TODO: Error when using [compare] both variables must be Bool.
	     classtable->semant_error(classtable->get_filename(), this)<< \
          "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
          ". [compare] When using compare both variables must be BOOL."<<endl;
      }
   }
   set_type(idtable.lookup_string("Bool"));
   return get_index("Bool");
}

int object_class::type_check(Environment pEnv, ClassTable* classtable)
{
   int T = pEnv->lookup_object( get_index(name->get_string()) );
   if ( T==(-1) ) {
      classtable->semant_error(classtable->get_filename(), this)<< \
       "Error: in Class: "<<idtable.lookup(pEnv->get_current_class())->get_string()<<\
       ". [object] cannot find: " <<name->get_string()<<" in Object Environment." <<endl;
      set_type(idtable.lookup_string("Object"));
	  return get_index("Object");
   } else {
      set_type(idtable.lookup(T));
      return T;
   }   
}

int no_expr_class::type_check(Environment pEnv, ClassTable* classtable)
{
   return -1;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
   initialize_constants();
   ofstream myfile;
   myfile.open("example.txt", std::ofstream::app);

   /* ClassTable constructor may do some semantic analysis */
   ClassTable *classtable = new ClassTable(classes);

   if ( !classtable->errors() ) {

      /* some semantic analysis code may go here */
      std::map<int, Graph_Node> pInhTree = classtable->get_inh_tree();
      Environment pEnv = new Environment_class(pInhTree);
      classtable->install_basic_classes(pEnv);
      pEnv->set_method_mode();
      construct_method_environment(myfile, pEnv, classtable);
      pEnv->set_normal_mode();
      //pEnv->print_by_class(myfile);

      Class_ classObj;
      for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
         classObj = classes->nth(i);
         pEnv->set_current_class( get_index(classObj->get_name()) );
         classtable->set_filename(classObj->get_filename());
//if (i==0){
         pEnv->clear_object_map();
         pEnv->set_object_mode();
         construct_object_environment(myfile, pEnv, classtable);
         pEnv->set_normal_mode();
         classObj->type_check(pEnv, classtable);
//}
      }   


      for (int i = idtable.first(); idtable.more(i); i=idtable.next(i) ) {
         // cout << "#: " << i << " Entry: " << idtable.lookup(i)->get_string() << endl;
      }

      myfile.close();
      // cout << "FINISH" <<endl;
   }
   if (classtable->errors()) {
      cerr << "Compilation halted due to static semantic errors." << endl;
      exit(1);
   }
}