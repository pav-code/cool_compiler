#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <vector>
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
      if ( strncmp(fromTable, name, strlen(name)) == 0 ) {
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

void Environment_class::add_method(char* name)
{
   int iMethod = get_index(name);
   FormalContainer* methodCheck = innerMethodMap->lookup(iMethod);

   if ( methodCheck != NULL ) {
      //TODO: error method redefinition
      cout << "Adding Method. Check. Method already in: " << iMethod << endl;
   } else {
//      cout << "now adding a method to innerMethodMap: " << iMethod << endl;
      innerMethodMap->addid(iMethod, formalList);  
   }
}

int* Environment_class::return_signature(int Class, int Method)
{
 // SymbolTable<int, FormalContainer> sTemp = methodEnv->lookup(Class);
 // FormalContainer fTemp = sTemp->lookup(Method);
 // int* signature = fTemp.formals;
    int* signature = methodEnv->lookup(Class)->lookup(Method)->formals;
	return (signature);
}

void Environment_class::print_by_class(ostream& myfile)
{ 
   int index;
   SymbolTable<int, FormalContainer>*singleMethod;
   SymbolTable<int, SymbolTable<int, FormalContainer > >*theMethodEnv;
   pFormal formalArgs;

   for (int i = 0; i<50; i++) {
      singleMethod = methodEnv->lookup(i);
      if ( singleMethod != NULL ) {
         cout << "Class: " << idtable.lookup(i) << endl;
         for (int j = 0; j<50; j++) {
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


//   methodEnv->dump();
//   if (methodReturn != NULL) {
//      cout << "we are in the map supposedly" << endl;
//      //methodReturn->dump();
//      pFormal pfTest = methodReturn->lookup(25);
//      for (int i = 0; i<20;i++){
//         if (pfTest->formals[i] != -1) {
//            index = pfTest->formals[i];
//            cout << "Formal#: " << i << " type: " << idtable.lookup(index) << endl;
//         }
//      }
//   } else {
//      cout << "we cannot find the _main_ entry" << endl;
//   }
//for (int i = idtable.first(); idtable.more(i); i=idtable.next(i) ) {
//   cout << "#: " << i << " Entry: " << idtable.lookup(i)->get_string() << endl;
//}

}

void Environment_class::add_object(int name, int* type)
{
   int* test = objectEnv->probe(name);
   if ( test == NULL ) {
      objectEnv->addid(name, type);
   } else {
      //TODO: Error redefining an attr in the current scope
      cout << "Error Trying to redefine an attr in current scope" <<endl;
   }
}

int Environment_class::lookup_object(int name)
{
   int* type = objectEnv->lookup(name);
   if (type==NULL) {
      //TODO: Error variable(object) not in objectEnv.
      cout<<"Error: variable(object) not in objectEnv."<<endl;
      return get_index("Object");
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

/*
int Environment_class::get_index(char* name)
{
   char* fromTable;
   for (int i = idtable.first(); idtable.more(i); i=idtable.next(i) ) {
      fromTable = idtable.lookup(i)->get_string();
      if ( strncmp(fromTable, name, strlen(name)) == 0 ) {
         return (i);
      }
   }
   return (NULL);
}
*/

void ClassTable::install_basic_classes() {

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
}


/*
void ClassTable::print_class(Class_ pClass)
{
    ofstream myfile;
    myfile.open("example.txt", std::ofstream::app);
    Features pFeatures = pClass->get_features();
    Feature pFeature;

//pFeature = pFeatures->nth(0);

//    for (int i = pFeatures->first(); pFeatures->more(i); pFeatures->next(i) ) {
//        pFeature = pFeatures->nth(i);
//        if ( strcmp(pFeature->feature_type(), "method") == 0 ) {
//            myfile << "method: " << pFeature->get_name() << "( ";
//            print_formals(pFeature->get_formals(), myfile);
//            myfile << " ). Return Type: " << pFeature->get_return_type() << endl;
//        } else if ( strcmp(pFeature->feature_type(), "attr") == 0 ) {
//            myfile << "attr: " << pFeature->get_name() << " : " << pFeature->get_type() << endl;
//        }
//    }

    myfile.close();
}

void ClassTable::print_formals(Formals pFormals, ofstream& myfile)
{
    Formal pFormal;
    for (int i = pFormals->first(); pFormals->more(i); pFormals->next(i)) {
        pFormal = pFormals->nth(i);
        myfile << "Formal-" << pFormal->get_name() << " : " << pFormal->get_type() << "," << endl;
    }
}
*/
/*
int ClassTable::get_index(char* name)
{
   char* fromTable;
   for (int i = idtable.first(); idtable.more(i); i=idtable.next(i) ) {
      fromTable = idtable.lookup(i)->get_string();
      if ( strncmp(fromTable, name, strlen(name)) == 0 ) {
         return (i);
      }
   }
   return (NULL);
}
*/

/* check this cause you talking to wen (cannot get a child)*/
/*
int ClassTable::get_child(int P)
{
   if  (InhTree == NULL) {
      return -1;
   }
   std::map<char*, Graph_Node>::iterator it;

   for (it=InhTree.begin(); it!=InhTree.end(); it++) {
      if ( P == get_index(it->first) ) {
         return ( get_index(it->second.Get) );
      }
   }
}
*/

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

void program_class::print_hierarchy(Class_ objClass, ostream& myfile, Environment pEnv)
{
    objClass->dump_test(myfile, pEnv);
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
           pClass->dump_test(myfile, pEnv);
        }
    }
}

void program_class::construct_method_environment(ostream& myfile, Environment pEnv)
{
   Class_ classObj;
   for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
      classObj = classes->nth(i);
      classObj->dump_test(myfile, pEnv);
   }
}

void program_class::construct_object_environment(ostream& myfile, Environment pEnv) 
{
   int currentClass = pEnv->get_current_class();
   int iClass;
   Class_ classObj;

   while (true) {
      for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
         classObj = classes->nth(i);
         iClass = get_index(classObj->get_name());
         if ( iClass == currentClass ) {
              classObj->dump_test(myfile, pEnv);
              break;
         }
      }
cout << "currentClass: " << currentClass<<endl;
      currentClass = get_index(classObj->get_parent());
cout << "currentClass's parent: " << currentClass<<endl;
      if (currentClass == get_index("IO") || currentClass == get_index("Object")) {
pEnv->dump_objects();
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

/*
//int program_class::get_index(char* name)
//{
//   char* fromTable;
//   for (int i = idtable.first(); idtable.more(i); i=idtable.next(i) ) {
//      fromTable = idtable.lookup(i)->get_string();
//      if ( strncmp(fromTable, name, strlen(name)) == 0 ) {
//         return (i);
//      }
//   }
//   return (NULL);//
//}
*/

void class__class::dump_test(ostream& myfile, Environment pEnv)
{
    Feature pFeature;
    if (pEnv->is_method_mode()) {
       pEnv->clear_inner_map();
    }

    myfile << "Class " << name->get_string() << endl;
    for(int i = features->first(); features->more(i); i = features->next(i)) {
        pFeature = features->nth(i);
        pFeature->dump_test(myfile, pEnv);
    }
    if (pEnv->get_mode() == pEnv->is_method_mode()) {
       pEnv->add_class_methods(name->get_string());
    }
}

int class__class::type_check(Environment pEnv)
{
    Feature pFeature;

    for(int i = features->first(); features->more(i); i = features->next(i)) {
        pFeature = features->nth(i);
        pFeature->type_check(pEnv);
    }
    return (-1);
//attr, method and class return values are never counted on
//therefore i think they can be -1 (for safety).
}

void attr_class::dump_test(ostream& myfile, Environment pEnv)
{
    myfile << "  attr - " << name->get_string() << " : " << type_decl->get_string() << endl;
    if (pEnv->is_object_mode()) {
       int iName = get_index( name->get_string() );

       int* pType = new int ( get_index(type_decl->get_string()) );
       pEnv->add_object(iName, pType);
    }
}

int attr_class::type_check(Environment pEnv)
{
   int T0 = get_index( (type_decl->get_string()) );
   pEnv->enter_object_scope();
   int* pType = new int(get_index("SELF_TYPE"));
   pEnv->add_object( get_index("self"), pType );
   int T1 = init->type_check(pEnv);

   if ( T1 == -1 ) { /* [attr-no-init] */
      return (-1);
   } else {            /* [attr-init] */
      bool check = subclass(T1,T0,pEnv);
      pEnv->exit_object_scope();
      if (!check) {
         //TODO: error attr expression improperly assigned to parent->child
         cout<<"Error attr expression improperly assigned to parent->child"<<endl;
      }
      return (-1); //attr, method and class return values are never counted on
      //therefore i think they can be -1 (for safety).
   }
}

void method_class::dump_test(ostream& myfile, Environment pEnv)
{
    Formal pFormal;
    if (pEnv->is_method_mode()) {
       pEnv->clear_formals();
    }

    myfile << "  method - " << name->get_string() << " ( " ;
    for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
        pFormal = formals->nth(i);
        pFormal->dump_test(myfile, pEnv);
    }
    myfile << " ) : " << return_type->get_string() << endl;

    if (pEnv->is_method_mode()) {
       pEnv->add_method_return(return_type->get_string());
       pEnv->add_method(name->get_string());
    }
}

int method_class::type_check(Environment pEnv)
{
cout<<"HELLOOO!i am in method's type check!"<<endl;

   Formal pFormal;
   pEnv->enter_object_scope();
   int* pType = new int(get_index("SELF_TYPE"));
   pEnv->add_object( get_index("self"), pType );
   
   for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
       pFormal = formals->nth(i);
       //pFormal->type_check(pEnv);
   }
   
   //int T0prime = expr->type_check(pEnv);
int T0prime=0;
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
	  cout<<"Error T0prime is not a subclass of T0. aka the method's body doesn't return a subtype of the declared return"<<endl;
   }
   return (-1); 
//attr, method and class return values are never counted on
//therefore i think they can be -1 (for safety).
}

void formal_class::dump_test(ostream& myfile, Environment pEnv)
{
    myfile << name->get_string() << " : " << type_decl->get_string();
    if (pEnv->is_method_mode()) {
       pEnv->add_formal(type_decl->get_string());
    }
}

int formal_class::type_check(Environment pEnv)
{
   int* pType = new int ( get_index(type_decl->get_string()) );
   pEnv->add_object( get_index(name->get_string()), pType );
   return (-1);
//attr, method and class return values are never counted on
//therefore i think they can be -1 (for safety). Also Formal
//isn't counted on to return a value.
}

//Expressions begin (each have a "type" that must be set)
int assign_class::type_check(Environment pEnv)
{
   int T = pEnv->lookup_object( get_index(name->get_string()) );
   int Tprime = expr->type_check(pEnv);
   bool check = subclass(Tprime, T, pEnv);
   if (!check) {
       //TODO: Error [assign] expression does not return a subtype that fits into the declared varaible
	   cout<<"Error [assign] expression does not return a subtype that fits into the declared varaible"<<endl;
	   set_type(idtable.lookup_string("Object"));
	   return get_index("Object");
   } else {
       set_type(idtable.lookup(Tprime)); //TODO: careful with this make sure it's setting it right
       return Tprime;
   }
}

int bool_const_class::type_check(Environment pEnv)
{
   set_type(idtable.lookup_string("Bool"));
   return get_index("Bool");
}

int int_const_class::type_check(Environment pEnv)
{
   set_type(idtable.lookup_string("Int"));
   return get_index("Int");
}

int string_const_class::type_check(Environment pEnv)
{
   set_type(idtable.lookup_string("String"));
   return get_index("String");
}

int new__class::type_check(Environment pEnv)
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

int dispatch_class::type_check(Environment pEnv)
{
   Expression expression;
   int T0prime, returnPosition, Tn_p1;
   int* args = new int [20];
   int* signature;
   bool check;
   
   memset (args,-1, sizeof(args)*sizeof(int) ); //TODO: be very careful and test this
   int T0 = expr->type_check(pEnv);
   if ( T0 == get_index("SELF_TYPE") ) {
      T0prime = pEnv->get_current_class();
   } else {
      T0prime = T0;
   }
   for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
        expression = actual->nth(i);
        args[i+1] = expression->type_check(pEnv);
   }
   signature = pEnv->return_signature( T0prime, get_index(name->get_string()) );
   for (int i = 1; i<20; i++) {
       check = subclass(args[i], signature[i], pEnv);
	   if (!check) {
	       //TODO: Error arg[i] does not conform to signature[i]
		   cout<<"TODO: Error arg[i] does not conform to signature[i]"<<endl;
	   }
   }
   for (int i=1; i<20; i++) {
      if (args[i] == -1) {
	     returnPosition = i-1;
	  }
   }
   if ( T0 == get_index("SELF_TYPE") ) {
      Tn_p1 = T0;
   } else {
      Tn_p1 = args[returnPosition];
   }
   set_type(idtable.lookup(Tn_p1));
   return Tn_p1;
}

int static_dispatch_class::type_check(Environment pEnv)
{
   Expression expression;
   int T0prime, returnPosition, Tn_p1;
   int* args = new int [20];
   int* signature;
   bool check;
   
   int T0 = expr->type_check(pEnv);
   check = subclass( T0, get_index(type_name->get_string()) , pEnv);
   if (!check) {
      //TODO: Error expression (e0) is not a subclass of type (@Type) called.
	  cout<<"Error expression (e0) is not a subclass of type (@Type) called."<<endl;
   }
   for(int i = actual->first(); actual->more(i); i = actual->next(i)) {
        expression = actual->nth(i);
        args[i+1] = expression->type_check(pEnv);
   }
   signature = pEnv->return_signature( get_index(type_name->get_string()),
                                       get_index(name->get_string()) );
   for (int i = 1; i<20; i++) {
       check = subclass(args[i], signature[i], pEnv);
	   if (!check) {
	       //TODO: Error arg[i] does not conform to signature[i]
		   cout<<"Error arg[i] does not conform to signature[i]"<<endl;
	   }
   }
   if ( signature[0] == get_index("SELF_TYPE") ) {
      Tn_p1 = T0;
   } else {
      Tn_p1 = signature[0];
   }
   set_type(idtable.lookup(Tn_p1));
   return Tn_p1;
}

int cond_class::type_check(Environment pEnv)
{
   int boolean = pred->type_check(pEnv);
   if (boolean != get_index("Bool")) {
      //TODO: Error if statement predicate is not of type bool.
	  cout<<"Error if statement predicate is not of type bool."<<endl;
   }
   int T2 = then_exp->type_check(pEnv);
   int T3 = else_exp->type_check(pEnv);
   int lub_result = lub(T2, T3, pEnv);
   set_type(idtable.lookup(lub_result));
   return lub_result;
}

int block_class::type_check(Environment pEnv)
{
   Expression pExpr;
   int Tn;
   for(int i = body->first(); body->more(i); i = body->next(i)) {
       pExpr = body->nth(i);
       Tn = pExpr->type_check(pEnv);
   }
   set_type(idtable.lookup(Tn));
   return Tn;
}

int let_class::type_check(Environment pEnv)
{
   int T0 = get_index( (type_decl->get_string()) );
   int T0prime;
   if (T0 == get_index("SELF_TYPE")) {
      T0prime = get_index("SELF_TYPE");
   } else {
      T0prime = T0;
   }
   int T1 = init->type_check(pEnv); 
   //this coudl return no_expr. Therefore this is the [let-no-init] branch
   if (T1 == get_index("_no_type"))  {
      pEnv->enter_object_scope();
      int* pType = new int(T0prime);
      pEnv->add_object( get_index(identifier->get_string()), pType );
	  T1 = body->type_check(pEnv);
	  pEnv->exit_object_scope();
	  
	  set_type(idtable.lookup(T1));
	  return T1;
   } else {
      bool check = subclass(T1, T0prime, pEnv);
	  if (!check) {
	     //TODO: Error let expression's initializer isn't a subclass of the declared type
		 //assign Object?! do we need to recover??
		 cout<<"Error let expression's initializer isn't a subclass of the declared type"<<endl;
	  }
      pEnv->enter_object_scope();
      int* pType = new int(T0prime);
      pEnv->add_object( get_index(identifier->get_string()), pType );
      int T2 = body->type_check(pEnv);
      pEnv->exit_object_scope();
	  
      set_type(idtable.lookup(T2));
      return T2;	  
   }
}

int typcase_class::type_check(Environment pEnv)
{
   Case pCase;
   int Tn, Tn_accumulate;
   int T0 = expr->type_check(pEnv);
   
   for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
       pCase = cases->nth(i);
       Tn = pCase->type_check(pEnv);
	   if (i == cases->first()) {
	      Tn_accumulate = Tn;
	   }
	   Tn_accumulate = lub(Tn,Tn_accumulate,pEnv);
   }
   set_type(idtable.lookup(Tn_accumulate));
   return Tn_accumulate;
}

int branch_class::type_check(Environment pEnv)
{
   int Tn;
   
   pEnv->enter_object_scope();
   int* pType = new int( get_index(type_decl->get_string()) );
   pEnv->add_object( get_index(name->get_string()), pType );	   
   Tn = expr->type_check(pEnv);
   pEnv->exit_object_scope();

   return Tn;   
}

int loop_class::type_check(Environment pEnv)
{
   int T_pred = pred->type_check(pEnv);
   if ( T_pred != get_index("Bool") ) {
      //TODO: Error look predicate is not a boolean
	  cout<<"Error look predicate is not a boolean"<<endl;
   }
   int T2 = body->type_check(pEnv);
   set_type(idtable.lookup_string("Object"));
   return get_index("Object");
}

int isvoid_class::type_check(Environment pEnv)
{
   int T1 = e1->type_check(pEnv);
   set_type(idtable.lookup_string("Bool"));
   return get_index("Bool");
}

//TODO: Note the comp_class always returns Bool (we know this, so it only reports errors).
int comp_class::type_check(Environment pEnv)
{
   int T = e1->type_check(pEnv);
   if ( T != get_index("Bool") ) {
      //TODO: Error NOT expression not used on a BOOL
	  cout<<"Error NOT expression not used on a BOOL"<<endl;
   }
   set_type(idtable.lookup_string("Bool"));
   return get_index("Bool");
}

int lt_class::type_check(Environment pEnv)
{
   int T1 = e1->type_check(pEnv);
   if ( T1 != get_index("Int") ) {
      //TODO: Error left side of [less than] operator is not Type Int.
	  cout<<"Error left side of [less than] operator is not Type Int."<<endl;
   }
   int T2 = e2->type_check(pEnv);
   if ( T2 != get_index("Int") ) {
      //TODO: Error right side of [less than] operator is not Type Int.
	  cout<<"Error right side of [less than] operator is not Type Int."<<endl;
   }
   set_type(idtable.lookup_string("Bool"));
   return get_index("Bool");
}

int leq_class::type_check(Environment pEnv)
{
   int T1 = e1->type_check(pEnv);
   if ( T1 != get_index("Int") ) {
      //TODO: Error left side of [less than equal to] operator is not Type Int.
	  cout<<"Error left side of [less than equal to] operator is not Type Int."<<endl;
   }
   int T2 = e2->type_check(pEnv);
   if ( T2 != get_index("Int") ) {
      //TODO: Error right side of [less than equal to] operator is not Type Int.
	  cout<<"Error right side of [less than equal to] operator is not Type Int."<<endl;
   }
   set_type(idtable.lookup_string("Bool"));
   return get_index("Bool");   
}

int neg_class::type_check(Environment pEnv)
{
   int T1 = e1->type_check(pEnv);
   if ( T1 != get_index("Int") ) {
      //TODO: Error [not] operator is not Type Int.
	  cout<<"Error [not] operator is not Type Int."<<endl;
   }
   set_type(idtable.lookup_string("Int"));
   return get_index("Int"); 
}

int plus_class::type_check(Environment pEnv)
{
   int T1 = e1->type_check(pEnv);
   if ( T1 != get_index("Int") ) {
      //TODO: Error left side of [plus] operator is not Type Int.
	  cout<<"Error left side of [plus] operator is not Type Int."<<endl;
   }
   int T2 = e2->type_check(pEnv);
   if ( T2 != get_index("Int") ) {
      //TODO: Error right side of [plus] operator is not Type Int.
	  cout<<"Error right side of [plus] operator is not Type Int."<<endl;
   }
   set_type(idtable.lookup_string("Int"));
   return get_index("Int");    
}

int sub_class::type_check(Environment pEnv)
{
   int T1 = e1->type_check(pEnv);
   if ( T1 != get_index("Int") ) {
      //TODO: Error left side of [minus] operator is not Type Int.
	  cout<<"Error left side of [minus] operator is not Type Int."<<endl;
   }
   int T2 = e2->type_check(pEnv);
   if ( T2 != get_index("Int") ) {
      //TODO: Error right side of [minus] operator is not Type Int.
	  cout<<"Error right side of [minus] operator is not Type Int."<<endl;
   }
   set_type(idtable.lookup_string("Int"));
   return get_index("Int");  
}

int mul_class::type_check(Environment pEnv)
{
   int T1 = e1->type_check(pEnv);
   if ( T1 != get_index("Int") ) {
      //TODO: Error left side of [multiply] operator is not Type Int.
	  cout<<"Error left side of [multiply] operator is not Type Int."<<endl;
   }
   int T2 = e2->type_check(pEnv);
   if ( T2 != get_index("Int") ) {
      //TODO: Error right side of [multiply] operator is not Type Int.
	  cout<<"Error right side of [multiply] operator is not Type Int."<<endl;
   }
   set_type(idtable.lookup_string("Int"));
   return get_index("Int");
}

int divide_class::type_check(Environment pEnv)
{
   int T1 = e1->type_check(pEnv);
   if ( T1 != get_index("Int") ) {
      //TODO: Error left side of [divide] operator is not Type Int.
	  cout<<"Error left side of [divide] operator is not Type Int."<<endl;
   }
   int T2 = e2->type_check(pEnv);
   if ( T2 != get_index("Int") ) {
      //TODO: Error right side of [divide] operator is not Type Int.
	  cout<<"Error right side of [divide] operator is not Type Int."<<endl;
   }
   set_type(idtable.lookup_string("Int"));
   return get_index("Int");
}

int eq_class::type_check(Environment pEnv)
{
   int T1 = e1->type_check(pEnv);
   int T2 = e2->type_check(pEnv);
   
   if ( T1 == get_index("String") || T2 == get_index("String") ) {
      if (T1 != get_index("String") || T2 != get_index("String")) {
	     //TODO: Error when using [compare] both variables must be String.
		 cout<<"Error when using [compare] both variables must be String."<<endl;
	  }
   }
   if ( T1 == get_index("Int") || T2 == get_index("Int") ) {
      if (T1 != get_index("Int") || T2 != get_index("Int")) {
         //TODO: Error when using [compare] both variables must be Int.
		 cout<<"Error when using [compare] both variables must be Int."<<endl;
	  }
   }
   if ( T1 == get_index("Bool") || T2 == get_index("Bool") ) {
      if (T1 != get_index("Bool") || T2 != get_index("Bool")) {
         //TODO: Error when using [compare] both variables must be Bool.
		 cout<<"Error when using [compare] both variables must be Bool."<<endl;
	  }
   }
   set_type(idtable.lookup_string("Bool"));
   return get_index("Bool");
}

int object_class::type_check(Environment pEnv)
{
   int T = pEnv->lookup_object( get_index(name->get_string()) );
   set_type(idtable.lookup(T));
   return T;   
}

int no_expr_class::type_check(Environment pEnv)
{
   return (-1);
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

    /* some semantic analysis code may go here */

/////////// An idtable Look up example /////////////////////////////////
//    for (int i = idtable.first(); idtable.more(i); i = idtable.next(i)) {
//       //idtable.
//       myfile << (idtable.lookup(i))->get_string() << endl;
//    }
//
//
//  for (int i = stringtable.first(); stringtable.more(i); i = stringtable.next(i)) {
//     myfile << (stringtable.lookup(i))->get_string() << endl;
//  }
//
//    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
//        Class_ classObj = classes->nth(i);
//        classObj->dump_test(myfile);
//    }
/////////////////////////////////////////////////////////////////////////
   std::map<int, Graph_Node> pInhTree = classtable->get_inh_tree();
   Environment pEnv = new Environment_class(pInhTree);
   pEnv->set_method_mode();
   construct_method_environment(myfile, pEnv);
   pEnv->set_normal_mode();
   pEnv->print_by_class(myfile);

   Class_ classObj;
   for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
       classObj = classes->nth(i);
       pEnv->set_current_class( get_index(classObj->get_name()) );
if (i==0){
       pEnv->set_object_mode();
       construct_object_environment(myfile, pEnv);
       pEnv->set_normal_mode();
classObj->type_check(pEnv);
}
   }   



//int res1 = lub(get_index("IO"), 
//               get_index("D"),
//               pEnv);
//cout << "lub(IO, D, pEnv) = " << res1 << endl;

for (int i = idtable.first(); idtable.more(i); i=idtable.next(i) ) {
   cout << "#: " << i << " Entry: " << idtable.lookup(i)->get_string() << endl;
}

    myfile.close();
cout << "FINISH" <<endl;
    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


