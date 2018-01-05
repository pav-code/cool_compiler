#ifndef CUSTOM_HEADER_H_
#define CUSTOM_HEADER_H_

#include <cstring>
#include <vector>
#include <map>
#include "symtab.h"
#include "list.h"


#define MAX_NUMBER_ARGS 20


class Graph_Node {
private:
  Graph_Node* nodeParent;
  int iName;
  int iParent;
	
public:
  Graph_Node(int Name, int Parent, Graph_Node* nodeParent=NULL): iName(Name), iParent(Parent) {}
  Graph_Node* Get_Parent();
  Graph_Node* Copy();
  void Set_Parent(Graph_Node* );
  int get_name_index();
  int get_parent_index();
};

typedef class FormalContainer *pFormal;

class FormalContainer 
{
public:
   int formals [MAX_NUMBER_ARGS]; 
   int index;
   FormalContainer() { 
      index = 1;
      for (int i = 0; i<MAX_NUMBER_ARGS; i++) {
          formals[i] = -1;
      }
   }  
};


typedef class Environment_class *Environment;

class Environment_class {
private:
  SymbolTable<int, SymbolTable<int, FormalContainer > >*methodEnv;
  SymbolTable<int, FormalContainer>*innerMethodMap;
  pFormal formalList;
  std::vector<int> branch_check;
  bool branch_error;

  SymbolTable<int, int>*objectEnv;

  int iCurrentClass;  

  int fOperationalEnvironment;
  char* Error;
  bool fError;
  
public:
  std::map<int, Graph_Node> pInhTree;

  Environment_class(std::map<int, Graph_Node> InhTree) {
    methodEnv = new SymbolTable<int ,SymbolTable<int, FormalContainer > >();
    innerMethodMap = new SymbolTable<int, FormalContainer>();
    methodEnv->enterscope();
    innerMethodMap->enterscope();
    formalList = new FormalContainer;

    objectEnv = new SymbolTable<int, int>();
    objectEnv->enterscope();

    pInhTree = InhTree;

    fError = false;
    branch_error = false;
  }
  int  add_object(int name, int* type);
  int  lookup_object(int name);
  void enter_object_scope();
  void exit_object_scope();
  void clear_object_map();

  void dump_objects();
  void add_formal(char* type);
  void add_method_return(char* type);
  int  add_method(char* name);
  void print_by_class(ostream& myfile);
  void add_class_methods(char* className);
  void clear_inner_map();
  void clear_formals();
  int* return_signature(int Class, int Method);
  bool m_main_check();
  void m_overload_check(int C, int* f);
  int  m_declare_check(int* c_C, int* m_M);
  bool b_check();
  void b_clear();
  void b_add(int );

  int  iget_current_class() { return  iCurrentClass; }
  void iset_current_class(int setClass) { iCurrentClass=setClass; }
  int  get_current_class() { return iCurrentClass; }
  void set_current_class(int setClass) { iCurrentClass=setClass; }
  
  int get_mode() { return fOperationalEnvironment; }
  bool is_method_mode() { return( (fOperationalEnvironment==1) ? true : false); }
  bool is_object_mode() { return( (fOperationalEnvironment==2) ? true : false); }
  bool is_normal_mode() { return( (fOperationalEnvironment==0) ? true : false); }
  void set_method_mode() { fOperationalEnvironment = 1; }
  void set_object_mode() { fOperationalEnvironment = 2; }
  void set_normal_mode() { fOperationalEnvironment = 0; }

};

#endif