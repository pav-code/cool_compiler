#include <stdlib.h>
#include <stdio.h>
#include <symtab.h>

int main (int argc, char *argv[]) {
  SymbolTable <char*, char> *map = new SymbolTable<char*, char> ();
  map->enterscope();
  
  char* names [4];// = {"Bob","Jim","Kat","Luke"};
  names[0] = "Bob";
  names[0] = "Ryan";
  names[1] = "Omar";
  names[2] = "Hoota";
  names[3] = "Gill";
  char** cool = names;
  cool[1] = "Mom";

  cout << "Try the ending name" << names[sizeof(names)/sizeof(char*)-1] << endl;
  cout << names[1] << endl;
  cout << names[2] << endl;
  cout << names[3] << endl;

  SymbolTable<char* ,SymbolTable<char*, char* [MAX_NUMBER_ARGS]> > *methodMap  =  \
      new SymbolTable<char* ,SymbolTable<char*, char* [MAX_NUMBER_ARGS]> >;

  SymbolTable<char*, char* MAX_NUMBER_ARGS]>*mapTest = new SymbolTable<char*, char* [MAX_NUMBER_ARGS]>();


char * ok2 [20];
//char * pData [3];
char** pData;
ok2[0] = "friend 0544455";
ok2[1] = "friend 1";
ok2[2] = "friend 2454";
ok2[3] = 0;
cout << ((ok2[4] == NULL) ? "Yes\n" : "No\n");

char* Fred = "Fred";
SymbolTable<char*, char*[20]>*mapTest = new SymbolTable<char*, char*[20]>();

mapTest->enterscope();
mapTest->addid(Fred, &ok2);
pData = *mapTest->lookup(Fred);
if (pData == NULL)
   cout << "NULL " <<  endl;
else
   cout << "NOT NULL! The Data is: " << pData[0] << endl;

SymbolTable<char*, char*[20]>*pFunc;

methodMap->enterscope();
methodMap->addid(Fred, mapTest);
pFunc = methodMap->lookup(Fred);
if (pFunc == NULL)
   cout << "NULL " <<  endl;
else {
   cout << "NOT NULL! The Data is: Class " << Fred;
   pData = *pFunc->lookup(Fred);
   cout << " Method Name " << Fred << " Arguments " << pData[0] << endl;
   
}

  return 0;
}