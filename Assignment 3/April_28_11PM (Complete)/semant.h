#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>
#include <map>
#include "custom-header.h"  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0


// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

// Comparason class for our char*s (keys) in Map
struct char_cmp { 
    bool operator () (const char *a,const char *b) const 
    {
        return strcmp(a,b)<0;
    } 
};



#endif

