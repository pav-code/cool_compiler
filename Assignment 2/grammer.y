    /* Complete the nonterminal list below, giving a type for the semantic
    value of each non terminal. (See section 3.6 in the bison 
    documentation for details). */
    
    /* Declare types for the grammar's non-terminals. */
    %type <program> program
    %type <classes> class_list
    %type <class_> class
    %type <features> feature_list
    %type <feature> feature
    %type <formals> formal_list
    %type <formal> formal
    %type <expression> expression
    %type <expressions> expression_list
    /* Precedence declarations go here. */    
    
    %%
    /* 
    Save the root of the abstract syntax tree in a global variable.
    */
    program	: class_list	{ @$ = @1; ast_root = program($1); }
    ;
    
    class_list
    : class			/* single class */
    { $$ = single_Classes($1); parse_results = $$; }
    | class_list class	       /* several classes */
    { $$ = append_Classes($1,single_Classes($2)); parse_results = $$; }
    ;
    
    /* If no parent is specified, the class inherits from the Object class. */
    class	: CLASS TYPEID '{' feature_list '}' ';'
    { $$ = class_($2,idtable.add_string("Object"),$4,
    stringtable.add_string(curr_filename)); }
    | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'
    { $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
    ;
    
    /* Feature list may be empty, but no empty features in list. */
    feature_list:		/* empty */
    { $$ = nil_Features(); }
    | feature                   /* single feature */
    { $$ = single_Feature($1); parse_results = $$; }
    | feature_list
    { $$ = append_Features($1, single_Features($1)); parse_results = $$; }
//correct later


    feature       : OBJECTID '(' formal_list ')' ':' TYPEID '{' expression '}' ';'
    { $$ = method($1, $3, $6, $8); }
    | OBJECTID ':' TYPEID ';'
    { $$ = attr($1, $3); }
    | OBJECTID ':' TYPEID ASSIGN expression ';'
    { $$ = attr($1, $3, $5); }

    formal_list
    : formal             /* single formal */
    { $$ = single_Formals($1); parse_results = $$; }
    | formal_list
    { $$ = append_Formals($1, single_Formals($1)); parse_results = $$; }

    class_list
    : class			/* single class */
    { $$ = single_Classes($1); parse_results = $$; }
    | class_list class	/* several classes */
    { $$ = append_Classes($1,single_Classes($2)); parse_results = $$; }
    ;

    formal        : OBJECTID ':' TYPEID
    { $$ = formal($1, $3); }
    | OBJECTID ':' TYPEID ','
    { $$ = formal($1, $3); }

    expression_list:         /* empty */
    { $$ = nil_Expressions(); } 
    | expression
    { $$ = single_Expressions($1); parse_results = $$; }
    | expression_list expression
    { $$ = append_Expressions($1, single_Expressions($2)); parse_results = $$; }
    ;

    expression      :
    { $$ = nil_Expression(); }



    /* end of grammar */
    %%