/*
 *  The scanner definition for COOL.
 * FIX: Length of string errors, proper implementation required
 * Weird Behaviour Requirements, and confusing implementations:
 * COMMENT_Type1: a comment of the type "--" until the end of the line.
 *  Starts on: "--" (LN:), then its exclusive state comes on. Turns off on: two 
 *  cases 1) [\n] newline (LN:) 2) <<EOF>> end-of-file (LN:)
 * COMMENT_Type2: a comment of the type "(* ... *)". This type can be nested!
 *  Starts on: "(*" with an increment of a counter: nestedCommentCounter, which
 *  is used to keep track of how many "nestings" there are (LN:).
 *  Accepts input: . anything but new line(LN:). "\n" a non escaped newline, increment
 *  the line counter (curr_lineno) (LN:). 
 *  Turns off on: 1) <<EOF>> end-of-line, returns ERROR (LN:). 2) "*"/")" star followed
 *  by a right bracket (LN:), first we must check if there are any nested comments. In either 
 *  case we must skip the next match (close bracket). In the nested case the nested counter
 *  is decremented. There are two kinds of skips: SKIP and SKIP2, the reason for this is
 *  SKIP2 goes into the COMMENT_Type2 state (since there was a nested comment) (LN:).
 *  
 */ 

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>
#include <string>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char string_buf_ptr[MAX_STR_CONST+100];
char buffer[10];

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

int nestedCommentCounter = 0;
int strSize = 0;
bool bError = false;

/*
 *  Add Your own definitions here
 *  COMMENT_Type1 - the line comment starting with "--"
 *  COMMENT_Type2 - the full comment enclosed with "*...*"
 */
%}
int curr_lineno = 0;
DARROW          =>
%x COMMENT_Type1
%x COMMENT_Type2
%x STRING
%x SKIP
%x SKIP2

a [aA]
b [bB]
c [cC]
d [dD]
e [eE]
f [fF]
g [gG]
h [hH]
i [iI]
j [jJ]
k [kK]
l [lL]
m [mM]
n [nN]
o [oO]
p [pP]
q [qQ]
r [rR]
s [sS]
t [tT]
u [uU]
v [vV]
w [wW]
x [xX]
y [yY]
z [zZ]

/*
 * Define names for regular expressions here.
 * 
 */

%%
"\n"    { curr_lineno++; }
"--"    { BEGIN(COMMENT_Type1); }
"(*"    { BEGIN(COMMENT_Type2); }
\"      { BEGIN(STRING); }
"*)"    { cool_yylval.error_msg = "Unmatched *)"; return ERROR; } 

<COMMENT_Type2>"(*" { nestedCommentCounter++; }

{c}{l}{a}{s}{s} { return CLASS; }

{i}{f} { return IF; }
{t}{h}{e}{n} { return THEN; }
{e}{l}{s}{e} { return ELSE; }
{f}{i} { return FI; }

{w}{h}{i}{l}{e} { return WHILE; }
{l}{o}{o}{p} { return LOOP; }
{p}{o}{o}{l} { return POOL; }

{l}{e}{t} { return LET; }
{i}{n}    { return IN; }
{i}{n}{h}{e}{r}{i}{t}{s} { return INHERITS; }
{n}{e}{w} { return NEW; }

{c}{a}{s}{e} { return CASE; }
{e}{s}{a}{c} { return ESAC; }

{o}{f} { return OF; }
{n}{o}{t} { return NOT; }
{i}{s}{v}{o}{i}{d} { return ISVOID; }

t{r}{u}{e}     { cool_yylval.boolean = 1; return BOOL_CONST; }
f{a}{l}{s}{e}  { cool_yylval.boolean = 0; return BOOL_CONST; }

[0-9]+    {
    // INTEGERS
    cool_yylval.symbol = inttable.add_string(yytext);
    return INT_CONST;
}
[A-Z][A-Za-z0-9_]*    {
    // TYPE IDENTIFIERS
    cool_yylval.symbol = idtable.add_string(yytext);
    return TYPEID;
}
[a-z][A-Za-z0-9_]*    {
    // OBJECT IDENTIFIERS
    cool_yylval.symbol = idtable.add_string(yytext);
    return OBJECTID;
}

"."    return '.';
"@"    return '@';
"~"    return '~';
":"    return ':';
";"    return ';';
"*"    return '*';
"/"    return '/';
"+"    return '+';
"-"    return '-';
"<"    return '<';
"<="   return LE;
"=>"   return DARROW;
"="    return '=';
"("    return '(';
")"    return ')';
"{"    return '{';
"}"    return '}';
"<-"   return ASSIGN;
","    return ',';
[ \t\v\r\f]+     { ; }

<COMMENT_Type1>[\n]      { BEGIN(INITIAL); curr_lineno++; }
<COMMENT_Type1><<EOF>>   { BEGIN(INITIAL); }

<COMMENT_Type1>.         { ; }

<COMMENT_Type2><<EOF>>   { cool_yylval.error_msg = "EOF in comment"; 
    BEGIN(INITIAL); 
    return ERROR;  
}
<COMMENT_Type2>\\[^\n]   { ; }
<COMMENT_Type2>"\n"      { curr_lineno++; }
<COMMENT_Type2>"*"/")"   { 
    if (nestedCommentCounter == 0)
        BEGIN(SKIP);
    else
    {
        BEGIN(SKIP2);
        nestedCommentCounter--;
    }
} 
<COMMENT_Type2>.         { ; }

<STRING>\\ [\0]         { 
    cool_yylval.error_msg = "String contains escaped null character.";
    bError = true;
    return ERROR;
}
<STRING>[\0]           {
    cool_yylval.error_msg = "String contains null character.";
    bError = true;
    return ERROR;
}
<STRING><<EOF>>      { 
    cool_yylval.error_msg = "EOF in string constant."; 
    BEGIN(INITIAL); 
    return ERROR; 
}

<STRING>[^\n\"]      { strcat (string_buf_ptr,yytext); strSize++;}
<STRING>\\\n         { strcat (string_buf_ptr,"\n"); curr_lineno++; strSize++;}
<STRING>\\\"         { strcat(string_buf_ptr,"\""); strSize++;}
<STRING>\\[^btnf]    { strncpy(buffer,yytext+1,1); strcat(string_buf_ptr,buffer); strSize++; }
<STRING>\\[n]        { strcat(string_buf_ptr,"\n"); strSize++; }
<STRING>\\[b]        { strcat(string_buf_ptr,"\b"); strSize++; }
<STRING>\\[t]        { strcat(string_buf_ptr,"\t"); strSize++; }
<STRING>\\[f]        { strcat(string_buf_ptr,"\f"); strSize++; }

<STRING>\n           {
    if (!bError)
    { 
        cool_yylval.error_msg = "Unterminated string constant"; 
        curr_lineno++;
        strcpy(string_buf_ptr, ""); 
        BEGIN(INITIAL); 
        return ERROR;
    }
    else
    {
        curr_lineno++;
        strcpy(string_buf_ptr, "");
        BEGIN(INITIAL);
     }
}
<STRING>\"           {
    if ((strSize >= 1025) && (!bError))
    {
        cool_yylval.error_msg = "String constant too long";
        strcpy (string_buf_ptr, "");
        strSize = 0;
        BEGIN(INITIAL);
        return ERROR;    
    }
    else if (!bError)
    {     
        cool_yylval.symbol = stringtable.add_string(string_buf_ptr);
        strcpy (string_buf_ptr, "");
        strSize = 0;
        BEGIN(INITIAL);
        return STR_CONST;
    }
    else if (bError)
    {
        strcpy (string_buf_ptr, "");
        strSize = 0;        
        BEGIN(INITIAL);
        bError = false;
    }
}

<SKIP>")"            { BEGIN(INITIAL); }
<SKIP2>")"           { BEGIN(COMMENT_Type2);}

.                    { cool_yylval.error_msg = yytext; return ERROR; }

 /*
  *  Nested comments
  *  Lexical Units of COOL: integers, type indentifiers, object identifiers
  *  special notation, strings, keywords, and white spaces
  */


 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

%%
