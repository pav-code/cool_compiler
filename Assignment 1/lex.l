%{
/*	
*	Lexical analyzer
*/

#include	<string.h>
#include	<stdlib.h>	
#include	"coolc.tab.h"
#include	"symbol.h"

extern int	lineno;		

void lex_init()
{
}

#define	YY_USER_INIT	lex_init();
%}

uppercase		[A-Z]
lowercase		[a-z]
digit			[0-9]
alphanum		[A-Za-z0-9]
alphanumu		[A-Za-z0-9_]
alphastr		[ A-Za-z0-9_\-*/.:()]

%%
[ \t]			break;		/* ignore white space */
"\n" 			++lineno; 


class			{ return CLASS; }
is			{ return IS; }

if			{ return IF; }
then			{ return THEN; }
else			{ return ELSE; }
fi			{ return FI; }

while			{ return WHILE; }
loop			{ return LOOP; }
pool			{ return POOL; }

begin			{ return BBEGIN; }
end			{ return END; }

true			{ return TRUE; }
false			{ return FALSE; }
not			{ return NOT; }

print			{ return PRINT; }

{uppercase}{alphanumu}*	{	/* Class name */
				yylval.cValue=strdup(yytext);
				return TYPEID;
			}

{lowercase}{alphanumu}*	{	/* Identifier */
				yylval.cValue=strdup(yytext);
				return ID;
			}

{digit}+		{	/* Some number */
                                yylval.iValue=atoi(yytext);
				return NUMBER;
			}

\"{alphastr}*\"		{	/* String enclosed in quotes */
				yylval.cValue=strdup(yytext);
				return STRING;
			}

"("			return LPAREN;
")"			return RPAREN;
"{"			return LBRACE;
"}"			return RBRACE;
"="			return EQUAL;
","			return COLON;
";"			return SEMICOLON;
":"			return DOTS;
"."			return DOT;
"+"			return PLUS;
"-"			return MINUS;
"<-"			return ASSIGN;

.			{
				fprintf(stderr,	"Illegal character \'%s\' on line #%d\n",yytext,lineno);
				exit(1);
			}

%%

int yywrap()
{ return 1; }

