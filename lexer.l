%{
#include <cstdio>

#include "tree.h"

using namespace bc;
#include "parser.h"

#define YY_NO_UNPUT 1

extern int lineno;
void yyerror(const char *str, ...);
%}

%option nounput

%%

class				{ return CLASS; }
int				{ return INT; }
float				{ return FLOAT; }
void				{ return VOID; }
print				{ return PRINT; }
return				{ return RETURN; }
if				{ return IF; }
else				{ return ELSE; }
while				{ return WHILE; }
for				{ return FOR; }
yield				{ return YIELD; }
spawn				{ return SPAWN; }
[a-zA-Z_][a-zA-Z0-9_]*		{
					yylval.symbol_val = new Symbol(yytext);
					return IDENTIFIER;
				}
[0-9]+				{
					yylval.int_val = atoi(yytext);
					return INT_LITERAL;
				}
[0-9]*"."?[0-9]*([eE][+-]?[0-9]+)? {
					yylval.float_val = atof(yytext); 
					return FLOAT_LITERAL;
				}
"("|")"|"{"|"}"|";"|","|"+"|"-"|"*"|"/"|"="|"<"|">" { return *yytext; }
"+="				{ return ADDASSIGN; }
"-="				{ return SUBASSIGN; }
"*="				{ return MULASSIGN; }
"/="				{ return DIVASSIGN; }
"=="				{ return EQ; }
"!="				{ return NE; }
"<="				{ return LE; }
">="				{ return GE; }
"++"				{ return PLUSPLUS; }
"--"				{ return MINUSMINUS; }
"&&"				{ return AND; }
"||"				{ return OR; }
[ \t]+				{ /* eat up whitespace */ }
"//".*				{ ++lineno; }
\n				{ ++lineno; }
.				{
					yyerror("invalid character `%c'", *yytext);
				}

%%

int
yywrap()
{
	return 1;
}
