%{
#include <cstdio>
#include <cstring>
#include <cerrno>

extern "C" {
#include "panic.h"
}
#include "tree.h"
#include "icode.h"

using namespace bc;

extern int yylex();
extern void yyerror(const char *str, ...);

static Program *program;
%}

%union {
	Symbol *symbol_val;
	ClassList *class_list_val;
	Class *class_val;
	MemberList *member_list_val;
	Member *member_val;
	Formal *formal_val;
	FormalList *formal_list_val;
	Stmt *stmt_val;
	StmtList *stmt_list_val;
	Expr *expr_val;
	ExprList *expr_list_val;
	DeclStmt::SymInit *sym_init_val;
	DeclStmt::SymInitList *sym_init_list_val;
	Type type_val;
	int int_val;
	float float_val;
};

%token <symbol_val> IDENTIFIER
%token <int_val> INT_LITERAL
%token <float_val> FLOAT_LITERAL
%token CLASS
%token PRINT RETURN IF ELSE WHILE FOR YIELD SPAWN
%token INT FLOAT VOID

%nonassoc IF
%nonassoc ELSE

%type <class_list_val> class_list
%type <class_val> class
%type <member_list_val> member_list opt_member_list
%type <member_val> member field method
%type <formal_list_val> formal_list opt_formal_list
%type <formal_val> formal
%type <stmt_val> stmt expr_stmt block_stmt return_stmt print_stmt decl_stmt if_stmt simple_if_stmt while_stmt for_stmt yield_stmt spawn_stmt
%type <stmt_list_val> opt_stmt_list stmt_list
%type <type_val> type
%type <expr_val> expr opt_expr unary_expr sym_ref primary fun_call opt_init
%type <expr_list_val> opt_expr_list expr_list
%type <sym_init_val> sym_init
%type <sym_init_list_val> sym_init_list

%right '=' ADDASSIGN SUBASSIGN MULASSIGN DIVASSIGN
%left OR
%left AND
%left EQ NE
%left '<' '>' LT GT LE GE
%left '-' '+'
%left '*' '/'
%left NEG
%right PLUSPLUS MINUSMINUS
%right UNARY

%%

program
: class_list
	{
		program = new Program($1);
	}
;

class_list
: class
	{
		$$ = new ClassList;
		$$->push_back($1);
	}
| class_list class
	{
		$$->push_back($2);
		$$ = $1;
	}
;

class
: CLASS IDENTIFIER '{' opt_member_list '}'
	{
		$$ = new Class($2, $4);
	}
;

opt_member_list
: /* NOTHING */
	{
		$$ = new MemberList;
	}
| member_list
;

member_list
: member
	{
		$$ = new MemberList;
		$$->push_back($1);
	}
| member_list member
	{
		$1->push_back($2);
		$$ = $1;
	}
;

member
: field
| method
;

field
: type IDENTIFIER ';'
	{
		$$ = new Field($1, $2);
	}
;

method
: type IDENTIFIER '(' opt_formal_list ')' block_stmt
	{
		$$ = new Method($1, $2, $4, dynamic_cast<BlockStmt *>($6));
	}
;

opt_formal_list
: /* NOTHING */
	{
		$$ = new FormalList;
	}
| formal_list
;

formal_list
: formal
	{
		$$ = new FormalList;
		$$->push_back($1);
	}
| formal_list ',' formal
	{
		$1->push_back($3);
		$$ = $1;
	}
;

formal
: type IDENTIFIER
	{
		$$ = new Formal($1, $2);
	}
;

type
: INT
	{
		$$ = T_INTEGER;
	}
| VOID
	{
		$$ = T_VOID;
	}
| FLOAT
	{
		$$ = T_FLOAT;
	}
;

stmt
: block_stmt
| expr_stmt
| return_stmt
| print_stmt
| decl_stmt
| if_stmt
| while_stmt
| for_stmt
| yield_stmt
| spawn_stmt
;

block_stmt
: '{' opt_stmt_list '}'
	{
		$$ = new BlockStmt($2);
	}
;

opt_stmt_list
: /* NOTHING */
	{
		$$ = new StmtList;
	}
| stmt_list
;

stmt_list
: stmt
	{
		$$ = new StmtList;
		$$->push_back($1);
	}
| stmt_list stmt
	{
		$$->push_back($2);
		$$ = $1;
	}
;

expr_stmt
: expr ';'
	{
		$$ = new ExprStmt($1);
	}
;

return_stmt
: RETURN opt_expr ';'
	{
		$$ = new ReturnStmt($2);
	}
;

print_stmt
: PRINT expr ';'
	{
		$$ = new PrintStmt($2);
	}
;

decl_stmt
: type sym_init_list ';'
	{
		$$ = new DeclStmt($1, $2);
	}
;

sym_init_list
: sym_init
	{
		$$ = new DeclStmt::SymInitList;
		$$->push_back($1);
	}
| sym_init_list ',' sym_init
	{
		$1->push_back($3);
		$$ = $1;
	}
;

sym_init
: IDENTIFIER opt_init
	{
		$$ = new DeclStmt::SymInit($1, $2);
	}
;

opt_init
: /* NOTHING */
	{
		$$ = 0;
	}
| '=' expr
	{
		$$ = $2;
	}
;

opt_expr
: /* NOTHING */ 
	{
		$$ = 0;
	}
| expr
;

if_stmt
: simple_if_stmt ELSE stmt
	{
		$$ = $1;
		dynamic_cast<IfStmt *>($1)->set_else_stmt($3);
	}
| simple_if_stmt %prec IF
;

simple_if_stmt
: IF '(' expr ')' stmt
	{
		$$ = new IfStmt($3, $5);
	}
;

while_stmt
: WHILE '(' expr ')' stmt
	{
		$$ = new WhileStmt($3, $5);
	}
;

for_stmt
: FOR '(' expr ';' expr ';' expr ')' stmt
	{
		$$ = new ForStmt($3, $5, $7, $9);
	}
;

yield_stmt
: YIELD ';'
	{
		$$ = new YieldStmt;
	}
;

spawn_stmt
: SPAWN IDENTIFIER '(' opt_expr_list ')' ';'
	{
		$$ = new SpawnStmt($2, $4);
	}
;

expr
: fun_call
| unary_expr
| expr '+' expr
	{
		$$ = new BinaryOp(BT_ADD, $1, $3);
	}
| expr '-' expr
	{
		$$ = new BinaryOp(BT_SUB, $1, $3);
	}
| expr '*' expr
	{
		$$ = new BinaryOp(BT_MUL, $1, $3);
	}
| expr '/' expr
	{
		$$ = new BinaryOp(BT_DIV, $1, $3);
	}
| expr '=' expr
	{
		$$ = new BinaryOp(BT_ASSIGN, $1, $3);
	}
| expr ADDASSIGN expr
	{
		$$ = new BinaryOp(BT_ADD_ASSIGN, $1, $3);
	}
| expr SUBASSIGN expr
	{
		$$ = new BinaryOp(BT_SUB_ASSIGN, $1, $3);
	}
| expr MULASSIGN expr
	{
		$$ = new BinaryOp(BT_MUL_ASSIGN, $1, $3);
	}
| expr DIVASSIGN expr
	{
		$$ = new BinaryOp(BT_DIV_ASSIGN, $1, $3);
	}
| expr EQ expr
	{
		$$ = new BinaryOp(BT_EQ, $1, $3);
	}
| expr NE expr
	{
		$$ = new BinaryOp(BT_NE, $1, $3);
	}
| expr AND expr
	{
		$$ = new BinaryOp(BT_AND, $1, $3);
	}
| expr OR expr
	{
		$$ = new BinaryOp(BT_OR, $1, $3);
	}
| expr '<' expr
	{
		$$ = new BinaryOp(BT_LT, $1, $3);
	}
| expr '>' expr
	{
		$$ = new BinaryOp(BT_GT, $1, $3);
	}
| expr LE expr
	{
		$$ = new BinaryOp(BT_LE, $1, $3);
	}
| expr GE expr
	{
		$$ = new BinaryOp(BT_GE, $1, $3);
	}
| '-' expr %prec NEG
	{
		$$ = new UnaryOp(UT_NEG, $2);
	}
;

unary_expr
: primary
| PLUSPLUS sym_ref %prec UNARY
	{
		$$ = new BinaryOp(BT_ADD_ASSIGN, $2, new IntLiteral(1));
	}
| MINUSMINUS sym_ref %prec UNARY
	{
		$$ = new BinaryOp(BT_SUB_ASSIGN, $2, new IntLiteral(1));
	}
;

primary
: IDENTIFIER
	{
		$$ = new SymRef($1);
	}
| INT_LITERAL
	{
		$$ = new IntLiteral($1);
	}
| FLOAT_LITERAL
	{
		$$ = new FloatLiteral($1);
	}
| '(' expr ')'
	{
		$$ = $2;
	}
| sym_ref PLUSPLUS
	{
		$$ = new UnaryOp(UT_POST_INCR, $1);
	}
| sym_ref MINUSMINUS
	{
		$$ = new UnaryOp(UT_POST_DECR, $1);
	}
;

sym_ref
: IDENTIFIER
	{
		$$ = new SymRef($1);
	}
;

fun_call
: IDENTIFIER '(' opt_expr_list ')'
	{
		$$ = new FunCall($1, $3);
	}
;

opt_expr_list
: /* NOTHING */
	{
		$$ = new ExprList;
	}
| expr_list
;

expr_list
: expr
	{
		$$ = new ExprList;
		$$->push_back($1);
	}
| expr_list ',' expr
	{
		$1->push_back($3);
		$$ = $1;
	}
;

%%

int lineno;

void
yyerror(const char *str, ...)
{
	panic("error on line %d: %s", lineno, str);
	exit(1);
}

ProgramCode *
parse_script(const char *file_name)
{
	extern FILE *yyin;

	lineno = 1;

	if ((yyin = ::fopen(file_name, "r")) == NULL)
		panic("failed to open `%s': %s", file_name, strerror(errno));

	yyparse();

	::fclose(yyin);

	ProgramCode *code = new ProgramCode(*program);

	delete program;

	return code;
}
