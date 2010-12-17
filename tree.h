#ifndef TREE_H_
#define TREE_H_

#include <string>
#include <vector>

namespace bc {

class Class;
typedef std::vector<Class *> ClassList;

class Member;
typedef std::vector<Member *> MemberList;

class Formal;
typedef std::vector<Formal *> FormalList;

class Stmt;
typedef std::vector<Stmt *> StmtList;

class BlockStmt;

class Expr;
typedef std::vector<Expr *> ExprList;

class ProgramCode;
class ClassCode;
class MethodCode;
class BlockCode;

class CompileCtx;

enum Type {
	T_FLOAT,
	T_INTEGER,
	T_VOID,

	NUM_TYPES,

	T_UNDEFINED = -1,
};

class Symbol {
  public:
	Symbol(const char *name_) : name(name_) { }
	std::string name;
};

class Node {
  public:
	Node();
	virtual ~Node();
};

class Program : public Node {
friend class ProgramCode;

  public:
	Program(ClassList *class_list_);
	virtual ~Program();

  protected:
	ClassList *class_list;
};

class Class : public Node {
friend class ClassCode;

  public:
	Class(Symbol *name_, MemberList *member_list_);
	virtual ~Class();

	const std::string& get_name() const;

  protected:
	Symbol *name;
	MemberList *member_list;
};

class Member : public Node {
  public:
	Member(Type type_, Symbol *name_);
	virtual ~Member();

	virtual bool is_method() const = 0;

	const std::string& get_name() const;
	Type get_type() const;

  protected:
	Type type;
	Symbol *name;
};

class Field : public Member {
  public:
	Field(Type type_, Symbol *name_);

	virtual bool is_method() const;
};

class Method : public Member {
friend class MethodCode;
friend class Class;

  public:
	Method(Type type_, Symbol *name_, FormalList *formal_list_, BlockStmt *block_);
	virtual ~Method();

	virtual bool is_method() const;
	const BlockStmt& get_block() const;

  protected:
	FormalList *formal_list;
	BlockStmt *block;
};

class Formal : public Node {
  public:
	Formal(Type type_, Symbol *name_);
	virtual ~Formal();

	const std::string& get_name() const;
	Type get_type() const;

  protected:
	Type type;
	Symbol *name;
};

class Stmt : public Node {
  public:
	Stmt();
	virtual ~Stmt();

	virtual void compile(CompileCtx& ctx) = 0;
	virtual bool is_decl() const;
};

class ExprStmt : public Stmt {
  public:
	ExprStmt(Expr *expr_);
	virtual ~ExprStmt();

	virtual void compile(CompileCtx& ctx);

  protected:
	Expr *expr;
};

class BlockStmt : public Stmt {
friend class BlockCode;

  public:
	BlockStmt(StmtList *stmt_list_);
	virtual ~BlockStmt();

	virtual void compile(CompileCtx& ctx);

  protected:
	StmtList *stmt_list;
};

class ReturnStmt : public Stmt {
  public:
	ReturnStmt(Expr *expr_);
	virtual ~ReturnStmt();

	virtual void compile(CompileCtx& ctx);

  protected:
	Expr *expr;
};

class PrintStmt : public Stmt {
  public:
	PrintStmt(Expr *expr_);
	virtual ~PrintStmt();

	virtual void compile(CompileCtx& ctx);

  protected:
	Expr *expr;
};

class DeclStmt : public Stmt {
  public:
	class SymInit {
	  public:
		SymInit(Symbol *name_, Expr *init_);
		virtual ~SymInit();

		const std::string& get_name() const;
		void compile(CompileCtx& ctx);
		
	  protected:
		Symbol *name;
		Expr *init;
	};

	typedef std::vector<SymInit *> SymInitList;

	DeclStmt(Type type_, SymInitList *sym_init_list_);
	virtual ~DeclStmt();

	Type get_type() const;
	const SymInitList *get_symbols() const;

	virtual void compile(CompileCtx& ctx);
	virtual bool is_decl() const;

  protected:
	Type type;
	SymInitList *sym_init_list;
};

class IfStmt : public Stmt
{
  public:
	IfStmt(Expr *cond_, Stmt *if_stmt_);
	virtual ~IfStmt();

	void set_else_stmt(Stmt *else_stmt_);

	virtual void compile(CompileCtx& ctx);

  protected:
	Expr *cond;
	Stmt *if_stmt;
	Stmt *else_stmt;
};

class WhileStmt : public Stmt
{
  public:
	WhileStmt(Expr *cond_, Stmt *stmt_);
	virtual ~WhileStmt();

	virtual void compile(CompileCtx& ctx);

  protected:
	Expr *cond;
	Stmt *stmt;
};

class ForStmt : public Stmt
{
  public:
	ForStmt(Expr *pre_, Expr *cond_, Expr *post_, Stmt *stmt_);
	virtual ~ForStmt();

	virtual void compile(CompileCtx& ctx);

  protected:
	Expr *pre;
	Expr *cond;
	Expr *post;
	Stmt *stmt;
};

class YieldStmt : public Stmt
{
  public:
	YieldStmt();
	virtual ~YieldStmt();

	virtual void compile(CompileCtx& ctx);
};

class SpawnStmt : public Stmt
{
  public:
	SpawnStmt(Symbol *class_name_, ExprList *init_args_);
	virtual ~SpawnStmt();

	virtual void compile(CompileCtx& ctx);

  protected:
	Symbol *class_name;
	ExprList *init_args;
};

class Expr : public Node {
  public:
	Expr();
	virtual ~Expr();

	virtual Type compile(CompileCtx& ctx) = 0;
	virtual Type get_type(CompileCtx& ctx) = 0;
};

class IntLiteral : public Expr {
  public:
	IntLiteral(int value_);
	virtual ~IntLiteral();

	virtual Type compile(CompileCtx& ctx);
	virtual Type get_type(CompileCtx& ctx);

  protected:
	int value;
};

class FloatLiteral : public Expr {
  public:
	FloatLiteral(float value_);
	virtual ~FloatLiteral();

	virtual Type compile(CompileCtx& ctx);
	virtual Type get_type(CompileCtx& ctx);

  protected:
	float value;
};

class SymRef : public Expr {
  public:
	SymRef(Symbol *symbol_);
	virtual ~SymRef();

	virtual Type compile(CompileCtx& ctx);
	void compile_pop(CompileCtx& ctx);
	virtual Type get_type(CompileCtx& ctx);

  protected:
	void resolve_scope(const CompileCtx& ctx);

	enum SymbolScope {
		S_METHOD_ARGUMENT,
		S_BLOCK_LOCAL,
		S_CLASS_FIELD,
		S_UNRESOLVED,
	};

	Symbol *symbol;
	SymbolScope scope;
	int symbol_index;
	Type type;
};

enum BinaryOpType {
	BT_ADD,
	BT_SUB,
	BT_MUL,
	BT_DIV,
	BT_ASSIGN,
	BT_ADD_ASSIGN,
	BT_SUB_ASSIGN,
	BT_MUL_ASSIGN,
	BT_DIV_ASSIGN,
	BT_EQ,
	BT_NE,
	BT_LT,
	BT_GT,
	BT_LE,
	BT_GE,
	BT_AND,
	BT_OR,
	NUM_BINOP_TYPES,
};

class BinaryOp : public Expr {
  public:
	BinaryOp(BinaryOpType type_, Expr *left_, Expr *right_);
	virtual ~BinaryOp();

	virtual Type compile(CompileCtx& ctx);
	virtual Type get_type(CompileCtx& ctx);

  protected:
	bool is_relational_op() const;

	BinaryOpType binop_type;
	Expr *left;
	Expr *right;
};

enum UnaryOpType {
	UT_NEG,
	UT_POST_INCR,
	UT_POST_DECR,
	NUM_UNOP_TYPES,
};

class UnaryOp : public Expr {
  public:
	UnaryOp(UnaryOpType type_, Expr *down_);
	virtual ~UnaryOp();

	virtual Type compile(CompileCtx& ctx);
	virtual Type get_type(CompileCtx& ctx);

  protected:
	UnaryOpType unop_type;
	Expr *down;
};

class FunCall : public Expr {
  public:
	FunCall(Symbol *fun_name_, ExprList *args_);
	virtual ~FunCall();

	virtual Type compile(CompileCtx& ctx);
	virtual Type get_type(CompileCtx& ctx);

  protected:
	Symbol *fun_name;
	ExprList *args;
};

} // bc

#endif
