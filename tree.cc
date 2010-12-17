#include <cstdio>
#include <cassert>

extern "C" {
#include "panic.h"
}

#include "tree.h"
#include "icode.h"

namespace bc {

//
//  T r e e N o d e
//

Node::Node()
{ }

Node::~Node()
{ }

//
//  P r o g r a m
//

Program::Program(ClassList *class_list_)
: class_list(class_list_)
{ }

Program::~Program()
{
	ClassList::iterator end = class_list->end();
	for (ClassList::iterator i = class_list->begin(); i != end; i++)
		delete *i;
	delete class_list;
}

//
//  C l a s s
//

Class::Class(Symbol *name_, MemberList *member_list_)
: name(name_)
, member_list(member_list_)
{ }

Class::~Class()
{
	MemberList::iterator end = member_list->end();
	for (MemberList::iterator i = member_list->begin(); i != end; i++)
		delete *i;
	delete member_list;
	delete name;

}

const std::string&
Class::get_name() const
{
	return name->name;
}

//
//  M e m b e r
//

Member::Member(Type type_, Symbol *name_)
: type(type_)
, name(name_)
{ }

Member::~Member()
{ 
	delete name;
}

const std::string&
Member::get_name() const
{
	return name->name;
}

Type
Member::get_type() const
{
	return type;
}


//
//  F i e l d
//

Field::Field(Type type_, Symbol *name_)
: Member(type_, name_)
{ }

bool
Field::is_method() const
{
	return false;
}

//
//  M e t h o d
//

Method::Method(Type type_, Symbol *name_, FormalList *formal_list_, BlockStmt *block_)
: Member(type_, name_)
, formal_list(formal_list_)
, block(block_)
{ }

Method::~Method()
{
	delete block;
	FormalList::iterator end = formal_list->end();
	for (FormalList::iterator i = formal_list->begin(); i != end; i++)
		delete *i;
	delete formal_list;
}

bool
Method::is_method() const
{
	return true;
}

const BlockStmt&
Method::get_block() const
{
	return *block;
}

//
//  F o r m a l
//

Formal::Formal(Type type_, Symbol *name_)
: type(type_)
, name(name_)
{ }

Formal::~Formal()
{
	delete name;
}

const std::string&
Formal::get_name() const
{
	return name->name;
}

Type
Formal::get_type() const
{
	return type;
}

//
//  S t m t
//

Stmt::Stmt()
{ }

Stmt::~Stmt()
{ }

bool
Stmt::is_decl() const
{
	return false;
}

//
//  E x p r e s s i o n S t m t
//

ExprStmt::ExprStmt(Expr *expr_)
: expr(expr_)
{ }

ExprStmt::~ExprStmt()
{
	delete expr;
}

void
ExprStmt::compile(CompileCtx& ctx)
{
	if (expr->compile(ctx) != T_VOID)
		ctx.emit_pop();
}

//
//  B l o c k S t m t
//

BlockStmt::BlockStmt(StmtList *stmt_list_)
: stmt_list(stmt_list_)
{ }

BlockStmt::~BlockStmt()
{
	StmtList::iterator end = stmt_list->end();
	for (StmtList::iterator i = stmt_list->begin(); i != end; i++)
		delete *i;
	delete stmt_list;
}

void
BlockStmt::compile(CompileCtx& ctx)
{
	ctx.emit_block(*this);
}

//
//  D e c l S t m t
//

DeclStmt::DeclStmt(Type type_, SymInitList *sym_init_list_)
: type(type_)
, sym_init_list(sym_init_list_)
{ }

DeclStmt::~DeclStmt()
{ 
	SymInitList::iterator end = sym_init_list->end();
	for (SymInitList::iterator i = sym_init_list->begin(); i != end; i++)
		delete *i;
	delete sym_init_list;
}

Type
DeclStmt::get_type() const
{
	return type;
}

const DeclStmt::SymInitList *
DeclStmt::get_symbols() const
{
	return sym_init_list;
}

void
DeclStmt::compile(CompileCtx& ctx)
{
	assert(0);
}

bool
DeclStmt::is_decl() const
{
	return true;
}

DeclStmt::SymInit::SymInit(Symbol *name_, Expr *init_)
: name(name_)
, init(init_)
{ }

DeclStmt::SymInit::~SymInit()
{
	delete name;
	if (init)
		delete init;
}

const std::string&
DeclStmt::SymInit::get_name() const
{
	return name->name;
}

#define COMPILE_NOT_VOID(expr) \
  ({ Type t = (expr)->compile(ctx); \
    if (t == T_VOID) \
	    panic("void value not ignored as it ought to be"); \
    t; })

#define GET_TYPE_NOT_VOID(expr) \
  ({ Type t = (expr)->get_type(ctx); \
    if (t == T_VOID) \
	    panic("void value not ignored as it ought to be"); \
    t; })

void
DeclStmt::SymInit::compile(CompileCtx& ctx)
{
	if (init) {
		Type tr = COMPILE_NOT_VOID(init);

		std::pair<int, Type> index_type = ctx.get_block().get_local_index_and_type(get_name());

		assert(index_type.second != T_UNDEFINED);

		if (index_type.second == T_INTEGER) {
			if (tr == T_FLOAT)
				ctx.emit_float_to_int();
		} else {
			assert(index_type.second == T_FLOAT);
			if (tr == T_INTEGER)
				ctx.emit_int_to_float();
		}

		ctx.emit_pop_local(index_type.first);
	}
}

//
//  R e t u r n S t m t
//

ReturnStmt::ReturnStmt(Expr *expr_)
: expr(expr_)
{ }

ReturnStmt::~ReturnStmt()
{
	if (expr)
		delete expr;
}

void
ReturnStmt::compile(CompileCtx& ctx)
{
	Type rt = ctx.get_method().get_return_type();

	if (expr) {
		if (rt == T_VOID)
			panic("return statement with a value, in function returning `void'");

		Type et = COMPILE_NOT_VOID(expr);

		if (rt == T_INTEGER) {
			if (et == T_FLOAT)
				ctx.emit_float_to_int();
		} else {
			assert(rt == T_FLOAT);
			if (et == T_INTEGER)
				ctx.emit_int_to_float();
		}
	} else if (rt != T_VOID) {
		panic("return statement with no value, in function returning `int'");
	}

	ctx.emit_return();
}

//
//  P r i n t S t m t
//

PrintStmt::PrintStmt(Expr *expr_)
: expr(expr_)
{ }

PrintStmt::~PrintStmt()
{
	delete expr;
}

void
PrintStmt::compile(CompileCtx& ctx)
{
	Type t = COMPILE_NOT_VOID(expr);

	switch (t) {
		case T_INTEGER:
			ctx.emit_print_int();
			break;

		case T_FLOAT:
			ctx.emit_print_float();
			break;

		default:
			assert(0);
	}
}

//
//  I f S t m t
//

IfStmt::IfStmt(Expr *cond_, Stmt *if_stmt_)
: cond(cond_)
, if_stmt(if_stmt_)
, else_stmt(0)
{ }

IfStmt::~IfStmt()
{
	delete cond;
	delete if_stmt;
	if (else_stmt)
		delete else_stmt;
}

void
IfStmt::compile(CompileCtx& ctx)
{
	COMPILE_NOT_VOID(cond);

	ctx.emit_cond_jump_unless(-1);
	int cond_jump = ctx.get_cur_insn_index() - 1;

	if_stmt->compile(ctx);

	if (!else_stmt) {
		ctx.set_insn_operand(cond_jump, ctx.get_cur_insn_index());
	} else {
		ctx.emit_jump(-1);
		int jump = ctx.get_cur_insn_index() - 1;

		ctx.set_insn_operand(cond_jump, ctx.get_cur_insn_index());

		else_stmt->compile(ctx);

		ctx.set_insn_operand(jump, ctx.get_cur_insn_index());
	}
}

void
IfStmt::set_else_stmt(Stmt *else_stmt_)
{
	else_stmt = else_stmt_;
}

//
//  W h i l e S t m t
//

WhileStmt::WhileStmt(Expr *cond_, Stmt *stmt_)
: cond(cond_)
, stmt(stmt_)
{ }

WhileStmt::~WhileStmt()
{
	delete cond;
	delete stmt;
}

void
WhileStmt::compile(CompileCtx& ctx)
{
	int top = ctx.get_cur_insn_index();

	COMPILE_NOT_VOID(cond);

	ctx.emit_cond_jump_unless(-1);
	int cond_jump = ctx.get_cur_insn_index() - 1;

	stmt->compile(ctx);
	ctx.emit_jump(top);

	ctx.set_insn_operand(cond_jump, ctx.get_cur_insn_index());
}

//
//  F o r S t m t
//

ForStmt::ForStmt(Expr *pre_, Expr *cond_, Expr *post_, Stmt *stmt_)
: pre(pre_)
, cond(cond_)
, post(post_)
, stmt(stmt_)
{ }

ForStmt::~ForStmt()
{
	delete pre;
	delete cond;
	delete post;
	delete stmt;
}

void
ForStmt::compile(CompileCtx& ctx)
{
	if (pre->compile(ctx) != T_VOID)
		ctx.emit_pop();

	int top = ctx.get_cur_insn_index();

	COMPILE_NOT_VOID(cond);

	ctx.emit_cond_jump_unless(-1);
	int cond_jump = ctx.get_cur_insn_index() - 1;

	stmt->compile(ctx);

	if (post->compile(ctx) != T_VOID)
		ctx.emit_pop();

	ctx.emit_jump(top);

	ctx.set_insn_operand(cond_jump, ctx.get_cur_insn_index());
}

//
//  Y i e l d
//

YieldStmt::YieldStmt()
{ }

YieldStmt::~YieldStmt()
{ }

void
YieldStmt::compile(CompileCtx& ctx)
{
	ctx.emit_yield();
}

//
//  S p a w n
//

SpawnStmt::SpawnStmt(Symbol *class_name_, ExprList *init_args_)
: class_name(class_name_)
, init_args(init_args_)
{ }

SpawnStmt::~SpawnStmt()
{
	delete class_name;
	ExprList::iterator end = init_args->end();
	for (ExprList::iterator i = init_args->begin(); i != end; i++)
		delete *i;
	delete init_args;
}

void
SpawnStmt::compile(CompileCtx& ctx)
{
	ExprList::iterator end = init_args->end();
	for (ExprList::iterator i = init_args->begin(); i != end; i++)
		COMPILE_NOT_VOID(*i);
	ctx.emit_push_int_const(init_args->size());
	ctx.emit_spawn(class_name->name);
}

//
//  E x p r
//

Expr::Expr()
{ }

Expr::~Expr()
{ }

//
//  I n t L i t e r a l
//

IntLiteral::IntLiteral(int value_)
: value(value_)
{ }

IntLiteral::~IntLiteral()
{ }

Type
IntLiteral::compile(CompileCtx& ctx)
{
	ctx.emit_push_int_const(value);

	return T_INTEGER;
}

Type
IntLiteral::get_type(CompileCtx& ctx)
{
	return T_INTEGER;
}

//
//  F l o a t L i t e r a l
//

FloatLiteral::FloatLiteral(float value_)
: value(value_)
{ }

FloatLiteral::~FloatLiteral()
{ }

Type
FloatLiteral::compile(CompileCtx& ctx)
{
	ctx.emit_push_float_const(value);

	return T_FLOAT;
}

Type
FloatLiteral::get_type(CompileCtx& ctx)
{
	return T_FLOAT;
}

//
//  S y m b o l R e f
//

SymRef::SymRef(Symbol *symbol_)
: symbol(symbol_)
, scope(S_UNRESOLVED)
{ }

SymRef::~SymRef()
{
	delete symbol;
}

Type
SymRef::compile(CompileCtx& ctx)
{
	resolve_scope(ctx);

	switch (scope) {
		case S_METHOD_ARGUMENT:
			ctx.emit_push_arg(symbol_index);
			break;

		case S_BLOCK_LOCAL:
			ctx.emit_push_local(symbol_index);
			break;

		case S_CLASS_FIELD:
			ctx.emit_push_field(symbol_index);
			break;

		default:
			assert(0);
	}

	return type;
}

void
SymRef::compile_pop(CompileCtx& ctx)
{
	resolve_scope(ctx);

	switch (scope) {
		case S_METHOD_ARGUMENT:
			ctx.emit_pop_arg(symbol_index);
			break;

		case S_BLOCK_LOCAL:
			ctx.emit_pop_local(symbol_index);
			break;

		case S_CLASS_FIELD:
			ctx.emit_pop_field(symbol_index);
			break;

		default:
			assert(0);
	}
}

void
SymRef::resolve_scope(const CompileCtx& ctx)
{
	if (scope == S_UNRESOLVED) {
		const std::string& name = symbol->name;

		//  local?

		const BlockCode *block = &ctx.get_block();

		int block_index = 0;

		while (scope == S_UNRESOLVED && block) {
			std::pair<int, Type> local = block->get_local_index_and_type(name);

			if (local.second != T_UNDEFINED) {
				symbol_index = local.first | (block_index << 16);
				type = local.second;
				scope = S_BLOCK_LOCAL;
				return;
			}

			block = block->get_parent();
			++block_index;
		}

		//  method argument?

		std::pair<int, Type> arg = ctx.get_method().get_arg_index_and_type(name);

		if (arg.second != T_UNDEFINED) {
			symbol_index = arg.first;
			type = arg.second;
			scope = S_METHOD_ARGUMENT;
			return;
		}

		//  class field?

		std::pair<int, Type> field = ctx.get_class().get_field_index_and_type(name);

		if (field.second != T_UNDEFINED) {
			symbol_index = field.first;
			type = field.second;
			scope = S_CLASS_FIELD;
			return;
		}

		panic("`%s' was not declared in this scope", name.c_str());
	}
}

Type
SymRef::get_type(CompileCtx& ctx)
{
	resolve_scope(ctx);
	return type;
}

//
//  B i n a r y O p
//

BinaryOp::BinaryOp(BinaryOpType type_, Expr *left_, Expr *right_)
: binop_type(type_)
, left(left_)
, right(right_)
{ }

BinaryOp::~BinaryOp()
{
	delete left;
	delete right;
}

Type
BinaryOp::compile(CompileCtx& ctx)
{
	Type rt;

	switch (binop_type) {
#define COMPILE_ARITH(type, method) \
		case BT_##type: \
			{ \
			Type tl = COMPILE_NOT_VOID(left); \
			Type tr = GET_TYPE_NOT_VOID(right); \
			if (tr == T_FLOAT) { \
				if (tl == T_INTEGER) \
					ctx.emit_int_to_float(); \
				COMPILE_NOT_VOID(right); \
				ctx.emit_##method##_float(); \
				rt = T_FLOAT; \
			} else { \
				assert(tr == T_INTEGER); \
				COMPILE_NOT_VOID(right); \
				if (tl == T_FLOAT) { \
					ctx.emit_int_to_float(); \
					ctx.emit_##method##_float(); \
					rt = T_FLOAT; \
				} else { \
					ctx.emit_##method##_int(); \
					rt = T_INTEGER; \
				} \
			} \
			} \
			break;
COMPILE_ARITH(ADD, add)
COMPILE_ARITH(SUB, sub)
COMPILE_ARITH(MUL, mul)
COMPILE_ARITH(DIV, div)
#undef COMPILE_ARITH

#define COMPILE_REL(type, method) \
		case BT_##type: \
			{ \
			Type tl = COMPILE_NOT_VOID(left); \
			Type tr = GET_TYPE_NOT_VOID(right); \
			if (tr == T_FLOAT) { \
				if (tl == T_INTEGER) \
					ctx.emit_int_to_float(); \
				COMPILE_NOT_VOID(right); \
				ctx.emit_##method##_float(); \
			} else { \
				assert(tr == T_INTEGER); \
				COMPILE_NOT_VOID(right); \
				if (tl == T_FLOAT) { \
					ctx.emit_int_to_float(); \
					ctx.emit_##method##_float(); \
				} else { \
					ctx.emit_##method##_int(); \
				} \
			} \
			rt = T_INTEGER; \
			} \
			break;
COMPILE_REL(EQ, eq)
COMPILE_REL(NE, ne)
COMPILE_REL(LT, lt)
COMPILE_REL(GT, gt)
COMPILE_REL(LE, le)
COMPILE_REL(GE, ge)
#undef COMPILE_REL

		case BT_ASSIGN:
			{
			SymRef *sym = dynamic_cast<SymRef *>(left);
			if (sym == 0)
				panic("invalid value in assignment");
			Type tr = COMPILE_NOT_VOID(right);
			Type tl = GET_TYPE_NOT_VOID(left);
			if (tl == T_INTEGER) {
				if (tr == T_FLOAT)
					ctx.emit_float_to_int();
			} else {
				assert(tl == T_FLOAT);
				if (tr == T_INTEGER)
					ctx.emit_int_to_float();
			}
			ctx.emit_dup_top();
			sym->compile_pop(ctx);
			rt = tl;
			}
			break;

#define COMPILE_ARITH_ASSIGN(type, method) \
		case BT_##type##_ASSIGN: \
			{ \
			SymRef *sym = dynamic_cast<SymRef *>(left); \
			if (sym == 0) \
				panic("invalid value in assignment"); \
			Type tl = COMPILE_NOT_VOID(left); \
			Type tr = COMPILE_NOT_VOID(right); \
			if (tl == T_INTEGER) { \
				if (tr == T_FLOAT) \
					ctx.emit_float_to_int(); \
				ctx.emit_##method##_int(); \
			} else { \
				assert(tl == T_FLOAT); \
				if (tr == T_INTEGER) \
					ctx.emit_int_to_float(); \
				ctx.emit_##method##_float(); \
			} \
			ctx.emit_dup_top(); \
			sym->compile_pop(ctx); \
			rt = tl; \
			} \
			break;
COMPILE_ARITH_ASSIGN(ADD, add)
COMPILE_ARITH_ASSIGN(SUB, sub)
COMPILE_ARITH_ASSIGN(MUL, mul)
COMPILE_ARITH_ASSIGN(DIV, div)
#undef COMPILE_ARITH_ASSIGN

		case BT_AND:
			{
			Type tl = COMPILE_NOT_VOID(left);
			if (tl != T_INTEGER) {
				assert(tl == T_FLOAT);
				ctx.emit_float_to_int();
			}

			ctx.emit_cond_jump_unless(-1);
			int cond_jump_1 = ctx.get_cur_insn_index() - 1;

			Type tr = COMPILE_NOT_VOID(right);
			if (tr != T_INTEGER) {
				assert(tr == T_FLOAT);
				ctx.emit_float_to_int();
			}

			ctx.emit_cond_jump_unless(-1);
			int cond_jump_2 = ctx.get_cur_insn_index() - 1;

			ctx.emit_push_int_const(1);

			ctx.emit_jump(-1);
			int jump = ctx.get_cur_insn_index() - 1;

			ctx.set_insn_operand(cond_jump_1, ctx.get_cur_insn_index());
			ctx.set_insn_operand(cond_jump_2, ctx.get_cur_insn_index());

			ctx.emit_push_int_const(0);

			ctx.set_insn_operand(jump, ctx.get_cur_insn_index());

			rt = T_INTEGER;
			}
			break;

		case BT_OR:
			{
			Type tl = COMPILE_NOT_VOID(left);
			if (tl != T_INTEGER) {
				assert(tl == T_FLOAT);
				ctx.emit_float_to_int();
			}

			ctx.emit_cond_jump(-1);
			int cond_jump_1 = ctx.get_cur_insn_index() - 1;

			Type tr = COMPILE_NOT_VOID(right);
			if (tr != T_INTEGER) {
				assert(tr == T_FLOAT);
				ctx.emit_float_to_int();
			}

			ctx.emit_cond_jump(-1);
			int cond_jump_2 = ctx.get_cur_insn_index() - 1;

			ctx.emit_push_int_const(0);

			ctx.emit_jump(-1);
			int jump = ctx.get_cur_insn_index() - 1;

			ctx.set_insn_operand(cond_jump_1, ctx.get_cur_insn_index());
			ctx.set_insn_operand(cond_jump_2, ctx.get_cur_insn_index());

			ctx.emit_push_int_const(1);

			ctx.set_insn_operand(jump, ctx.get_cur_insn_index());

			rt = T_INTEGER;
			}
			break;

		default:
			assert(0);
	}

	return rt;
}

Type
BinaryOp::get_type(CompileCtx& ctx)
{
	if (is_relational_op()) {
		return T_INTEGER;
	} else {
		// XXX: if assignment should return type of var

		Type tl = GET_TYPE_NOT_VOID(left);
		Type tr = GET_TYPE_NOT_VOID(right);

		if (tl == T_FLOAT || tr == T_FLOAT) {
			return T_FLOAT;
		} else {
			return T_INTEGER;
		}
	}
}

bool
BinaryOp::is_relational_op() const
{
	return
	  binop_type == BT_EQ ||
	  binop_type == BT_NE ||
	  binop_type == BT_LT ||
	  binop_type == BT_GT ||
	  binop_type == BT_LE ||
	  binop_type == BT_GE ||
	  binop_type == BT_AND ||
	  binop_type == BT_OR;
}

//
//  U n a r y O p
//

UnaryOp::UnaryOp(UnaryOpType type_, Expr *down_)
: unop_type(type_)
, down(down_)
{ }

UnaryOp::~UnaryOp()
{
	delete down;
}

Type
UnaryOp::compile(CompileCtx& ctx)
{
	Type t = COMPILE_NOT_VOID(down);

	switch (unop_type) {
		case UT_NEG:
			ctx.emit_neg_int();
			break;

#define COMPILE_POST_OP(type, method) \
		case UT_POST_##type: \
			{ \
			SymRef *sym = dynamic_cast<SymRef *>(down); \
			if (sym == 0) \
				panic("invalid value in assignmemnt"); \
			ctx.emit_dup_top(); \
			Type dt = sym->get_type(ctx); \
			if (dt == T_INTEGER) { \
				ctx.emit_push_int_const(1); \
				ctx.emit_##method##_int(); \
			} else { \
				assert(dt == T_FLOAT); \
				ctx.emit_push_float_const(1); \
				ctx.emit_##method##_float(); \
			} \
			sym->compile_pop(ctx); \
			} \
			break;

COMPILE_POST_OP(INCR, add)
COMPILE_POST_OP(DECR, sub)

		default:
			assert(0);
	}

	return t;
}

Type
UnaryOp::get_type(CompileCtx& ctx)
{
	return GET_TYPE_NOT_VOID(down);
}

//
//  F u n c t i o n C a l l
//

FunCall::FunCall(Symbol *fun_name_, ExprList *args_)
: fun_name(fun_name_)
, args(args_)
{ }

FunCall::~FunCall()
{
	delete fun_name;
	ExprList::iterator end = args->end();
	for (ExprList::iterator i = args->begin(); i != end; i++)
		delete *i;
	delete args;
}

Type
FunCall::compile(CompileCtx& ctx)
{
	const MethodCode *method = ctx.get_class().get_method_code(fun_name->name);

	MethodCode::ArgList method_args = method->args;

	if (method_args.size() != args->size())
		panic("invalid number of parameters");

	for (unsigned i = 0; i < args->size(); i++) {
		Expr *expr = (*args)[i];

		Type expr_type = COMPILE_NOT_VOID(expr);
		Type arg_type = method_args[i].second;

		if (arg_type == T_INTEGER) {
			if (expr_type == T_FLOAT)
				ctx.emit_float_to_int();
		} else {
			assert(arg_type == T_FLOAT);
			if (expr_type == T_INTEGER)
				ctx.emit_int_to_float();
		}
	}

	ctx.emit_funcall(fun_name->name);

	return method->get_return_type();
}

Type
FunCall::get_type(CompileCtx& ctx)
{
	const MethodCode *method = ctx.get_class().get_method_code(fun_name->name);

	return method->get_return_type();
}

} // bc
