#include <cstdio>
#include <cassert>

extern "C" {
#include "panic.h"
}

#include "icode.h"

namespace bc {

ExecuteCtx::ExecuteCtx(const ClassCode *clazz_, SpawnListener& on_spawn_)
: clazz(clazz_)
, on_spawn(on_spawn_)
, is_executing(false)
{
	class_fields.reserve(clazz->get_num_fields());
}

void
ExecuteCtx::prepare(const std::string& method_name, const DataList& args)
{
	if (is_executing)
		panic("can't call prepare while method in execution");

	assert(data_stack.size == 0 && call_stack.size == 0);

	const MethodCode *method = clazz->get_method_code(method_name);
	if (method == 0)
		panic("no such method: `%s'", method_name.c_str());

	if (method->get_return_type() != T_VOID)
		panic("return type for `%s' should be void", method_name.c_str());

	const unsigned num_args = method->get_num_args();
	if (args.size() != num_args)
		panic("invalid number of arguments for `%s' (got %u args, expected %u)",
		  method_name.c_str(), args.size(), num_args);

	DataList::const_iterator end = args.end();
	for (DataList::const_iterator i = args.begin(); i != end; i++)
		data_stack.push(*i);

	cur_state = ExecuteState(method->get_root_block(), data_stack.data,
	  &data_stack.data[data_stack.size]);

	data_stack.size += cur_state.cur_block->get_num_locals();
}

void
ExecuteCtx::prepare(const std::string& method_name)
{
	static const DataList empty_arg_list;
	prepare(method_name, empty_arg_list);
}

ExecuteCtx::ExecuteState::ExecuteState(const BlockCode *cur_block_, Data *cur_method_args_, Data *cur_locals_)
: cur_block(cur_block_)
, cur_method_args(cur_method_args_)
, cur_locals(cur_locals_)
, cur_insn(0)
{ }

Data&
ExecuteCtx::get_local_var(int index)
{
	const int var_index = index & 0xffff;
	const int block_index = index >> 16;

	Data *local_base;

	if (block_index == 0) {
		local_base = cur_state.cur_locals;
	} else {
		assert(call_stack.size >= block_index - 1);
		local_base = call_stack.data[call_stack.size - block_index].cur_locals;
	}

	return local_base[var_index];
}

const Data *
ExecuteCtx::get_field(const std::string& name) const
{
	std::pair<int, Type> index_and_type =
	  clazz->get_field_index_and_type(name);

	if (index_and_type.second == T_UNDEFINED)
		return 0;
	else
		return &class_fields[index_and_type.first];
}

bool
ExecuteCtx::execute()
{
	is_executing = true;

	bool must_yield = false;

	while (is_executing && !must_yield) {
		while (cur_state.cur_insn == cur_state.cur_block->insns.size()) {
			data_stack.size -= cur_state.cur_block->get_num_locals();

			if (cur_state.cur_block->is_method_root_block()) {
				const MethodCode& method = cur_state.cur_block->get_method();

				if (method.get_return_type() != T_VOID)
					panic("control reaches end of non-void function");

				const int num_args = method.get_num_args();
				assert(data_stack.size >= num_args);
				data_stack.size -= num_args;
			}

			if (call_stack.size == 0) {
				is_executing = false;
				break;
			}

			cur_state = call_stack.pop();
		}

		if (!is_executing)
			break;

		const Insn& cur_insn = cur_state.cur_block->insns[cur_state.cur_insn++];
		const Operand& operand = cur_insn.operand;

		switch (cur_insn.opcode) {
			case OP_BLOCK:
				call_stack.push(cur_state);
				cur_state = ExecuteState(operand.block, cur_state.cur_method_args,
				  &data_stack.data[data_stack.size]);
				data_stack.size += cur_state.cur_block->get_num_locals();
				break;

			case OP_FUNCALL:
				{
				const MethodCode *method = operand.method;

				const int num_args = method->get_num_args();

				Data *next_args;

				if (num_args == 0) {
					next_args = 0;
				} else {
					assert(data_stack.size >= num_args);
					next_args = &data_stack.data[data_stack.size - num_args];
				}

				call_stack.push(cur_state);
				cur_state = ExecuteState(method->get_root_block(), next_args,
				  &data_stack.data[data_stack.size]);
				data_stack.size += cur_state.cur_block->get_num_locals();
				}
				break;

			case OP_PUSH_INT_CONST:
				data_stack.push(operand.int_val);
				break;

			case OP_PUSH_FLOAT_CONST:
				data_stack.push(operand.float_val);
				break;

			case OP_PUSH_ARG:
				assert(cur_state.cur_method_args);
				data_stack.push(cur_state.cur_method_args[operand.int_val]);
				break;

			case OP_PUSH_FIELD:
				data_stack.push(class_fields[operand.int_val]);
				break;

			case OP_PUSH_LOCAL:
				data_stack.push(get_local_var(operand.int_val));
				break;

			case OP_POP_ARG:
				assert(cur_state.cur_method_args);
				cur_state.cur_method_args[operand.int_val] = data_stack.pop();
				break;

			case OP_POP_FIELD:
				class_fields[operand.int_val] = data_stack.pop();
				break;

			case OP_POP_LOCAL:
				get_local_var(operand.int_val) = data_stack.pop();
				break;

			case OP_DUP_TOP:
				data_stack.push(data_stack.data[data_stack.size - 1]);
				break;


#define HANDLE_BINOP(opcode, op) \
			case opcode##_INT: \
				{ \
				int v0 = data_stack.pop().int_val; \
				int v1 = data_stack.pop().int_val; \
				data_stack.push(v1 op v0); \
				} \
				break; \
			case opcode##_FLOAT: \
				{ \
				float v0 = data_stack.pop().float_val; \
				float v1 = data_stack.pop().float_val; \
				data_stack.push(v1 op v0); \
				} \
				break;
HANDLE_BINOP(OP_ADD, +)
HANDLE_BINOP(OP_SUB, -)
HANDLE_BINOP(OP_MUL, *)
HANDLE_BINOP(OP_DIV, /)
HANDLE_BINOP(OP_EQ, ==)
HANDLE_BINOP(OP_NE, !=)
HANDLE_BINOP(OP_LT, <)
HANDLE_BINOP(OP_GT, >)
HANDLE_BINOP(OP_LE, <=)
HANDLE_BINOP(OP_GE, >=)
#undef HANDLE_BINOP

			case OP_NEG_INT:
				data_stack.push(-data_stack.pop().int_val);
				break;

			case OP_NEG_FLOAT:
				data_stack.push(-data_stack.pop().float_val);
				break;

			case OP_RETURN:
				{
				Data rv = 0;

				const MethodCode& method = cur_state.cur_block->get_method();

				if (method.get_return_type() != T_VOID)
					rv = data_stack.pop();

				while (!cur_state.cur_block->is_method_root_block()) {
					data_stack.size -= cur_state.cur_block->get_num_locals();
					cur_state = call_stack.pop();
				}

				data_stack.size -=
				  cur_state.cur_block->get_num_locals() +
				  method.get_num_args();

				if (method.get_return_type() != T_VOID)
					data_stack.push(rv);

				if (call_stack.size > 0)
					cur_state = call_stack.pop();
				else
					is_executing = false;
				}
				break;
			
			case OP_PRINT_INT:
				fprintf(stderr, "PRINT_INT: %d\n", data_stack.pop().int_val);
				break;

			case OP_PRINT_FLOAT:
				fprintf(stderr, "PRINT_FLOAT: %f\n", data_stack.pop().float_val);
				break;

			case OP_POP:
				data_stack.pop();
				break;

			case OP_JUMP:
				cur_state.cur_insn = operand.int_val;
				break;

			case OP_COND_JUMP:
				if (data_stack.pop().int_val)
					cur_state.cur_insn = operand.int_val;
				break;

			case OP_COND_JUMP_UNLESS:
				if (!data_stack.pop().int_val)
					cur_state.cur_insn = operand.int_val;
				break;

			case OP_INT_TO_FLOAT:
				{
				int val = data_stack.top().int_val;
				data_stack.top().float_val = val;
				}
				break;

			case OP_FLOAT_TO_INT:
				{
				float val = data_stack.top().float_val;
				data_stack.top().int_val = static_cast<int>(val);
				}
				break;

			case OP_YIELD:
				must_yield = true;
				break;

			case OP_SPAWN:
				{
				const ClassCode *c =
				  clazz->get_program().get_class_code(*operand.str_val);

				ExecuteCtx *ctx = c->get_execute_ctx(on_spawn);

				int num_args = data_stack.pop().int_val;
				std::vector<Data> init_args;

				assert(data_stack.size >= num_args);

				for (int i = 0; i < num_args; i++)
					init_args.push_back(data_stack.data[data_stack.size - num_args + i]);

				data_stack.size -= num_args;

				ctx->prepare("INIT", init_args);
				ctx->execute();

				on_spawn(this, ctx);
				}
				break;

			default:
				assert(0);
		}
	}

	if (!is_executing)
		assert(data_stack.size == 0 && call_stack.size == 0);

	return is_executing;
}

ProgramCode::ProgramCode(const Program& program)
{
	const ClassList *class_list = program.class_list;

	ClassList::const_iterator end = class_list->end();
	for (ClassList::const_iterator i = class_list->begin(); i != end; i++) {
		Class *c = *i;
		classes[c->get_name()] = new ClassCode(*this, *c);
	}
}

ProgramCode::~ProgramCode()
{
	ClassMap::iterator i;

	for (i = classes.begin(); i != classes.end(); i++) {
		delete i->second;
	}
}

ExecuteCtx *
ProgramCode::get_execute_ctx(const std::string& clazz, SpawnListener& spawn_listener) const
{
	return get_class_code(clazz)->get_execute_ctx(spawn_listener);
}

const ClassCode *
ProgramCode::get_class_code(const std::string& clazz) const
{
	ClassMap::const_iterator i = classes.find(clazz);

	if (i == classes.end())
		panic("Undefined class `%s'", clazz.c_str());

	return i->second;
}

ClassCode::ClassCode(const ProgramCode& program_, const Class& clazz)
: program(program_)
{
	const MemberList *member_list = clazz.member_list;

	MemberList::const_iterator end = member_list->end();

	// initialize fields

	int num_fields = 0;

	for (MemberList::const_iterator i = member_list->begin(); i != end; i++) {
		if (!(*i)->is_method()) {
			const Field *f = dynamic_cast<const Field *>(*i);

			const std::string& name = f->get_name();
			if (fields.find(name) != fields.end())
				panic("redefinition of field `%s'", name.c_str());

			fields[name] = std::pair<int, Type>(num_fields++, f->get_type());
		}
	}

	// initialize method signatures

	for (MemberList::const_iterator i = member_list->begin(); i != end; i++) {
		if ((*i)->is_method()) {
			const Method *m = dynamic_cast<const Method *>(*i);

			const std::string& name = m->get_name();
			if (methods.find(name) != methods.end())
				panic("redefinition of method `%s'", name.c_str());

			methods[name] = new MethodCode(*this, *m);
		}
	}

	// compile method blocks
	
	for (MemberList::const_iterator i = member_list->begin(); i != end; i++) {
		if ((*i)->is_method()) {
			const Method *m = dynamic_cast<const Method *>(*i);

			MethodCode *mc = methods[m->get_name()];
			mc->set_root_block(new BlockCode(*mc, m->get_block(), 0));
		}
	}
}

ClassCode::~ClassCode()
{
	MethodMap::iterator i;

	for (i = methods.begin(); i != methods.end(); i++) {
		delete i->second;
	}
}

const ProgramCode &
ClassCode::get_program() const
{
	return program;
}

ExecuteCtx *
ClassCode::get_execute_ctx(SpawnListener& spawn_listener) const
{
	return new ExecuteCtx(this, spawn_listener);
}

const MethodCode *
ClassCode::get_method_code(const std::string& method) const
{
	MethodMap::const_iterator i = methods.find(method);

	if (i == methods.end())
		panic("Undefined method `%s'", method.c_str());

	return i->second;
}

std::pair<int, Type>
ClassCode::get_field_index_and_type(const std::string& name) const
{
	FieldMap::const_iterator i = fields.find(name);

	if (i != fields.end())
		return i->second;
	else
		return std::pair<int, Type>(-1, T_UNDEFINED);
}

int
ClassCode::get_num_fields() const
{
	return fields.size();
}

MethodCode::MethodCode(const ClassCode& clazz_, const Method& method)
: clazz(clazz_)
, return_type(method.type)
, root_block(0)
{
	const FormalList *formal_list = method.formal_list;

	FormalList::const_iterator end = formal_list->end();

	for (FormalList::const_iterator i = formal_list->begin(); i != end; i++) {
		const Formal *f = *i;
		args.push_back(std::pair<std::string, Type>(f->get_name(), f->get_type()));
	}
}

MethodCode::~MethodCode()
{
	if (root_block)
		delete root_block;
}

void
MethodCode::set_root_block(BlockCode *block)
{
	root_block = block;
}

const BlockCode *
MethodCode::get_root_block() const
{
	return root_block;
}

const ClassCode& 
MethodCode::get_class() const
{
	return clazz;
}

const Type
MethodCode::get_return_type() const
{
	return return_type;
}

int
MethodCode::get_num_args() const
{
	return args.size();
}

std::pair<int, Type>
MethodCode::get_arg_index_and_type(const std::string& name) const
{
	for (unsigned i = 0; i < args.size(); i++) {
		const std::pair<std::string, Type>& arg = args[i];

		if (arg.first == name)
			return std::pair<int, Type>(i, arg.second);
	}

	return std::pair<int, Type>(-1, T_UNDEFINED);
}

BlockCode::BlockCode(const MethodCode& method_, const BlockStmt& block, const BlockCode *parent_)
: method(method_)
, parent(parent_)
{
	CompileCtx ctx(*this);

	const StmtList *stmt_list = block.stmt_list;
	StmtList::const_iterator end = stmt_list->end();

	int num_locals = 0;

	for (StmtList::const_iterator i = stmt_list->begin(); i != end; i++) {
		Stmt *s = *i;

		if (!s->is_decl()) {
			s->compile(ctx);
		} else {
			const DeclStmt *d = dynamic_cast<DeclStmt *>(s);

			const Type type = d->get_type();
			const DeclStmt::SymInitList *syms = d->get_symbols();

			DeclStmt::SymInitList::const_iterator end = syms->end();

			for (DeclStmt::SymInitList::const_iterator j = syms->begin(); j != end; j++) {
				DeclStmt::SymInit *s = *j;

				const std::string& name = s->get_name();

				if (locals.find(name) != locals.end())
					panic("redefinition of local `%s'", name.c_str());

				locals[name] = std::pair<int, Type>(num_locals++, type);

				s->compile(ctx);
			}
		}
	}
}

BlockCode::~BlockCode()
{
	InsnList::iterator end = insns.end();

	for (InsnList::iterator i = insns.begin(); i != end; i++) {
		if (i->opcode == OP_BLOCK)
			delete i->operand.block;
	}
}

void
BlockCode::add_insn(Opcode opcode, Operand operand)
{
	insns.push_back(Insn(opcode, operand));
}

const MethodCode&
BlockCode::get_method() const
{
	return method;
}

bool
BlockCode::is_method_root_block() const
{
	return parent == 0;
}

const BlockCode *
BlockCode::get_parent() const
{
	return parent;
}

std::pair<int, Type>
BlockCode::get_local_index_and_type(const std::string& name) const
{
	LocalMap::const_iterator i = locals.find(name);

	if (i != locals.end())
		return i->second;
	else
		return std::pair<int, Type>(-1, T_UNDEFINED);
}

int
BlockCode::get_num_locals() const
{
	return locals.size();
}

int
BlockCode::get_cur_insn_index() const
{
	return insns.size();
}

void
BlockCode::set_insn_operand(unsigned index, Operand operand)
{
	assert(index < insns.size());
	insns[index].operand = operand;
}

CompileCtx::CompileCtx(BlockCode& block_code_)
: block_code(block_code_)
{ }

void
CompileCtx::emit_block(const BlockStmt& block)
{
	block_code.add_insn(OP_BLOCK, new BlockCode(block_code.get_method(), block, &block_code));
}

void
CompileCtx::emit_dup_top()
{
	block_code.add_insn(OP_DUP_TOP);
}

void
CompileCtx::emit_pop()
{
	block_code.add_insn(OP_POP);
}

void
CompileCtx::emit_return()
{
	block_code.add_insn(OP_RETURN);
}

void
CompileCtx::emit_print_int()
{
	block_code.add_insn(OP_PRINT_INT);
}

void
CompileCtx::emit_print_float()
{
	block_code.add_insn(OP_PRINT_FLOAT);
}

void
CompileCtx::emit_push_int_const(int value)
{
	block_code.add_insn(OP_PUSH_INT_CONST, value);
}

void
CompileCtx::emit_push_float_const(float value)
{
	block_code.add_insn(OP_PUSH_FLOAT_CONST, value);
}

void
CompileCtx::emit_push_arg(int index)
{
	block_code.add_insn(OP_PUSH_ARG, index);
}

void
CompileCtx::emit_push_local(int index)
{
	block_code.add_insn(OP_PUSH_LOCAL, index);
}

void
CompileCtx::emit_push_field(int index)
{
	block_code.add_insn(OP_PUSH_FIELD, index);
}

void
CompileCtx::emit_pop_arg(int index)
{
	block_code.add_insn(OP_POP_ARG, index);
}

void
CompileCtx::emit_pop_local(int index)
{
	block_code.add_insn(OP_POP_LOCAL, index);
}

void
CompileCtx::emit_pop_field(int index)
{
	block_code.add_insn(OP_POP_FIELD, index);
}

void
CompileCtx::emit_add_int()
{
	block_code.add_insn(OP_ADD_INT);
}

void
CompileCtx::emit_sub_int()
{
	block_code.add_insn(OP_SUB_INT);
}

void
CompileCtx::emit_mul_int()
{
	block_code.add_insn(OP_MUL_INT);
}

void
CompileCtx::emit_div_int()
{
	block_code.add_insn(OP_DIV_INT);
}

void
CompileCtx::emit_neg_int()
{
	block_code.add_insn(OP_NEG_INT);
}

void
CompileCtx::emit_eq_int()
{
	block_code.add_insn(OP_EQ_INT);
}

void
CompileCtx::emit_ne_int()
{
	block_code.add_insn(OP_NE_INT);
}

void
CompileCtx::emit_lt_int()
{
	block_code.add_insn(OP_LT_INT);
}

void
CompileCtx::emit_gt_int()
{
	block_code.add_insn(OP_GT_INT);
}

void
CompileCtx::emit_le_int()
{
	block_code.add_insn(OP_LE_INT);
}

void
CompileCtx::emit_ge_int()
{
	block_code.add_insn(OP_GE_INT);
}

void
CompileCtx::emit_add_float()
{
	block_code.add_insn(OP_ADD_FLOAT);
}

void
CompileCtx::emit_sub_float()
{
	block_code.add_insn(OP_SUB_FLOAT);
}

void
CompileCtx::emit_mul_float()
{
	block_code.add_insn(OP_MUL_FLOAT);
}

void
CompileCtx::emit_div_float()
{
	block_code.add_insn(OP_DIV_FLOAT);
}

void
CompileCtx::emit_neg_float()
{
	block_code.add_insn(OP_NEG_FLOAT);
}

void
CompileCtx::emit_eq_float()
{
	block_code.add_insn(OP_EQ_FLOAT);
}

void
CompileCtx::emit_ne_float()
{
	block_code.add_insn(OP_NE_FLOAT);
}

void
CompileCtx::emit_lt_float()
{
	block_code.add_insn(OP_LT_FLOAT);
}

void
CompileCtx::emit_gt_float()
{
	block_code.add_insn(OP_GT_FLOAT);
}

void
CompileCtx::emit_le_float()
{
	block_code.add_insn(OP_LE_FLOAT);
}

void
CompileCtx::emit_ge_float()
{
	block_code.add_insn(OP_GE_FLOAT);
}

void
CompileCtx::emit_jump(int index)
{
	block_code.add_insn(OP_JUMP, index);
}

void
CompileCtx::emit_cond_jump(int index)
{
	block_code.add_insn(OP_COND_JUMP, index);
}

void
CompileCtx::emit_cond_jump_unless(int index)
{
	block_code.add_insn(OP_COND_JUMP_UNLESS, index);
}

void
CompileCtx::emit_funcall(const std::string& name)
{
	const MethodCode *method = block_code.get_method().get_class().get_method_code(name);
	block_code.add_insn(OP_FUNCALL, method);
}

void
CompileCtx::emit_int_to_float()
{
	block_code.add_insn(OP_INT_TO_FLOAT);
}

void
CompileCtx::emit_float_to_int()
{
	block_code.add_insn(OP_FLOAT_TO_INT);
}

void
CompileCtx::emit_yield()
{
	block_code.add_insn(OP_YIELD);
}

void
CompileCtx::emit_spawn(const std::string& name)
{
	block_code.add_insn(OP_SPAWN, new std::string(name));
}

const BlockCode&
CompileCtx::get_block() const
{
	return block_code;
}

const MethodCode&
CompileCtx::get_method() const
{
	return block_code.get_method();
}

const ClassCode&
CompileCtx::get_class() const
{
	return get_method().get_class();
}

int
CompileCtx::get_cur_insn_index() const
{
	return block_code.get_cur_insn_index();
}

void
CompileCtx::set_insn_operand(unsigned index, Operand operand)
{
	block_code.set_insn_operand(index, operand);
}

} // bc
