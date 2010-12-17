#ifndef ICODE_H_
#define ICODE_H_

#include <vector>
#include <map>
#include <string>
#include <deque>

#include "panic.h"
#include "tree.h"

namespace bc {

enum Opcode {
	OP_BLOCK,		// enter block
	OP_DUP_TOP,		// duplicate top of stack
	OP_PUSH_INT_CONST,	// push integer constant
	OP_PUSH_FLOAT_CONST,
	OP_PUSH_ARG_INT,	// push value of method argument
	OP_PUSH_LOCAL,		// push value of local variable
	OP_PUSH_FIELD,		// push value of class member
	OP_PUSH_ARG,		// push value of method argument
	OP_PUSH_LOCAL_FLOAT,	// push value of local variable
	OP_PUSH_FIELD_FLOAT,	// push value of class member
	OP_ADD_INT,		// pop 2 elements, add, and push result
	OP_SUB_INT,		// pop 2 elements, subtract, and push result
	OP_MUL_INT,		// pop 2 elements, multiply, and push result
	OP_DIV_INT,		// pop 2 elements, divide, and push result
	OP_NEG_INT,		// negate element on top of the stack
	OP_EQ_INT,
	OP_NE_INT,
	OP_LT_INT,
	OP_GT_INT,
	OP_LE_INT,
	OP_GE_INT,
	OP_ADD_FLOAT,		// pop 2 elements, add, and push result
	OP_SUB_FLOAT,		// pop 2 elements, subtract, and push result
	OP_MUL_FLOAT,		// pop 2 elements, multiply, and push result
	OP_DIV_FLOAT,		// pop 2 elements, divide, and push result
	OP_NEG_FLOAT,		// negate element on top of the stack
	OP_EQ_FLOAT,
	OP_NE_FLOAT,
	OP_LT_FLOAT,
	OP_GT_FLOAT,
	OP_LE_FLOAT,
	OP_GE_FLOAT,
	OP_POP_ARG,		// pop and store in method argument
	OP_POP_LOCAL,		// pop and store in local variable
	OP_POP_FIELD,		// pop and store in class field
	OP_PRINT_INT,		// pop and print
	OP_PRINT_FLOAT,
	OP_RETURN,		
	OP_POP,
	OP_FUNCALL,		// function call
	OP_JUMP,
	OP_COND_JUMP,		// pop top of stack and jump if evaluates to true
	OP_COND_JUMP_UNLESS,	// pop top of stack and jump if evaluates to false
	OP_INT_TO_FLOAT,
	OP_FLOAT_TO_INT,
	OP_YIELD,
	OP_SPAWN,
};

class MethodCode;
class BlockCode;

union Operand {
	Operand(int int_val_) : int_val(int_val_) { }
	Operand(float float_val_) : float_val(float_val_) { }
	Operand(std::string *str_val_) : str_val(str_val_) { }
	Operand(const BlockCode *block_) : block(block_) { }
	Operand(const MethodCode *method_) : method(method_) { }

	int int_val;
	float float_val;
	const BlockCode *block;		// if OP_BLOCK
	const MethodCode *method;	// if OP_FUNCALL
	std::string *str_val;		// if OP_SPAWN
};

struct Insn {
	Insn(Opcode opcode_, Operand operand_)
	: opcode(opcode_), operand(operand_)
	{ }

	Opcode opcode;
	Operand operand;
};

typedef std::vector<Insn> InsnList;

union Data {
	Data() { }
	Data(int int_val_) : int_val(int_val_) { }
	Data(float float_val_) : float_val(float_val_) { }

	int int_val;
	float float_val;
};

typedef std::vector<Data> DataList;

class MethodCode;

class ExecuteCtx;

struct SpawnListener {
	virtual void operator()(ExecuteCtx *parent, ExecuteCtx *child) = 0;
};

class ExecuteCtx {
  public:
	ExecuteCtx(const ClassCode *clazz_, SpawnListener& on_spawn_);

	void prepare(const std::string& method_name, const DataList& args);
	void prepare(const std::string& method_name);
	bool execute();

	const Data *get_field(const std::string& name) const;

  protected:
	Data& get_local_var(int index);

	template <class T>
	struct Stack
	{
		Stack() : size(0) { }

		T pop()
		{
			if (size == 0)
				panic("stack underflow");
			const T& rv = data[--size];
			return rv;
		}

		void push(const T& value)
		{
			if (size == MAX_STACK_SIZE)
				panic("stack overflow");
			data[size++] = value;
		}

		T& top()
		{
			return data[size - 1];
		}

		enum { MAX_STACK_SIZE = 512 };

		T data[MAX_STACK_SIZE];
		int size;
	};

	struct ExecuteState {
		ExecuteState(const BlockCode *cur_block_ = 0,
		  Data *cur_method_args_ = 0,
		  Data *cur_locals_ = 0);
		const BlockCode *cur_block;
		Data *cur_method_args;
		Data *cur_locals;
		unsigned cur_insn;
	};

	const ClassCode *clazz;
	SpawnListener& on_spawn;
	Stack<Data> data_stack;
	Stack<ExecuteState> call_stack;
	ExecuteState cur_state;
	DataList class_fields;
	bool is_executing;
};

class BlockCode {
friend class ExecuteCtx;
	typedef std::map<std::string, std::pair<int, Type> > LocalMap;

  public:
	BlockCode(const MethodCode& method, const BlockStmt& block, const BlockCode *parent);
	virtual ~BlockCode();

	void add_insn(Opcode opcode, Operand operand = Operand(0));

	const MethodCode& get_method() const;
  	bool is_method_root_block() const;
	const BlockCode *get_parent() const;

	std::pair<int, Type> get_local_index_and_type(const std::string& name) const;
	int get_num_locals() const;

	int get_cur_insn_index() const;
	void set_insn_operand(unsigned index, Operand operand);

  protected:
  	LocalMap locals;
	const MethodCode& method;
	const BlockCode *parent;
	InsnList insns;
};

class MethodCode {
	friend class FunCall;
	typedef std::vector<std::pair<std::string, Type> > ArgList;

  public:
	MethodCode(const ClassCode& clazz, const Method& method);
	virtual ~MethodCode();

	void set_root_block(BlockCode *block);
	const BlockCode *get_root_block() const;

	const ClassCode& get_class() const;
	const Type get_return_type() const;
	int get_num_args() const;

	std::pair<int, Type> get_arg_index_and_type(const std::string& name) const;

  protected:
  	const ClassCode& clazz;
	Type return_type;
	ArgList args;
	struct BlockCode *root_block;
};

class ClassCode {
	typedef std::map<std::string, MethodCode *> MethodMap;
	typedef std::map<std::string, std::pair<int, Type> > FieldMap;

  public:
	ClassCode(const ProgramCode& program, const Class& clazz);
	virtual ~ClassCode();

	const ProgramCode& get_program() const;

	ExecuteCtx *get_execute_ctx(SpawnListener& spawn_listener) const;
	const MethodCode *get_method_code(const std::string& method) const;

	std::pair<int, Type> get_field_index_and_type(const std::string& name) const;
	int get_num_fields() const;

  protected:
  	const ProgramCode& program;
	MethodMap methods;
	FieldMap fields;
};

class ProgramCode {
	typedef std::map<std::string, ClassCode *> ClassMap;

  public:
	ProgramCode(const Program& program);
	virtual ~ProgramCode();

	ExecuteCtx *get_execute_ctx(const std::string& clazz, SpawnListener& spawn_listener) const;
	const ClassCode *get_class_code(const std::string& clazz) const;

  protected:
	ClassMap classes;
};

class CompileCtx {
  public:
	CompileCtx(BlockCode& block_);

	void emit_dup_top();
	void emit_pop();
	void emit_block(const BlockStmt& block);
	void emit_return();
	void emit_print_int();
	void emit_print_float();
	void emit_push_int_const(int value);
	void emit_push_float_const(float value);
	void emit_push_arg(int index);
	void emit_push_local(int index);
	void emit_push_field(int index);
	void emit_pop_arg(int index);
	void emit_pop_local(int index);
	void emit_pop_field(int index);
	void emit_add_int();
	void emit_sub_int();
	void emit_mul_int();
	void emit_div_int();
	void emit_neg_int();
	void emit_eq_int();
	void emit_ne_int();
	void emit_lt_int();
	void emit_gt_int();
	void emit_le_int();
	void emit_ge_int();
	void emit_add_float();
	void emit_sub_float();
	void emit_mul_float();
	void emit_div_float();
	void emit_neg_float();
	void emit_eq_float();
	void emit_ne_float();
	void emit_lt_float();
	void emit_gt_float();
	void emit_le_float();
	void emit_ge_float();
	void emit_funcall(const std::string& name);
	void emit_jump(int index);
	void emit_cond_jump(int index);
	void emit_cond_jump_unless(int index);
	void emit_int_to_float();
	void emit_float_to_int();
	void emit_yield();
	void emit_spawn(const std::string& name);

	int get_cur_insn_index() const;
	void set_insn_operand(unsigned index, Operand operand);

	const BlockCode& get_block() const;
	const MethodCode& get_method() const;
	const ClassCode& get_class() const;

  protected:
	BlockCode& block_code;
};

} // bc

#endif
