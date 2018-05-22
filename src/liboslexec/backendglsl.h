
#pragma once

#include <vector>
#include <map>

#include "oslexec_pvt.h"
using namespace OSL;
using namespace OSL::pvt;


OSL_NAMESPACE_ENTER

namespace pvt {   // OSL::pvt


class BackendGLSL : public OSOProcessorBase {
public:
    BackendGLSL(
		ShadingSystemImpl & shadingsys, 
		ShaderGroup & group,
        ShadingContext *context);

    virtual ~BackendGLSL();

    virtual void run();

	const std::string & get_code() const { return m_code; }

	void set_OpenCL(bool val) { m_OpenCL = val; }

private:
	void reset_code();
	void begin_code(const std::string & code);
	void add_code(const std::string & code);
	void push_block();
	void pop_block();
	void push_function(Symbol & function_name);
	void pop_function();
	void gen_typespec(const TypeSpec & typespec, const std::string & name, bool newline);
	void gen_data(const Symbol *dealiased);
	void gen_symbol(Symbol & sym);
	bool gen_code(const Opcode & op);
	void call_layer(int layer, bool unconditional);
	void run_connected_layers(
		Symbol & sym, int symindex,
		int opnum,
		std::set<int> *already_run);
	bool build_op(int opnum);
	bool build_block(int beginop, int endop);
	void get_or_allocate_symbol(const Symbol & sym);
	void assign_zero(const Symbol & sym);
	void assign_initial_value(const Symbol & sym);
	void init_array_constants(const Symbol & sym);
	bool build_instance(bool groupentry);
	void build_init();
	void type_groupdata();

	std::string m_code;					///< Generated code string
	std::vector<int> m_layer_remap;     ///< Remapping of layer ordering
	std::set<std::string> m_named_values;
	std::set<int> m_layers_already_run; ///< List of layers run
    int m_num_used_layers;              ///< Number of layers actually used
	int m_block_level;

	int m_function_id;					///< Line counter for function call
	std::vector<int> m_function_stack;	///< Stack of function calls

	bool m_OpenCL;						///< Target language is OpenCL
};


}; // namespace pvt
OSL_NAMESPACE_EXIT
