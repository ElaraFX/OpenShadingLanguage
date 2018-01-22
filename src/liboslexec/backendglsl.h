
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

private:
	void reset_code();
	void begin_code(const std::string & code);
	void add_code(const std::string & code);
	void push_block();
	void pop_block();
	void gen_symbol(Symbol & sym);
	bool gen_code(const Opcode & op);
	void call_layer(int layer, bool unconditional);
	void run_connected_layers(
		Symbol & sym, int symindex,
		int opnum,
		std::set<int> *already_run);
	bool build_op(int opnum);
	bool build_block(int beginop, int endop);
	void allocate_symbol(const Symbol & sym);
	void assign_zero(const Symbol & sym);
	void assign_initial_value(const Symbol & sym);
	void gen_assign(const Symbol & a, const Symbol & b);
	bool build_instance(bool groupentry);
	void build_init();

	std::string m_code;					///< Generated code string
	std::vector<int> m_layer_remap;     ///< Remapping of layer ordering
	std::set<int> m_layers_already_run; ///< List of layers run
    int m_num_used_layers;              ///< Number of layers actually used
	int m_block_level;
};


}; // namespace pvt
OSL_NAMESPACE_EXIT
