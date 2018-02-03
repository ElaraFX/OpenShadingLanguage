
#include <OpenImageIO/filesystem.h>
#include <OpenImageIO/strutil.h>

#include "oslexec_pvt.h"
#include "backendglsl.h"

using namespace OSL;
using namespace OSL::pvt;

OSL_NAMESPACE_ENTER

namespace pvt {

static ustring op_end("end");
static ustring op_nop("nop");
static ustring op_if("if");
static ustring op_functioncall("functioncall");
static ustring op_dowhile("dowhile");
static ustring op_for("for");
static ustring op_while("while");
static ustring op_break("break");
static ustring op_continue("continue");
static ustring op_return("return");
static ustring op_exit("exit");
static ustring op_useparam("useparam");
static ustring op_assign("assign");
static ustring op_neg("neg");
static ustring op_add("add");
static ustring op_sub("sub");
static ustring op_mul("mul");
static ustring op_div("div");
static ustring op_modulus("modulus");
static ustring op_bitand("bitand");
static ustring op_bitor("bitor");
static ustring op_xor("xor");
static ustring op_shl("shl");
static ustring op_shr("shr");
static ustring op_clamp("clamp");
static ustring op_mix("mix");
static ustring op_compref("compref");
static ustring op_compassign("compassign");
static ustring op_raytype("raytype");
static ustring op_eq("eq");
static ustring op_neq("neq");
static ustring op_lt("lt");
static ustring op_le("le");
static ustring op_gt("gt");
static ustring op_ge("ge");
static ustring op_floor("floor");
static ustring op_ceil("ceil");
static ustring op_trunc("trunc");
static ustring op_round("round");
static ustring op_Dx("Dx");
static ustring op_Dy("Dy");
static ustring op_min("min");
static ustring op_max("max");
static ustring op_pow("pow");
static ustring op_closure("closure");
static ustring op_texture("texture");
static ustring op_getattribute("getattribute");
static ustring u_alpha("alpha");
static ustring u_width("width");

std::string format_var(const std::string & name)
{
	std::string var = name;
	for (size_t i = 0; i < var.length(); ++i) {
		if (var[i] == ' ' || 
			var[i] == '-' || 
			var[i] == '#' || 
			var[i] == '$') {
			var[i] = '_';
		}
	}
	return var;
}

std::string format_float(const std::string & val)
{
	std::vector<char> buf(val.length() + 1);
	char *str = &buf[0];
	memcpy(str, val.data(), buf.size());

	int len = 0;
	int point = -1;

	for (int i = 0; ; ++i)
	{
		if (str[i] == '\0')
		{
			len = i;
			break;
		}

		if (str[i] == '.')
		{
			point = i;
		}
	}

	if (point != -1)
	{
		for (int i = len - 1; i > point + 1; --i)
		{
			if (str[i] == '0')
			{
				str[i] = '\0';
			}
			else
			{
				break;
			}
		}
	}

	return std::string(str);
}

BackendGLSL::BackendGLSL(
	ShadingSystemImpl & shadingsys, 
    ShaderGroup & group, 
	ShadingContext *ctx)
    : OSOProcessorBase(shadingsys, group, ctx)
{
}

BackendGLSL::~BackendGLSL()
{
}

void BackendGLSL::reset_code()
{
	m_code.clear();
	m_block_level = 0;
	m_function_id = 0;
	m_function_stack.clear();
}

void BackendGLSL::begin_code(const std::string & code)
{
	for (int i = 0; i < m_block_level; ++i)
	{
		m_code += "    ";
	}

	add_code(code);
}

void BackendGLSL::add_code(const std::string & code)
{
	m_code += code;
}

void BackendGLSL::push_block()
{
	begin_code("{\n");

	++ m_block_level;
}

void BackendGLSL::pop_block()
{
	-- m_block_level;

	begin_code("}\n");
}

void BackendGLSL::push_function(Symbol & function_name)
{
	begin_code(Strutil::format("// FUNCTIONCALL_%d ", m_function_id));
	gen_symbol(function_name);
	add_code("\n");

	m_function_stack.push_back(m_function_id);
	++ m_function_id;

	push_block();
}

void BackendGLSL::pop_function()
{
	pop_block();

	int last_function_id = m_function_stack.back();
	m_function_stack.pop_back();

	begin_code(Strutil::format("FUNCTIONCALL_%d_AFTER_BLOCK:\n", last_function_id));
}

void BackendGLSL::gen_typespec(const TypeSpec & typespec, const std::string & name)
{
    if (typespec.is_closure() || typespec.is_closure_array()) {
        begin_code("closure_color ");
		add_code(format_var(name));
        if (typespec.is_unsized_array()) {
            add_code("[]");
		} else if (typespec.arraylength() > 0) {
            add_code(Strutil::format("[%d]", typespec.arraylength()));
		}
		add_code(";\n");
    } else if (typespec.structure() > 0) {
		begin_code(typespec.structspec()->mangled());
		add_code(" ");
		add_code(format_var(name));
        if (typespec.is_unsized_array()) {
            add_code("[]");
		} else if (typespec.arraylength() > 0) {
            add_code(Strutil::format("[%d]", typespec.arraylength()));
		}
		add_code(";\n");
    } else {
		begin_code(typespec.simpletype().c_str());
		add_code(" ");
		add_code(format_var(name));
        add_code(";\n");
    }
}

void BackendGLSL::gen_data(const Symbol *dealiased)
{
	TypeDesc t = dealiased->typespec().simpletype();
	if (t.is_array()) {
		add_code("{");
	}
	for (int a = 0; a < t.numelements(); ++a) {
		if (t.is_array()) {
			add_code((a != 0) ? ", " : "");
		}
		if (t.aggregate != 1) {
			add_code(t.c_str());
			add_code("(");
		}
		if (t.basetype == TypeDesc::FLOAT) {
			for (int j = 0; j < t.aggregate; ++j) {
				add_code((j != 0) ? ", " : "");
				add_code(format_float(Strutil::format("(float)%.9f", ((float *)dealiased->data())[j])));
			}
		} else if (t.basetype == TypeDesc::INT) {
			for (int j = 0; j < t.aggregate; ++j) {
				add_code((j != 0) ? ", " : "");
				add_code(Strutil::format("%d", ((int *)dealiased->data())[j]));
			}
		} else if (t.basetype == TypeDesc::STRING) {
			for (int j = 0; j < t.aggregate; ++j) {
				add_code((j != 0) ? ", " : "");
				add_code("\"");
				add_code(Strutil::escape_chars(((ustring *)dealiased->data())[j].string()));
				add_code("\"");
			}
		}
		if (t.aggregate != 1) {
			add_code(")");
		}
	}
	if (t.is_array()) {
		add_code("}");
	}
}

void BackendGLSL::gen_symbol(Symbol & sym)
{
	Symbol* dealiased = sym.dealias();
	std::string mangled_name = dealiased->mangled();

	if (sym.symtype() == SymTypeParam || 
		sym.symtype() == SymTypeOutputParam)
	{
		std::string unique_layer_name = Strutil::format("%s_%d", inst()->shadername().c_str(), inst()->id());

		mangled_name = "groupdata." + unique_layer_name + "__" + mangled_name;
	}
	else if (sym.symtype() == SymTypeGlobal)
	{
		mangled_name = "sg." + mangled_name;
	}

	if (dealiased->is_constant() && dealiased->data() != NULL)
	{
		gen_data(dealiased);
	}
	else
	{
		add_code(format_var(mangled_name));
	}
}

bool BackendGLSL::gen_code(const Opcode & op)
{
	begin_code(op.opname().string());
	add_code(" (sg, ");
	for (int i = 0; i < op.nargs(); ++i)
	{
		add_code((i != 0) ? ", " : "");

		Symbol & sym = *opargsym(op, i);
		gen_symbol(sym);
	}
	add_code(")");
	return true;
}

void BackendGLSL::call_layer(int layer, bool unconditional)
{
    // Make code that looks like:
    //     if (! groupdata->run[parentlayer])
    //         parent_layer (sg, groupdata);
    // if it's a conditional call, or
    //     parent_layer (sg, groupdata);
    // if it's run unconditionally.
    // The code in the parent layer itself will set its 'executed' flag.

    ShaderInstance *parent = group()[layer];
    //llvm::Value *layerfield = layer_run_ref(layer_remap(layer));
	int layerfield = m_layer_remap[layer];

    std::string name = Strutil::format ("%s_%d", parent->shadername().c_str(),
                                        parent->id());

	if (!unconditional)
	{
		begin_code("if (!groupdata.run[");
		add_code(Strutil::format("%d", layerfield));
		add_code("])\n");
		push_block();

		begin_code(format_var(name));
		add_code("(sg, groupdata);\n");
		
		pop_block();
	}
	else
	{
		begin_code(format_var(name));
		add_code("(sg, groupdata);\n");
	}
}

void BackendGLSL::run_connected_layers(
	Symbol & sym, int symindex,
    int opnum,
    std::set<int> *already_run)
{
    if (sym.valuesource() != Symbol::ConnectedVal)
        return;  // Nothing to do

    bool inmain = (opnum >= inst()->maincodebegin() &&
                   opnum < inst()->maincodeend());

    for (int c = 0; c < inst()->nconnections(); ++c) {
        const Connection & con(inst()->connection(c));
        // If the connection gives a value to this param
        if (con.dst.param == symindex) {
            // already_run is a set of layers run for this particular op.
            // Just so we don't stupidly do several consecutive checks on
            // whether we ran this same layer. It's JUST for this op.
            if (already_run) {
                if (already_run->count (con.srclayer))
                    continue;  // already ran that one on this op
                else
                    already_run->insert (con.srclayer);  // mark it
            }

            if (inmain) {
                // There is an instance-wide m_layers_already_run that tries
                // to remember which earlier layers have unconditionally
                // been run at any point in the execution of this layer. But
                // only honor (and modify) that when in the main code
                // section, not when in init ops, which are inherently
                // conditional.
                if (m_layers_already_run.count (con.srclayer)) {
                    continue;  // already unconditionally ran the layer
                }
                if (! m_in_conditional[opnum]) {
                    // Unconditionally running -- mark so we don't do it
                    // again. If we're inside a conditional, don't mark
                    // because it may not execute the conditional body.
                    m_layers_already_run.insert (con.srclayer);
                }
            }

            // If the earlier layer it comes from has not yet been
            // executed, do so now.
            call_layer (con.srclayer, false/* not unconditional */);
        }
    }
}

bool BackendGLSL::build_op(int opnum)
{
	Opcode & op (inst()->ops()[opnum]);

	if (op.opname() == op_if)
	{
		Symbol & cond = *opargsym(op, 0);

		begin_code("if (");
		gen_symbol(cond);
		add_code(")\n");

		push_block();

		// Then block
		build_block (opnum + 1, op.jump(0));

		pop_block();

		// Else block
		if (op.jump(0) < op.jump(1))
		{
			begin_code("else\n");
			push_block();

			build_block (op.jump(0), op.jump(1));

			pop_block();
		}

		return true;
	}
	else if (op.opname() == op_functioncall)
	{
		Symbol & function_name = *opargsym(op, 0);

		push_function(function_name);

		build_block (opnum + 1, op.jump(0));

		pop_function();

		return true;
	}
	else if (
		op.opname() == op_dowhile || 
		op.opname() == op_for || 
		op.opname() == op_while)
	{
		// Branch on the condition, to our blocks
		Symbol & cond = *opargsym(op, 0);

		// Initialization (will be empty except for "for" loops)
		build_block (opnum + 1, op.jump(0));

		// For "do-while", we go straight to the body of the loop, but for
		// "for" or "while", we test the condition next.
		if (op.opname() == op_dowhile)
		{
			begin_code("do\n");

			push_block();

			// Loop body
			build_block (op.jump(1), op.jump(2));

			// Step block
			build_block (op.jump(2), op.jump(3));

			// Condition block
			build_block (op.jump(0), op.jump(1));

			pop_block();

			// Test condition
			begin_code("while (");
			gen_symbol(cond);
			add_code(");\n");
		}
		else
		{
			// Condition block
			build_block (op.jump(0), op.jump(1));

			// Test condition
			begin_code("while (");
			gen_symbol(cond);
			add_code(")\n");

			push_block();

			// Loop body
			build_block (op.jump(1), op.jump(2));

			// Step block
			build_block (op.jump(2), op.jump(3));

			// Condition block
			build_block (op.jump(0), op.jump(1));

			pop_block();
		}

		return true;
	}
	else if (
		op.opname() == op_break || 
		op.opname() == op_continue)
	{
		if (op.opname() == op_break) {
			begin_code("break;\n");
		} else {
			begin_code("continue;\n");
		}

		return true;
	}
	else if (
		op.opname() == op_return || 
		op.opname() == op_exit)
	{
		if (op.opname() == op_exit) {
			// If it's a real "exit", totally jump out of the shader instance. 
			// Since we compile shader instance into a function, a return will 
			// just do the trick.
			begin_code("return;\n");
		} else {
			// If it's a "return", jump to the exit point of the function.
			if (m_function_stack.empty()) {
				begin_code("return;\n");
			} else {
				int current_function_id = m_function_stack.back();
				begin_code(Strutil::format("goto FUNCTIONCALL_%d_AFTER_BLOCK;\n", current_function_id));
			}
		}

		return true;
	}
	else if (op.opname() == op_end)
	{
		// Nothing to do
		return true;
	}
	else if (op.opname() == op_useparam)
	{
		// If we have multiple params needed on this statement, don't waste
		// time checking the same upstream layer more than once.
		std::set<int> already_run;

		for (int i = 0;  i < op.nargs();  ++i) {
			Symbol & sym = *opargsym (op, i);
			int symindex = inst()->arg (op.firstarg()+i);
			run_connected_layers (sym, symindex, opnum, &already_run);
		}

		return true;
	}
	else if (
		op.opname() == op_assign || 
		op.opname() == op_neg)
	{
		Symbol & result = *opargsym(op, 0);
		Symbol & src = *opargsym(op, 1);

		begin_code("");
		gen_symbol(result);
		add_code(" = ");
		if (op.opname() == op_neg) {
			add_code("- ");
		}
		gen_symbol(src);
		add_code(";\n");

		return true;
	}
	else if (
		op.opname() == op_add || 
		op.opname() == op_sub || 
		op.opname() == op_mul || 
		op.opname() == op_div)
	{
		Symbol& result = *opargsym (op, 0);
		Symbol& a = *opargsym (op, 1);
		Symbol& b = *opargsym (op, 2);

		begin_code("");
		gen_symbol(result);
		add_code(" = ");
		gen_symbol(a);

		if (op.opname() == op_add) {
			add_code(" + ");
		} else if (op.opname() == op_sub) {
			add_code(" - ");
		} else if (op.opname() == op_mul) {
			add_code(" * ");
		} else {
			add_code(" / ");
		}

		gen_symbol(b);
		add_code(";\n");

		return true;
	}
	else if (op.opname() == op_modulus)
	{
		Symbol& result = *opargsym (op, 0);
		Symbol& a = *opargsym (op, 1);
		Symbol& b = *opargsym (op, 2);

		bool is_float = result.typespec().is_floatbased();

		if (is_float) {
			begin_code("");
			gen_symbol(result);
			add_code(" = fmod(");
			gen_symbol(a);
			add_code(", ");
			gen_symbol(b);
			add_code(");\n");
		} else {
			begin_code("");
			gen_symbol(result);
			add_code(" = ");
			gen_symbol(a);
			add_code(" % ");
			gen_symbol(b);
			add_code(";\n");
		}

		return true;
	}
	else if (
		op.opname() == op_bitand || 
		op.opname() == op_bitor || 
		op.opname() == op_xor || 
		op.opname() == op_shl || 
		op.opname() == op_shr)
	{
		Symbol& result = *opargsym (op, 0);
		Symbol& a = *opargsym (op, 1);
		Symbol& b = *opargsym (op, 2);

		begin_code("");
		gen_symbol(result);
		add_code(" = ");
		gen_symbol(a);

		if (op.opname() == op_bitand) {
			add_code(" & ");
		} else if (op.opname() == op_bitor) {
			add_code(" | ");
		} else if (op.opname() == op_xor) {
			add_code(" ^ ");
		} else if (op.opname() == op_shl) {
			add_code(" << ");
		} else {
			add_code(" >> ");
		}

		gen_symbol(b);
		add_code(";\n");

		return true;
	}
	else if (op.opname() == op_clamp)
	{
		Symbol& Result = *opargsym (op, 0);
		Symbol& X = *opargsym (op, 1);
		Symbol& Min = *opargsym (op, 2);
		Symbol& Max = *opargsym (op, 3);

		begin_code("");
		gen_symbol(Result);
		add_code(" = min(max(");
		gen_symbol(X);
		add_code(", ");
		gen_symbol(Min);
		add_code("), ");
		gen_symbol(Max);
		add_code(");\n");

		return true;
	}
	else if (op.opname() == op_mix)
	{
		Symbol& Result = *opargsym (op, 0);
		Symbol& A = *opargsym (op, 1);
		Symbol& B = *opargsym (op, 2);
		Symbol& X = *opargsym (op, 3);

		begin_code("");
		gen_symbol(Result);
		add_code(" = (1.0 - ");
		gen_symbol(X);
		add_code(") * ");
		gen_symbol(A);
		add_code(" + ");
		gen_symbol(X);
		add_code(" * ");
		gen_symbol(B);
		add_code(";\n");

		return true;
	}
	else if (op.opname() == op_compref)
	{
		Symbol& Result = *opargsym (op, 0);
		Symbol& Val = *opargsym (op, 1);
		Symbol& Index = *opargsym (op, 2);

		begin_code("");
		gen_symbol(Result);
		add_code(" = ");
		gen_symbol(Val);
		add_code("[");
		gen_symbol(Index);
		add_code("];\n");

		return true;
	}
	else if (op.opname() == op_compassign)
	{
		Symbol& Result = *opargsym (op, 0);
		Symbol& Index = *opargsym (op, 1);
		Symbol& Val = *opargsym (op, 2);

		begin_code("");
		gen_symbol(Result);
		add_code("[");
		gen_symbol(Index);
		add_code("] = ");
		gen_symbol(Val);
		add_code(";\n");

		return true;
	}
	else if (op.opname() == op_raytype)
	{
		Symbol& Result = *opargsym (op, 0);
		Symbol& Name = *opargsym (op, 1);

		if (Name.is_constant()) {
			// We can statically determine the bit pattern
			ustring name = ((ustring *)Name.data())[0];
			int raytype_bit = shadingsys().raytype_bit(name);

			begin_code("");
			gen_symbol(Result);
			add_code(Strutil::format(" = raytype_bit(sg, %d);\n", raytype_bit));
		} else {
			// No way to know which name is being asked for
			begin_code("");
			gen_symbol(Result);
			add_code(" = raytype_name(sg, ");
			gen_symbol(Name);
			add_code(");\n");
		}

		return true;
	}
	else if (
		op.opname() == op_eq || 
		op.opname() == op_neq || 
		op.opname() == op_lt || 
		op.opname() == op_le || 
		op.opname() == op_gt || 
		op.opname() == op_ge)
	{
		Symbol &Result = *opargsym (op, 0);
		Symbol &A = *opargsym (op, 1);
		Symbol &B = *opargsym (op, 2);

		begin_code("");
		gen_symbol(Result);
		add_code(" = (");
		gen_symbol(A);

		if (op.opname() == op_eq) {
			add_code(" == ");
		} else if (op.opname() == op_neq) {
			add_code(" != ");
		} else if (op.opname() == op_lt) {
			add_code(" < ");
		} else if (op.opname() == op_le) {
			add_code(" <= ");
		} else if (op.opname() == op_gt) {
			add_code(" > ");
		} else {
			add_code(" >= ");
		}

		gen_symbol(B);
		add_code(");\n");

		return true;
	}
	else if (
		op.opname() == op_floor || 
		op.opname() == op_ceil || 
		op.opname() == op_trunc || 
		op.opname() == op_round)
	{
		Symbol & result = *opargsym(op, 0);
		Symbol & src = *opargsym(op, 1);

		begin_code("");
		gen_symbol(result);

		if (op.opname() == op_floor) {
			add_code(" = floor(");
		} else if (op.opname() == op_ceil) {
			add_code(" = ceil(");
		} else if (op.opname() == op_trunc) {
			add_code(" = trunc(");
		} else {
			add_code(" = round(");
		}

		gen_symbol(src);
		add_code(");\n");

		return true;
	}
	else if (
		op.opname() == op_Dx || 
		op.opname() == op_Dy)
	{
		Symbol & result = *opargsym(op, 0);
		Symbol & src = *opargsym(op, 1);

		begin_code("");
		gen_symbol(result);

		if (op.opname() == op_Dx) {
			add_code(" = Dx(sg, ");
		} else {
			add_code(" = Dy(sg, ");
		}

		gen_symbol(src);
		add_code(");\n");

		return true;
	}
	else if (
		op.opname() == op_min || 
		op.opname() == op_max || 
		op.opname() == op_pow)
	{
		Symbol& result = *opargsym (op, 0);
		Symbol& a = *opargsym (op, 1);
		Symbol& b = *opargsym (op, 2);

		begin_code("");
		gen_symbol(result);

		if (op.opname() == op_min) {
			add_code(" = min(");
		} else if (op.opname() == op_max) {
			add_code(" = max(");
		} else {
			add_code(" = pow(");
		}

		gen_symbol(a);
		add_code(", ");
		gen_symbol(b);
		add_code(");\n");

		return true;
	}
	else if (op.opname() == op_closure)
	{
		Symbol &Result = *opargsym (op, 0);
		int weighted   = opargsym(op, 1)->typespec().is_string() ? 0 : 1;
		Symbol *weight = weighted ? opargsym (op, 1) : NULL;
		Symbol &Id     = *opargsym (op, 1 + weighted);
		
		ustring closure_name = *((ustring *)Id.data());

		const ClosureRegistry::ClosureEntry * clentry = shadingsys().find_closure(closure_name);
		if (!clentry) {
			shadingcontext()->error(
				"Closure '%s' is not supported by the current renderer, called from %s:%d in shader \"%s\", layer %d \"%s\", group \"%s\"\n", 
				closure_name, op.sourcefile(), op.sourceline(), 
				inst()->shadername(), layer(), 
				inst()->layername(), group().name());
			return false;
		}

		// TODO: Currently we always assume there is no closure's 
		// "prepare" and "setup" methods.

		begin_code("");
		gen_symbol(Result);

		int closure_param_offset = 2 + weighted;
		int closure_param_count = op.nargs() - closure_param_offset;
		if (closure_param_count > 0)
		{
			if (weighted)
			{
				add_code(" = ");
				gen_symbol(*weight);
				add_code(Strutil::format(" * closure_%s(sg, ", 
					closure_name.c_str()));
			}
			else
			{
				add_code(Strutil::format(" = closure_%s(sg, ", 
					closure_name.c_str()));
			}

			for (int i = closure_param_offset; i < op.nargs(); ++i)
			{
				add_code((i != closure_param_offset) ? ", " : "");

				Symbol & sym = *opargsym(op, i);
				gen_symbol(sym);
			}
		}
		else
		{
			if (weighted)
			{
				add_code(" = ");
				gen_symbol(*weight);
				add_code(Strutil::format(" * closure_%s(sg", 
					closure_name.c_str()));
			}
			else
			{
				add_code(Strutil::format(" = closure_%s(sg", 
					closure_name.c_str()));
			}
		}

		add_code(");\n");

		return true;
	}
	else if (op.opname() == op_texture)
	{
		Symbol &Result = *opargsym (op, 0);
		Symbol &Filename = *opargsym (op, 1);
		Symbol &S = *opargsym (op, 2);
		Symbol &T = *opargsym (op, 3);
		int nchans = Result.typespec().aggregate();

		int first_optional_arg = 4;
		if (op.nargs() > 4 && opargsym(op, 4)->typespec().is_float()) {
			first_optional_arg = 8;
		}

		// TODO: Support more optional arguments including 
		// swidth, twidth, rwidth, 
		// blur, sblur, tblur, rblur, 
		// wrap, swrap, twrap, rwrap, 
		// firstchannel, fill, interp, 
		// time, subimage
		// missingcolor, missingalpha
		Symbol *alpha = NULL;
		Symbol *width = NULL;

		for (int a = first_optional_arg;  a < op.nargs(); ++a) {
			Symbol &Name (*opargsym(op, a));
			ustring name = *(ustring *)Name.data();
			++a; // advance to next argument

			if (!name) { // skip empty string param name
				continue;
			}

			Symbol &Val (*opargsym(op, a));

			if (name == u_alpha) {
				alpha = &Val;
			} else if (name == u_width) {
				width = &Val;
			}
		}

		begin_code("");
		gen_symbol(Result);
		add_code(" = texture(sg, ");
		gen_symbol(Filename);
		add_code(", ");
		gen_symbol(S);
		add_code(", ");
		gen_symbol(T);
		add_code(Strutil::format(", %d", nchans));

		add_code(", ");
		if (width != NULL) {
			gen_symbol(*width);
		} else {
			add_code("(float)1.0");
		}

		if (alpha != NULL) {
			add_code(", ");
			gen_symbol(*alpha);
		}

		add_code(");\n");

		return true;
	}
	else if (op.opname() == op_getattribute)
	{
		int nargs = op.nargs();

		bool array_lookup = opargsym(op, nargs - 2)->typespec().is_int();
		bool object_lookup = opargsym(op, 2)->typespec().is_string() && nargs >= 4;
		int object_slot = (int)object_lookup;
		int attrib_slot = object_slot + 1;
		int index_slot = array_lookup ? nargs - 2 : 0;

		Symbol& Result      = *opargsym (op, 0);
		Symbol& ObjectName  = *opargsym (op, object_slot); // only valid if object_slot is true
		Symbol& Attribute   = *opargsym (op, attrib_slot);
		Symbol& Index       = *opargsym (op, index_slot);  // only valid if array_lookup is true
		Symbol& Destination = *opargsym (op, nargs - 1);

		begin_code("");
		gen_symbol(Result);
		add_code(" = getattribute(sg, ");
		if (object_lookup) {
			gen_symbol(ObjectName);
		} else {
			add_code("");
		}
		add_code(", ");
		gen_symbol(Attribute);
		if (array_lookup) {
			add_code(", true, ");
		} else {
			add_code(", false, ");
		}
		gen_symbol(Index);
		add_code(", ");
		gen_symbol(Destination);
		add_code(");\n");

		return true;
	}
	else
	{
		// Handle the general case as a function
		gen_code(op);
		add_code(";\n");

		return true;
	}
}

bool BackendGLSL::build_block(int beginop, int endop)
{
    for (int opnum = beginop;  opnum < endop;  ++opnum) {
        const Opcode& op = inst()->ops()[opnum];
		// We don't really use the generator here, just to 
		// align with LLVM: we don't generate code when the 
		// LLVM backend does not.
        const OpDescriptor *opd = shadingsys().op_descriptor (op.opname());
        if (opd && opd->llvmgen) {
			if (!build_op(opnum)) {
				return false;
			}
        } else if (op.opname() == op_nop || 
                   op.opname() == op_end) {
            // Skip this op, it does nothing...
        } else {
            shadingcontext()->error ("Unsupported op %s in layer %s\n", 
				op.opname(), inst()->layername().c_str());
            return false;
        }

        // If the op we coded jumps around, skip past its recursive block
        // executions.
        int next = op.farthest_jump ();
        if (next >= 0)
            opnum = next-1;
    }

	return true;
}

void BackendGLSL::get_or_allocate_symbol(const Symbol & sym)
{
	DASSERT ((sym.symtype() == SymTypeLocal || sym.symtype() == SymTypeTemp || 
              sym.symtype() == SymTypeConst)
             && "get_or_allocate_symbol should only be for local, tmp, const");

    Symbol* dealiased = sym.dealias();
    std::string mangled_name = dealiased->mangled();
    std::set<std::string>::iterator iter = m_named_values.find(mangled_name);

    if (iter == m_named_values.end()) {
		gen_typespec(dealiased->typespec(), mangled_name);
        m_named_values.insert(mangled_name);
    }
}

void BackendGLSL::assign_zero(const Symbol & sym)
{
	Symbol* dealiased = sym.dealias();
    std::string mangled_name = format_var(dealiased->mangled());

	if (sym.symtype() == SymTypeGlobal)
	{
		mangled_name = "sg." + mangled_name;
	}

	if (!sym.typespec().is_array()) {
		begin_code(mangled_name);
		add_code(" = 0;\n");
	} else {
		int arraylen = sym.typespec().arraylength();
		for (int a = 0; a < arraylen; ++a) {
			begin_code(mangled_name);
			add_code(Strutil::format("[%d] = 0;\n", a));
		}
	}
}

void BackendGLSL::assign_initial_value(const Symbol & sym)
{
	// Don't write over connections!  Connection values are written into
    // our layer when the earlier layer is run, as part of its code.  So
    // we just don't need to initialize it here at all.
    if (sym.valuesource() == Symbol::ConnectedVal &&
          !sym.typespec().is_closure_based())
        return;
    if (sym.typespec().is_closure_based() && sym.symtype() == SymTypeGlobal)
        return;

    // Closures need to get their storage before anything can be
    // assigned to them.  Unless they are params, in which case we took
    // care of it in the group entry point.
    if (sym.typespec().is_closure_based() && 
        sym.symtype() != SymTypeParam && sym.symtype() != SymTypeOutputParam) {
        assign_zero (sym);
        return;
    }

    if ((sym.symtype() == SymTypeLocal || sym.symtype() == SymTypeTemp) && 
        sym.typespec().is_string_based()) {
        // Strings are pointers.  Can't take any chance on leaving
        // local/tmp syms uninitialized.
        assign_zero (sym);
        return;  // we're done, the parts below are just for params
    }

	// TODO: Currently we always assume lockgeom is true in our system...

    if (sym.has_init_ops() && sym.valuesource() == Symbol::DefaultVal) {
        // Handle init ops.
        build_block (sym.initbegin(), sym.initend());
    } else {
        // Use default value
		if (!sym.typespec().is_closure_based()) {
			Symbol* dealiased = sym.dealias();
			std::string mangled_name = dealiased->mangled();

			if (sym.symtype() == SymTypeParam || 
				sym.symtype() == SymTypeOutputParam)
			{
				std::string unique_layer_name = format_var(Strutil::format("%s_%d", inst()->shadername().c_str(), inst()->id()));

				mangled_name = "groupdata." + unique_layer_name + "__" + mangled_name;
			}
			else if (sym.symtype() == SymTypeGlobal)
			{
				mangled_name = "sg." + mangled_name;
			}

			// Fill in the constant val
			begin_code(format_var(mangled_name));
			add_code(" = ");
			gen_data(&sym);
			add_code(";\n");
		}
    }
}

bool BackendGLSL::build_instance(bool groupentry)
{
	// Make a layer function: void layer_func(ShaderGlobals*, GroupData*)
    // Note that the GroupData* is passed as a void*.
    std::string unique_layer_name = format_var(Strutil::format("%s_%d", inst()->shadername().c_str(), inst()->id()));

	if (inst()->entry_layer()) {
		begin_code("ENTRY_API void ");
	} else {
		begin_code("void ");
	}
	add_code(unique_layer_name);
	add_code("(ShaderGlobals & sg, GroupData & groupdata)\n");
	push_block();

	//llvm::Value *layerfield = layer_run_ref(m_layer_remap[layer()]);
	int layerfield = m_layer_remap[layer()];
    if (inst()->entry_layer()) {
        // For entry layers, we need an extra check to see if it already
        // ran. If it has, do an early return. Otherwise, set the 'ran' flag
        // and then run the layer.
		begin_code("if (groupdata.run[");
		add_code(Strutil::format("%d", layerfield));
		add_code("])\n");

		push_block();
		add_code("return;\n");
		pop_block();
    }
    // Mark this layer as executed
	begin_code("groupdata.run[");
	add_code(Strutil::format("%d", layerfield));
	add_code("] = true;\n");

	// Setup the symbols
	m_named_values.clear ();
	m_layers_already_run.clear ();
	BOOST_FOREACH (Symbol &s, inst()->symbols()) {
        // Skip constants -- we always inline scalar constants, and for
        // array constants we will just use the pointers to the copy of
        // the constant that belongs to the instance.
        if (s.symtype() == SymTypeConst)
            continue;
        // Skip structure placeholders
        if (s.typespec().is_structure())
            continue;
		// Allocate space for locals, temps, aggregate constants
        if (s.symtype() == SymTypeLocal || s.symtype() == SymTypeTemp || 
            s.symtype() == SymTypeConst)
            get_or_allocate_symbol (s);
        // Set initial value for constants, closures, and strings that are
        // not parameters.
        if (s.symtype() != SymTypeParam && s.symtype() != SymTypeOutputParam &&
            s.symtype() != SymTypeGlobal &&
            (s.is_constant() || s.typespec().is_closure_based() ||
             s.typespec().is_string_based()))
            assign_initial_value (s);
    }
    // make a second pass for the parameters (which may make use of
    // locals and constants from the first pass)
    FOREACH_PARAM (Symbol &s, inst()) {
        // Skip structure placeholders
        if (s.typespec().is_structure())
            continue;
        // Skip if it's never read and isn't connected
        if (! s.everread() && ! s.connected_down() && ! s.connected()
              && ! s.renderer_output())
            continue;
        // Skip if it's an interpolated (userdata) parameter and we're
        // initializing them lazily.
        if (s.symtype() == SymTypeParam
                && ! s.lockgeom() && ! s.typespec().is_closure()
                && ! s.connected() && ! s.connected_down()
                && shadingsys().lazy_userdata())
            continue;
        // Set initial value for params (may contain init ops)
        assign_initial_value (s);
    }

	// All the symbols are stack allocated now.
    if (groupentry) {
        // Group entries also need to run any earlier layers that must be
        // run unconditionally. It's important that we do this AFTER all the
        // parameter initialization for this layer.
        for (int i = 0;  i < group().nlayers()-1;  ++i) {
            ShaderInstance *gi = group()[i];
            if (!gi->unused() && !gi->empty_instance() && !gi->run_lazily())
                call_layer (i, true /* unconditionally run */);
        }
    }

    // Mark all the basic blocks, including allocating llvm::BasicBlock
    // records for each.
    find_basic_blocks ();
    find_conditionals ();

	build_block(
		inst()->maincodebegin(), 
		inst()->maincodeend());

	// Transfer all of this layer's outputs into the downstream shader's
    // inputs.
    for (int layer = this->layer()+1;  layer < group().nlayers();  ++layer) {
        ShaderInstance *child = group()[layer];
		std::string dst_layer_name = format_var(Strutil::format("%s_%d", child->shadername().c_str(), child->id()));
        for (int c = 0; c < child->nconnections(); ++c) {
            const Connection & con(child->connection(c));
            if (con.srclayer == this->layer()) {
                Symbol *srcsym (inst()->symbol (con.src.param));
                Symbol *dstsym (child->symbol (con.dst.param));
                run_connected_layers (*srcsym, con.src.param, -1, NULL);
                // FIXME -- I'm not sure I understand this.  Isn't this
                // unnecessary if we wrote to the parameter ourself?
				Symbol* dst_dealiased = dstsym->dealias();
				std::string dst_mangled = "groupdata." + dst_layer_name + "__" + dst_dealiased->mangled();

				Symbol* src_dealiased = srcsym->dealias();
				std::string src_mangled = "groupdata." + unique_layer_name + "__" + src_dealiased->mangled();

				begin_code(format_var(dst_mangled));
				add_code(" = ");
				add_code(format_var(src_mangled));
				add_code(";\n");
            }
        }
    }

	pop_block();

	return true;
}

void BackendGLSL::build_init()
{
    // Make a group init function: void group_init(ShaderGlobals*, GroupData*)
    // Note that the GroupData* is passed as a void*.
    std::string unique_name = Strutil::format ("group_%d_init", group().id());

	begin_code("void ");
	add_code(unique_name);
	add_code("(ShaderGlobals & sg, GroupData & groupdata)\n");
	push_block();

    // Group init clears all the "layer_run" and "userdata_initialized" flags.
	for (int i = 0; i < m_num_used_layers; ++i)
	{
		begin_code(Strutil::format("groupdata.run[%d] = false;\n", i));
	}

    int num_userdata = (int) group().m_userdata_names.size();
    if (num_userdata) {
        // TODO: Clear num_userdata entries...
    }

    // Group init also needs to allot space for ALL layers' params
    // that are closures (to avoid weird order of layer eval problems).
    for (int i = 0;  i < group().nlayers(); ++i) {
        ShaderInstance *gi = group()[i];
        if (gi->unused() || gi->empty_instance())
            continue;

		std::string unique_layer_name = format_var(Strutil::format("%s_%d", gi->shadername().c_str(), gi->id()));

        FOREACH_PARAM (Symbol &sym, gi) {
			if (sym.typespec().is_closure_based()) {
				Symbol* dealiased = sym.dealias();
				std::string mangled_name = format_var("groupdata." + unique_layer_name + "__" + dealiased->mangled());

				if (!sym.typespec().is_array()) {
					begin_code(mangled_name);
					add_code(" = 0;\n");
				} else {
					int arraylen = sym.typespec().arraylength();
					for (int a = 0; a < arraylen; ++a) {
						begin_code(mangled_name);
						add_code(Strutil::format("[%d] = 0;\n", a));
					}
				}
            }
        }
    }

	pop_block();
}

void BackendGLSL::type_groupdata()
{
	// TODO: Now add the array that tells which userdata have been initialized,
    // and the space for the userdata values.

	begin_code("struct GroupData\n");
	push_block();

	// For each layer in the group, add entries for all params that are
    // connected or interpolated, and output params.  Also mark those
    // symbols with their offset within the group struct.
    for (int layer = 0;  layer < group().nlayers(); ++layer) {
        ShaderInstance *inst = group()[layer];
        if (inst->unused())
            continue;

		std::string unique_layer_name = format_var(Strutil::format("%s_%d", inst->shadername().c_str(), inst->id()));

        FOREACH_PARAM (Symbol &sym, inst) {
            if (sym.typespec().is_structure())  // skip the struct symbol itself
                continue;

			Symbol *dealiased = sym.dealias();
			
			// Don't need "groupdata." prefix inside struct definition
			std::string mangled_name = unique_layer_name + "__" + dealiased->mangled();
			
			gen_typespec(dealiased->typespec(), mangled_name);
        }
    }

	begin_code(Strutil::format("bool run[%d];\n", m_num_used_layers));

	// Specialized pop_block for structure definition
	-- m_block_level;
	begin_code("};\n");
}

void BackendGLSL::run()
{
	reset_code();

	// Include all built-in ops here
	begin_code("#include <stdosl_glsl.h>\n");

	// Set up m_num_used_layers to be the number of layers that are
    // actually used, and m_layer_remap[] to map original layer numbers
    // to the shorter list of actually-called layers. We also note that
    // if m_layer_remap[i] is < 0, it's not a layer that's used.
    int nlayers = group().nlayers();
    m_layer_remap.resize (nlayers, -1);
    m_num_used_layers = 0;
    for (int layer = 0;  layer < nlayers;  ++layer) {
        // Skip unused or empty layers, unless they are callable entry
        // points.
        ShaderInstance *inst = group()[layer];
        bool is_single_entry = (layer == (nlayers-1) && group().num_entry_layers() == 0);
        if (inst->entry_layer() || is_single_entry ||
            (! inst->unused() && !inst->empty_instance())) {
            m_layer_remap[layer] = m_num_used_layers++;
        }
    }

	// Declare group data in in advance
	type_groupdata();

    // Generate the LLVM IR for each layer.  Skip unused layers.
	build_init();
    for (int layer = 0; layer < nlayers; ++layer) {
        set_inst (layer);
        if (m_layer_remap[layer] != -1) {
            // If no entry points were specified, the last layer is special,
            // it's the single entry point for the whole group.
            bool is_single_entry = (layer == (nlayers-1) && group().num_entry_layers() == 0);
            build_instance (is_single_entry);
        }
    }

	// Force the JIT to happen now and retrieve the JITed function pointers
    // for the initialization and all public entry points.
	std::string init_func_name = Strutil::format("group_%d_init", group().id());
	begin_code(init_func_name);
	add_code("(sg, groupdata);\n");
    for (int layer = 0; layer < nlayers; ++layer) {
		set_inst (layer);
		if (m_layer_remap[layer] != -1) {
			std::string layer_func_name = format_var(Strutil::format("%s_%d", inst()->shadername().c_str(), inst()->id()));
			if (group().is_entry_layer(layer) || // User specified entry layer
				(group().num_entry_layers() == 0 && 
				layer == (nlayers - 1))) { // or the last layer as entry
				begin_code(layer_func_name);
				add_code("(sg, groupdata);\n");
			}
		}
    }
}


}; // namespace pvt
OSL_NAMESPACE_EXIT
