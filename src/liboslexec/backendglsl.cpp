
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
static ustring op_add("add");
static ustring op_mul("mul");

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

void BackendGLSL::gen_symbol(Symbol & sym)
{
	add_code(" ");

	Symbol* dealiased = sym.dealias();
	std::string mangled_name = dealiased->mangled();

	if (dealiased->is_constant() && dealiased->data() != NULL)
	{
		TypeDesc t = dealiased->typespec().simpletype();
		int n = int(t.aggregate * t.numelements());
		if (t.basetype == TypeDesc::FLOAT) {
			for (int j = 0; j < n; ++j) {
				add_code(j ? " " : "");
				add_code(Strutil::format("%.9f", ((float *)dealiased->data())[j]));
			}
		} else if (t.basetype == TypeDesc::INT) {
			for (int j = 0; j < n; ++j) {
				add_code(j ? " " : "");
				add_code(Strutil::format("%d", ((int *)dealiased->data())[j]));
			}
		} else if (t.basetype == TypeDesc::STRING) {
			for (int j = 0; j < n; ++j) {
				add_code(j ? " " : "");
				add_code("\"");
				add_code(Strutil::escape_chars(((ustring *)dealiased->data())[j].string()));
				add_code("\"");
			}
		}
	}
	else
	{
		add_code(mangled_name);
	}
}

bool BackendGLSL::gen_code(const Opcode & op)
{
	begin_code(op.opname().string());
	for (int i = 0; i < op.nargs(); ++i)
	{
		Symbol & sym = *opargsym(op, i);
		gen_symbol(sym);
	}
	add_code("\n");
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

    std::string name = Strutil::format ("%s_%d", parent->layername().c_str(),
                                        parent->id());

	if (!unconditional)
	{
		begin_code("if !run[");
		add_code(Strutil::format("%d", layerfield));
		add_code("]\n");

		push_block();

		begin_code("call_layer ");
		add_code(name);
		add_code("\n");
		
		pop_block();
	}
	else
	{
		begin_code("call_layer ");
		add_code(name);
		add_code("\n");
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

    for (int c = 0;  c < inst()->nconnections();  ++c) {
        const Connection &con (inst()->connection (c));
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
		Symbol& cond = *(inst()->argsymbol(op.firstarg()));

		gen_code(op);

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
		gen_code(op);

		push_block();

		build_block (opnum + 1, op.jump(0));

		pop_block();

		return true;
	}
	else if (op.opname() == op_dowhile || op.opname() == op_for || op.opname() == op_while)
	{
		// Branch on the condition, to our blocks
		Symbol& cond = *(inst()->argsymbol(op.firstarg()));

		// Initialization (will be empty except for "for" loops)
		build_block (opnum + 1, op.jump(0));

		// For "do-while", we go straight to the body of the loop, but for
		// "for" or "while", we test the condition next.
		//rop.ll.op_branch (op.opname() == op_dowhile ? body_block : cond_block);

		// Load the condition variable and figure out if it's nonzero
		build_block (op.jump(0), op.jump(1)/*, cond_block*/);
		//llvm::Value* cond_val = rop.llvm_test_nonzero (cond);

		// Jump to either LoopBody or AfterLoop
		//rop.ll.op_branch (cond_val, body_block, after_block);

		// Body of loop
		build_block (op.jump(1), op.jump(2)/*, body_block*/);

		// Step
		build_block (op.jump(2), op.jump(3)/*, step_block*/);

		return true;
	}
	else if (op.opname() == op_break || op.opname() == op_continue)
	{
		if (op.opname() == op_break) {
			//rop.ll.op_branch (rop.ll.loop_after_block());
		} else {  // continue
			//rop.ll.op_branch (rop.ll.loop_step_block());
		}

		return true;
	}
	else if (op.opname() == op_return || op.opname() == op_exit)
	{
		if (op.opname() == op_exit) {
			// If it's a real "exit", totally jump out of the shader instance.
			// The exit instance block will be created if it doesn't yet exist.
			//rop.ll.op_branch (rop.llvm_exit_instance_block());
		} else {
			// If it's a "return", jump to the exit point of the function.
			//rop.ll.op_branch (rop.ll.return_block());
		}

		return true;
	}
	else if (op.opname() == op_useparam)
	{
		// If we have multiple params needed on this statement, don't waste
		// time checking the same upstream layer more than once.
		std::set<int> already_run;

		for (int i = 0;  i < op.nargs();  ++i) {
			Symbol& sym = *opargsym (op, i);
			int symindex = inst()->arg (op.firstarg()+i);
			run_connected_layers (sym, symindex, opnum, &already_run);
		}

		return true;
	}
	else if (op.opname() == op_assign)
	{
		Symbol & result = *opargsym(op, 0);
		Symbol & src = *opargsym(op, 1);

		begin_code("");
		gen_symbol(result);
		add_code(" = ");
		gen_symbol(src);
		add_code("\n");

		return true;
	}
	else if (op.opname() == op_add)
	{
		Symbol& result = *opargsym (op, 0);
		Symbol& a = *opargsym (op, 1);
		Symbol& b = *opargsym (op, 2);

		begin_code("");
		gen_symbol(result);
		add_code(" = ");
		gen_symbol(a);
		add_code(" + ");
		gen_symbol(b);
		add_code("\n");

		return true;
	}
	else if (op.opname() == op_mul)
	{
		Symbol& result = *opargsym (op, 0);
		Symbol& a = *opargsym (op, 1);
		Symbol& b = *opargsym (op, 2);

		begin_code("");
		gen_symbol(result);
		add_code(" = ");
		gen_symbol(a);
		add_code(" * ");
		gen_symbol(b);
		add_code("\n");

		return true;
	}
	else
	{
		return gen_code(op);
	}
}

bool BackendGLSL::build_block(int beginop, int endop)
{
    for (int opnum = beginop;  opnum < endop;  ++opnum) {
        const Opcode& op = inst()->ops()[opnum];
        const OpDescriptor *opd = shadingsys().op_descriptor (op.opname());
        if (opd) {
			if (!build_op(opnum)) {
				return false;
			}
        } else if (op.opname() == op_nop ||
                   op.opname() == op_end) {
            // Skip this op, it does nothing...
        } else {
            shadingcontext()->error ("Unsupported op %s in layer %s\n",
                                     op.opname(), inst()->layername());
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

void BackendGLSL::allocate_symbol(const Symbol & sym)
{
	Symbol* dealiased = sym.dealias();
    std::string mangled_name = dealiased->mangled();
    
	begin_code(mangled_name);
	add_code("\n");
}

void BackendGLSL::assign_zero(const Symbol & sym)
{
    int len;
    if (sym.typespec().is_closure_based())
        len = sizeof(void *) * sym.typespec().numelements();
    else
        len = sym.derivsize();
    // N.B. derivsize() includes derivs, if there are any
    size_t align = sym.typespec().is_closure_based() ? sizeof(void*) :
                         sym.typespec().simpletype().basesize();
	// TODO: Really memset it...
	//ll.op_memset (llvm_void_ptr(sym), 0, len, (int)align);
    
	Symbol* dealiased = sym.dealias();
    std::string mangled_name = dealiased->mangled();
    
	begin_code(mangled_name);
	add_code(" = 0\n");
}

void BackendGLSL::store_value(
	const Symbol & sym, 
	int arrayindex, 
	int component)
{
	Symbol* dealiased = sym.dealias();
    std::string mangled_name = dealiased->mangled();

	begin_code(mangled_name);
	add_code(Strutil::format("[%d][%d] = 0\n", arrayindex, component));
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

    int arraylen = std::max (1, sym.typespec().arraylength());

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
        int num_components = sym.typespec().simpletype().aggregate;
        TypeSpec elemtype = sym.typespec().elementtype();
        for (int a = 0, c = 0; a < arraylen;  ++a) {
            int arrind = sym.typespec().is_array() ? a : 0;
            if (sym.typespec().is_closure_based())
                continue;
            for (int i = 0; i < num_components; ++i, ++c) {
                store_value (sym, arrind, i);
            }
        }
    }
}

void BackendGLSL::gen_assign(const Symbol & a, const Symbol & b)
{
	Symbol* da = a.dealias();
    std::string ma = da->mangled();

	Symbol* db = b.dealias();
    std::string mb = db->mangled();

	begin_code(ma);
	add_code(" = ");
	add_code(mb);
	add_code("\n");
}

bool BackendGLSL::build_instance(bool groupentry)
{
	// Make a layer function: void layer_func(ShaderGlobals*, GroupData*)
    // Note that the GroupData* is passed as a void*.
    std::string unique_layer_name = Strutil::format ("%s_%d", inst()->layername(), inst()->id());

	if (inst()->entry_layer()) {
		begin_code("entry layer ");
	} else {
		begin_code("layer ");
	}
	add_code(unique_layer_name);
	add_code("\n");
	push_block();

	// TODO: Handle "exit" with m_exit_instance_block here...

	// TODO: Handle early return of entry layer...
	//llvm::Value *layerfield = layer_run_ref(m_layer_remap[layer()]);
	int layerfield = m_layer_remap[layer()];
    if (inst()->entry_layer()) {
        // For entry layers, we need an extra check to see if it already
        // ran. If it has, do an early return. Otherwise, set the 'ran' flag
        // and then run the layer.
        //llvm::Value *executed = ll.op_eq (ll.op_load (layerfield), ll.constant_bool(true));
		begin_code("if run[");
		add_code(Strutil::format("%d", layerfield));
		add_code("]\n");

		push_block();
		add_code("return\n");
		pop_block();
    }
    // Mark this layer as executed
	begin_code("run[");
	add_code(Strutil::format("%d", layerfield));
	add_code("] = true\n");

	// Setup the symbols
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
            allocate_symbol (s);
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

	// TODO: Handle exit point here...
	//if (llvm_has_exit_instance_block())
        //ll.op_branch (m_exit_instance_block); // also sets insert point

	// Transfer all of this layer's outputs into the downstream shader's
    // inputs.
    for (int layer = this->layer()+1;  layer < group().nlayers();  ++layer) {
        ShaderInstance *child = group()[layer];
        for (int c = 0;  c < child->nconnections();  ++c) {
            const Connection &con (child->connection (c));
            if (con.srclayer == this->layer()) {
                Symbol *srcsym (inst()->symbol (con.src.param));
                Symbol *dstsym (child->symbol (con.dst.param));
                run_connected_layers (*srcsym, con.src.param, -1, NULL);
                // FIXME -- I'm not sure I understand this.  Isn't this
                // unnecessary if we wrote to the parameter ourself?
                gen_assign (*dstsym, *srcsym);
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

	begin_code("init ");
	add_code(unique_name);
	add_code("\n");

	push_block();

    // Group init clears all the "layer_run" and "userdata_initialized" flags.
    if (m_num_used_layers > 1) {
        // TODO: Clear m_num_used_layers entries...
    }
    int num_userdata = (int) group().m_userdata_names.size();
    if (num_userdata) {
        // TODO: Clear num_userdata entries...
    }

    // Group init also needs to allot space for ALL layers' params
    // that are closures (to avoid weird order of layer eval problems).
    for (int i = 0;  i < group().nlayers();  ++i) {
        ShaderInstance *gi = group()[i];
        if (gi->unused() || gi->empty_instance())
            continue;
        FOREACH_PARAM (Symbol &sym, gi) {
           if (sym.typespec().is_closure_based()) {
                int arraylen = std::max (1, sym.typespec().arraylength());
                for (int a = 0; a < arraylen;  ++a) {
                    int arrind = sym.typespec().is_array() ? a : 0;
                    store_value (sym, arrind, 0);
                }
            }
        }
    }

	pop_block();
}

void BackendGLSL::run()
{
	reset_code();

	// TODO: Include all built-in ops here...

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
	std::string init_func_name = Strutil::format ("group_%d_init", group().id());
	begin_code("call_layer ");
	add_code(init_func_name);
	add_code("\n");
    for (int layer = 0; layer < nlayers; ++layer) {
		set_inst (layer);
		if (m_layer_remap[layer] != -1) {
			std::string layer_func_name = Strutil::format ("%s_%d", inst()->layername(), inst()->id());
			if (group().is_entry_layer(layer) || // User specified entry layer
				(group().num_entry_layers() == 0 && 
				layer == (nlayers - 1))) { // or the last layer as entry
				begin_code("call_layer ");
				add_code(layer_func_name);
				add_code("\n");
			}
		}
    }
}


}; // namespace pvt
OSL_NAMESPACE_EXIT
