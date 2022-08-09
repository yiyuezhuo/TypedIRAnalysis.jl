struct TNULL
end

const NULL = TNULL() # TODO: favour NULL over abused nothing.

abstract type AbstractCodeContext end # Concrete types should provide attributes in `CodeContext` at least.

"""
The analogy of `jl_codectx_t`
"""
mutable struct CodeContext <: AbstractCodeContext
    builder::Vector{AbstractIR.IR} # TODO: use a better name instead of lame LLVM builder analogy
    _module::Module
    linfo::Core.MethodInstance
    source::Core.CodeInfo
end

function make_context(::Type{T}, src::Core.CodeInfo) where T <: AbstractCodeContext
    @assert isdefined(src, :parent) "emit_function accept only CodeInfo with parent defined"
    lam::Core.MethodInstance = src.parent
    @assert lam.def isa Method "lam.def = Module is not implemented"
    _module::Module = lam.def.module
    return T(Any[], _module, lam, src)
end

function Base.show(io::IO, ctx::AbstractCodeContext)
    l1 = length(ctx.builder)
    l2 = length(ctx.source.code)
    if l1 != l2
        @warn "length(ctx.builder) != length(ctx.source.code): $l1, $l2"
    end
    lines = String[]
    for (left, right) in zip(ctx.source.code, ctx.builder)
        push!(lines, "$left ->\n $right")
    end
    push!(lines, "----")
    if l1 > l2
        for i in (l2+1):l1
            left = ctx.builder[i]
            push!(lines, "$left ->\n XXX")
        end
    end
    if l2 > l1
        for i in (l1+1):l2
            right = ctx.source.code[i]
            push!(lines, "XXX ->\n $right")
        end
    end
    return join(lines, "\n")
end


function emit!(ctx::AbstractCodeContext, x::Any)
    @debug "x = $x"

    push!(ctx.builder, x)
    return x
end
function emit!(ctx::AbstractCodeContext, x::CGVal)
    @debug "x = $x"
    return emit!(ctx, isnothing(x.tv) ? x.constant : x.tv)
end


Base.length(ctx::AbstractCodeContext) = length(ctx.builder)
code(ctx::AbstractCodeContext) = ctx.builder

"""
Partial replication of ```
static jl_llvm_functions_t
    emit_function(
        orc::ThreadSafeModule &TSM,
        jl_method_instance_t *lam,
        jl_code_info_t *src,
        jl_value_t *jlrettype,
        jl_codegen_params_t &params)
```
"""
function emit_function(src::Core.CodeInfo)
    #=
    @assert isdefined(src, :parent) "emit_function accept only CodeInfo with parent defined"
    lam::Core.MethodInstance = src.parent
    @assert lam.def isa Method "lam.def = Module is not implemented"
    _module::Module = lam.def.module
    ctx = CodeContext(Py[], _module, lam, src)

    return emit_function(ctx)
    =#
    ctx = make_context(CodeContext, src)
    return emit_function(ctx)
end

function emit_function(ctx::AbstractCodeContext)
    src = ctx.source

    for (idx, stmt) in enumerate(src.code)
        @debug "$idx: $stmt"
        try
            emit_stmtpos(ctx, stmt, idx) # Since all location of node are not modified, idx is passed directly as a "cursor".
        catch e
            println("emit_function report: idx=$idx, stmt=$stmt")
            rethrow()
        end
    end
    
    @debug "ctx=$(show(ctx))"
    # @show ctx src.code
    
    @assert length(ctx) == length(src.code) "$(length(ctx)) != $(length(src.code))"

    code_py = code(ctx)

    ssavaluetypes = src.ssavaluetypes .|> statictypedump |> AbstractIR.irtuple 
    ssaflags = src.ssaflags |> AbstractIR.irlist
    linetable = src.linetable |> AbstractIR.irlist # TODO: introduce Core.LineInfoNode?
    parent = src.parent

    # py_repr = AbstractIRMethodInstance[](mi, statictypedump(mi.specTypes), pytuple(statictypedump.(mi.sparam_vals))) # mi is `jl_ref`
    mi = src.parent

    specTypes = statictypedump(mi.specTypes)
    # @show mi.sparam_vals mi.def.file mi.def.line mi.def.sig
    sparam_vals = AbstractIR.irtuple(staticvaluedump.(mi.sparam_vals))

    cfg = Core.Compiler.compute_basic_blocks(src.code)
    cfg_py = transform_cfg(cfg)
    
    return AbstractIR.CodeInfo(code_py, ssavaluetypes, ssaflags, linetable, parent, specTypes, sparam_vals, cfg_py)
end

"""
Partial replication of `static void emit_stmtpos(jl_codectx_t &ctx, jl_value_t *expr, int ssaval_result)`
"""
function emit_stmtpos(ctx::AbstractCodeContext, stmt, ssaval_result)
    # While most non-expr nodes are eliminated in codegen, those elimination are not done here though.
    # Somce elimination in `emit_function` are converted to nodes here.

    if stmt isa Core.ReturnNode
        ret = isdefined(stmt, :val) ? AbstractIR.ReturnNode(emit_expr(ctx, stmt.val)) : AbstractIR.Unreachable() # TODO: check
        emit!(ctx, ret)
        return
    elseif stmt isa Core.GotoNode
        ret = AbstractIR.GotoNode(stmt.label)
        emit!(ctx, ret)
        return
    elseif stmt isa Core.UpsilonNode # TODO: over-simplified?
        if !isdefined(stmt, :val)
            val = CGVal(nothing, AbstractIR.Undef(), nothing, nothing)
        else
            val = emit_expr(ctx, stmt.val)
        end

        ret = AbstractIR.UpsilonNode(val)
        emit!(ctx, ret)
        return
    elseif stmt isa Core.GotoIfNot
        ret = AbstractIR.GotoIfNot(emit_expr(ctx, stmt.cond), stmt.dest) # TODO: is `emit_expr` over-simplified?
        emit!(ctx, ret)
        return
    end

    if !(stmt isa Expr)
        ret = emit_ssaval_assign(ctx, ssaval_result, stmt)
        emit!(ctx, ret)
        return
    end
    # @assert stmt isa Expr "Expect Expr but got $(stmt)::$(typeof(stmt))"
    ex::Expr = stmt
    args = ex.args
    head = ex.head
    if head ∈ (:meta, :inbounds, :code_coverage_effect, :aliasscope, :popaliasscope, :inline, :noinline)
        # (codegen.cpp) some expression types are metadata and can be ignored
        # in statement position
        emit!(ctx, AbstractIR.Ignored())
    elseif head == :enter # The expr with :enter is "comsumed" in `emit_function` in C++. However, this simplified implementation just labels it here.
        emit!(ctx, AbstractIR.Enter(args[1]))
    elseif head == :leave
        emit!(ctx, AbstractIR.Leave(args[1]))
    elseif head == :pop_exception
        excstack_state = emit_expr(ctx, args[1])
        emit!(ctx, AbstractIR.PopException(excstack_state))
    else
        ret = emit_ssaval_assign(ctx, ssaval_result, ex)
        emit!(ctx, ret)
    end
end

"""
Partial replication of static void emit_ssaval_assign(jl_codectx_t &ctx, ssize_t ssaidx_0based, jl_value_t *r)
"""
function emit_ssaval_assign(ctx::AbstractCodeContext, idx, r)
    if r isa Core.PhiNode
        return emit_phinode_assign(ctx, r)
    end
    if r isa Core.PhiCNode
        values = map(arg -> emit_expr(ctx, arg), r.values)
        ret = AbstractIR.PhiCNode(AbstractIR.irlist(values))
        return CGVal(nothing, ret, nothing, nothing)
    else
        cg_val = emit_expr(ctx, r, idx)
        return cg_val # Some "values" can appear in statement and value position, and represent different things.
    end
end

"""
Partial replication of `static void emit_phinode_assign(jl_codectx_t &ctx, ssize_t idx, jl_value_t *r)`
"""
function emit_phinode_assign(ctx::AbstractCodeContext, r::Core.PhiNode)
    edges = r.edges
    values = map(eachindex(r.values)) do idx
        if isassigned(r.values, idx)
            return emit_expr(ctx, r.values[idx])
        end
        return CGVal(nothing, AbstractIR.Undef(), nothing, nothing)
    end
    ret = AbstractIR.PhiNode(AbstractIR.irlist(edges), AbstractIR.irlist(values))
    return CGVal(nothing, ret, nothing, nothing)
end

"""
Partial replication of `static jl_cgval_t emit_expr(jl_codectx_t &ctx, jl_value_t *expr, ssize_t ssaidx_0based)`
in codegen.cpp
"""
function emit_expr(ctx::AbstractCodeContext, expr, ssaval=-1)
    if expr isa Symbol
        ret = AbstractIR.Symbol(string(expr))
        return CGVal(nothing, ret, nothing, nothing) # emit_global
    end
    if expr isa Union{Core.Slot, Core.SlotNumber}
        error("Not supported")
    end
    if expr isa Core.Argument
        ret = AbstractIR.Argument(expr.n)
        return CGVal(nothing, ret, nothing, nothing)
    end
    if expr isa Core.SSAValue
        # TODO: 0 based index or 1 based index, this is a question.
        ret = AbstractIR.SSAValue(expr.id)
        return CGVal(nothing, ret, nothing, nothing)
    end
    if expr isa GlobalRef
        return emit_globalref(ctx, expr.mod, expr.name)
    end
    if expr isa Union{Core.LineInfoNode, Core.LineNumberNode, Core.GotoIfNot, Core.GotoNode}
        error("$expr in value position")
    end
    if expr isa Core.PiNode
        # return convert_julia_type(ctx, emit_expr(ctx, jl_fieldref_noalloc(expr, 0)), jl_fieldref_noalloc(expr, 1), &skip);
        # @show expr expr.val expr.typ
        val = emit_expr(ctx, expr.val)
        typ = mark_julia_const(ctx, expr.typ)
        ret = AbstractIR.PiNode(val, typ)
        return CGVal(nothing, ret, nothing, nothing)
    end
    
    if !(expr isa Expr)
        """
        if (jl_is_quotenode(expr)) {
            expr = jl_fieldref_noalloc(expr,0);
        }
        """
        if expr isa QuoteNode
            expr = expr.value
        end

        return mark_julia_const(ctx, expr)
    end

    ex::Expr = expr
    args = ex.args
    nargs = length(args)
    head = ex.head

    if head == :isdefined
        @assert nargs == 1
        error("Not implemented") # return emit_isdefined(ctx, args[0]);
    elseif head == :throw_undef_if_not
        @assert nargs == 2
        var = CGVal(nothing, AbstractIR.Symbol(string(args[1])), nothing, nothing)
        cond = emit_expr(ctx, args[2])
        #= #  C++ source generate some handlers here.
        if !cond 
            if var == Symbol("##getfield##")
                error("$var: the field is not defined ")
            else
                error("$var: the value is not defined ")
            end
        end
        =#
        ret = AbstractIR.ThrowUndefIfNot(var, cond)
        return CGVal(nothing, ret, nothing, nothing)
    elseif head == :invoke
        expr_t = ctx.source.ssavaluetypes[ssaval]
        return emit_invoke(ctx, ex, expr_t)
    elseif head == :invoke_modify
        error("Not supported")
    elseif head == :call
        expr_t = ctx.source.ssavaluetypes[ssaval]
        return emit_call(ctx, ex, expr_t)
    elseif head == :foreigncall
        return emit_ccall(ctx, args) # emit_ccall(ctx, args, jl_array_dim0(ex->args));
    elseif head == :cfunction
        @assert nargs == 5
        error("Not implemented") # return emit_cfunction(ctx, args[0], fexpr_rt, args[2], (jl_svec_t*)args[3]);
    elseif head == Symbol("=")
        @assert nargs == 2
        #=
        emit_assignment(ctx, args[0], args[1], ssaidx_0based);
        return ghostValue(ctx, jl_nothing_type);
        =#
        error("Not implemented") # not supported?
    elseif head == :static_parameter
        @assert nargs == 1
        # emit_sparam(ctx, jl_unbox_long(args[0]) - 1);
        ret = AbstractIR.StaticParameter(args[1])
        return CGVal(nothing, ret, nothing, nothing)
    elseif head == :method
        error("Not supported")
    elseif head == :const
        error("Not supported")
    elseif head == :new
        argv = map(arg -> emit_expr(ctx, arg), args)
        ty = argv[1].typ
        @assert ty isa Type && ty.parameters[1] isa DataType "Dynamic creation is not implemented/supported."  # TODO: concrete type assertion is ignored here
        ret = AbstractIR.New(argv[1], argv[2:end]) # TODO: over-simplified?
        # TODO: Since emit_expr is expected to eliminate the outer QuoteNode which wrapped the Expr, in Python end, the QuoteNode type annotation situation can be removed.
        return CGVal(nothing, ret, nothing, nothing)
    elseif head == :jl_splatnew
        error("Not supported")
    elseif head == :new_opaque_closure
        error("Not supported")
    elseif head == :the_exception # jl_exc_sym
        @assert nargs == 0
        ret = AbstractIR.TheException()
        return CGVal(nothing, ret, nothing, nothing)
    elseif head == :copyast
        @assert nargs == 1
        ast = emit_expr(ctx, args[1])
        if ast.typ != Expr && ast.typ != Any
            return ast
        end
        ret = AbstractIR.Copyast(ast) # TODO: over-simplified?
        return CGVal(nothing, ret, nothing, nothing)
    elseif head == :loopinfo
        ret = AbstractIR.Ignored()
        return CGVal(nothing, ret, nothing, nothing)
    elseif head ∈ (:leave, :coverageeffect, :pop_exception, :enter, :inbounds, :aliasscope, :popaliasscope, :inline)
        error("Expr($head) in value position")
    elseif head == :boundscheck
        ret = AbstractIR.Boundscheck()
        return CGVal(nothing, ret, nothing, nothing)
    elseif head == :gc_preserve_begin
        ret = AbstractIR.GcPreserveBegin(args)
        return CGVal(nothing, ret, nothing, nothing) # TODO: complicated & check
    elseif head == :gc_preserve_end
        ret = AbstractIR.GcPreserveEnd()
        return CGVal(nothing, ret, nothing, nothing)
    else
        error("Not implemented for head=$head")
    end

    error("Not implemented")
end

#=
function transform_methodinstance_with_cache(mi::Core.MethodInstance)
    if haskey(G_MethodInstances, mi)
        return G_MethodInstances[mi]
    end
    py_repr = AbstractIR.MethodInstance(mi) # mi is `jl_ref`:: Any
    G_MethodInstances[mi] = py_repr
    return py_repr
end
=#

"""
'Inspired' by `static inline jl_cgval_t mark_julia_const(jl_codectx_t &ctx, jl_value_t *jv)`
"""
function mark_julia_const(ctx::AbstractCodeContext, jv; transform=true)
    # return transform_value(jv)
    typ = jv isa Type ? Type{jv} : typeof(jv)
    try
        #=
        if typ == Core.MethodInstance # TODO: ugly hack, it may be better to handle it in `transform_value`.
            tv = transform_methodinstance_with_cache(jv)
        else
            # @show jv
            tv = transform ? transform_value(jv) : nothing
        end
        =#
        tv = transform ? transform_value(jv) : nothing
        constant = CGVal(nothing, jv, typ, tv)

        return constant    
    catch e
        @debug "e=$e jv=$jv"
        rethrow()
    end
end


# function ghostValue() end # TODO: implement ghostValue

# function mark_julia_type(ctx::CodeContext, v)

"""
static jl_cgval_t emit_invoke(jl_codectx_t &ctx, jl_expr_t *ex, jl_value_t *rt)
"""
function emit_invoke(ctx, ex, expr_t)
    mi = emit_expr(ctx, ex.args[1])
    @assert mi.typ == Core.MethodInstance "Any is not supported"
    args = map(arg -> emit_expr(ctx, arg), ex.args[3:end]) |> AbstractIR.irlist
    ret = AbstractIR.Invoke(mi, args)
    return CGVal(nothing, ret, nothing, nothing)
end

"""
Partial replication of `static jl_cgval_t emit_call(jl_codectx_t &ctx, jl_expr_t *ex, jl_value_t *rt, bool is_promotable)`
"""
function emit_call(ctx, ex, expr_t)
    args = ex.args
    nargs = length(args)
    @assert nargs >= 1
    f = emit_expr(ctx, args[1])

    if isdefined(f, :constant) && f.constant isa Core.IntrinsicFunction
        fi = f.constant
        return emit_intrinsic(ctx, fi, args[2:end])
    end

    argv = CGVal[f]
    for arg in args[2:end]
        push!(argv, emit_expr(ctx, arg))
    end

    if isdefined(f, :constant) && f.constant isa Core.Builtin
        if f.constant == Core.ifelse && nargs == 4 # TODO: check
            ret = AbstractIR.Ifelse(argv[2], argv[3], argv[4])
            return CGVal(nothing, ret, nothing, nothing)
        end
        ret = AbstractIR.BuiltinCall(f.constant, argv[2:end])
        return CGVal(nothing, ret, nothing, nothing)
    end

    ret = AbstractIR.Call(f.constant, argv[2:end])
    return CGVal(nothing, ret, nothing, nothing)
    # ex |> dump
    # error("Not implemented for dynamic call: $ex")
end

function emit_intrinsic(ctx::AbstractCodeContext, f, fargs)
    # TODO: over-simplified?
    argv = map(arg -> emit_expr(ctx, arg), fargs)
    ret = AbstractIR.IntrinsicCall(f, AbstractIR.irlist(argv))
    return CGVal(nothing, ret, nothing, nothing)
end

"""
A very limited subset of functionalities of `static jl_value_t *static_eval(jl_codectx_t &ctx, jl_value_t *ex)` are implemented.
"""
function static_eval(ctx, ex)
    if ex isa Symbol
        sym::Symbol = ex
        if isconst(ctx._module, sym)
            return getfield(ctx._module, sym) # TODO: Use true correspondence of jl_get_global
        end
        return NULL
    end
    if ex isa Core.Slot || ex isa Core.SlotNumber || ex isa Core.Argument
        return NULL
    end
    if ex isa Core.SSAValue
        error("Not implemented")
    end
    if ex isa QuoteNode # Most Common
        return ex.value
    end
    if ex isa Core.MethodInstance
        return NULL
    end
    if ex isa GlobalRef
        s = ex.name
        b = getfield(ex.mod, s) # TODO: over-simplified?
        return b
        # error("Not implemented")
    end
    if ex isa Expr
        e::Expr = ex
        if e.head == :call
            f = static_eval(ctx, e.args[1])
            if f != NULL
                if length(e.args) == 3 && f == getfield
                    m = static_eval(ctx, e.args[2])
                    if !m || !(m isa Module)
                        return NULL
                    end
                    s = static_eval(ctx, e.args[3])
                    if s && s isa Symbol
                        b = getfield(m, s) # TODO: get_binding
                        error("Not implemented")
                    end
                elseif f == tuple || f == Core.apply_type
                    n = length(e.args) - 1
                    n == 0 && f == tuple && return tuple()
                    v = Any[f]
                    for i in 1:n
                        vi = static_eval(ctx, e.args[i+1])
                        push!(v, vi)
                        vi == NULL && return NULL
                    end
                    try
                        result = f(v[2:end]...)
                        return result
                    catch
                        result = NULL
                        return result
                    end
                end
            end
        elseif e.head == :static_parameter
            error("Not implemented")
        end
    end
    return NULL
end

"""
Partial replication of `static void interpret_symbol_arg(jl_codectx_t &ctx, native_sym_arg_t &out, jl_value_t *arg, const char *fname, bool llvmcall)`
return (f_name, f_lib, pointer_or_fallback) 
"""
function interpret_symbol_arg(ctx, arg)
    ptr = static_eval(ctx, arg)
    # @show ptr
    if ptr == NULL
        if arg isa Expr && arg.head == :call && length(arg.args) == 3 &&
                arg.args[1] isa GlobalRef && arg.args[1].mod == Core &&
                arg.args[1].name == :tuple
            name_val = static_eval(ctx, arg.args[2])
            if name_val != NULL && name_val isa Symbol
                f_name = string(name_val)
                lib_expr = arg.args[3]
                return f_name, lib_expr, NULL # TODO: check
            elseif name_val && name_val isa String
                return name_val, arg.args[3], NULL # TODO: check
            end
        end
        arg1 = emit_expr(ctx, arg)
        # @show arg1
        if !isnothing(arg1.typ) && arg1.typ <: Union{String, Symbol} # TODO: weird hack
            return string(arg1.constant), nothing, nothing
        end
        return NULL, NULL, arg1 
    else
        if ptr isa Tuple && length(ptr) == 1
            ptr = ptr[1]
        end

        if ptr isa Union{Symbol, String}
            return string(ptr), nothing, NULL # nothing denotes the result of jl_dlfind_win32
        elseif ptr isa Tuple && length(ptr) > 1
            return string(ptr[1]), string(ptr[2]), NULL
        else
            return NULL, NULL, ptr # the elseif order is swapped to represent fallback.
        end
    end
end

"""
Partial replication of `static jl_cgval_t emit_ccall(jl_codectx_t &ctx, jl_value_t **args, size_t nargs)`
"""
function emit_ccall(ctx::AbstractCodeContext, args)
    @assert length(args) >= 5
    rt = args[2] # C++ use `args -= 1` so index are the same in the following code
    at = args[3]
    nccallargs = length(at)
    nreqargs = args[4]
    @assert args[5] isa QuoteNode
    jlcc = args[5].value
    if jlcc isa Symbol
        cc_sym = jlcc
    elseif jlcc isa Tuple
        cc_sym = jlcc[1]
    end
    @assert cc_sym isa Symbol

    f_name, f_lib, fptr = interpret_symbol_arg(ctx, args[1])

    if f_name != NULL
        func = AbstractIR.CGlobal(f_name, f_lib)
    else
        func = fptr
    end

    @assert !(f_name == NULL && f_lib == NULL && fptr == NULL) # not the same
    
    #=
    // emit arguments
    jl_cgval_t *argv = (jl_cgval_t*)alloca(sizeof(jl_cgval_t) * nccallargs);
    for (size_t i = 0; i < nccallargs; i++) {
        // Julia (expression) value of current parameter
        jl_value_t *argi = ccallarg(i);
        argv[i] = emit_expr(ctx, argi);
    }
    =#

    # calling_convention_args = args[6:(5+nccallargs)]

    argv = map(a -> emit_expr(ctx, a), args[6:(5+nccallargs)])

    if length(args) > 5 + nccallargs
        # gc_roots = args[6+nccallargs:end]
        gc_roots = map(a -> emit_expr(ctx, a), args[6+nccallargs:end]) |> AbstractIR.irlist
    else
        gc_roots = nothing
    end

    ret_type_py = statictypedump(rt)
    argstypes_py = AbstractIR.irtuple(statictypedump.(at))

    num_varargs_py = nreqargs

    calling_convention_py = AbstractIR.Symbol(string(cc_sym))

    calling_convention_args_py = AbstractIR.irlist(argv) # TODO: check
    gc_roots_py = isnothing(gc_roots) ? nothing : AbstractIR.irlist(gc_roots) # TODO: check

    if cc_sym == :ccall
        ret = AbstractIR.CCall(func, ret_type_py, argstypes_py, num_varargs_py, calling_convention_args_py, gc_roots_py)
    else
        ret = AbstractIR.Foreigncall(func, ret_type_py, argstypes_py, num_varargs_py, calling_convention_py, calling_convention_args_py, gc_roots_py)
    end

    return CGVal(nothing, ret, nothing, nothing)
end

function emit_globalref(ctx, mod, name)
    if isconst(mod, name)
        val = getfield(mod, name)
        return mark_julia_const(ctx, val)
    end
    ret = AbstractIR.GlobalRef(mod, name)
    return CGVal(nothing, ret, nothing, nothing)
end

#=
function emit_globalref(ctx, mod, name)
    # TODO: over-simplified?
    pymod = transform_value(mod)
    pyname = Py(name)
    if isconst(mod, name)
        val = getfield(mod, name)
        
        let zero_size_binding = (val isa Core.Builtin || Base.aligned_sizeof(typeof(val)) == 0)
            if !zero_size_binding && !PythonCall.pytruth(pymod.is_bound(pyname))
                pyval = transform_value(val)
                pymod.bind_(pyname, true, pyval)

                return mark_julia_const(ctx, val)
            end
        end
        
        try
            return mark_julia_const(ctx, val)
        catch
            return mark_julia_const(ctx, val; transform=false) # TODO: very bad hack though
        end
    end
    ret = AbstractIR.GlobalRef(pymod, pyname) # The non-reduced form represents known type or unknown type dynamic reference
    return CGVal(nothing, ret, nothing, nothing) # TODO: implement and use  mark_julia_type and emit_checked_var
end
=#
