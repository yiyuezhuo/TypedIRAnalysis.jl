# Those reduction may work for simple situation like Intrinsic calls, but general situation requires "correct" handling
# leading to yet another overhaul.

extract_type(arg::Core.SSAValue, info::Core.CodeInfo) = info.ssavaluetypes[arg.id]
function extract_type(arg::Core.Argument, info::Core.CodeInfo)
    p = info.parent.specTypes.parameters[arg.n]
    return p
end
#=
extract_type(arg::T, info::Core.CodeInfo) where T <: AbstractString = T
extract_type(arg::T, info) where T <: Number = T
extract_type(arg::T, info) where T <: AbstractChar = T
=#
# extract_type(arg::QuoteNode, info) = typeof(arg.value)
function extract_type(arg::Type, info)
    return Type{arg}
end
extract_type(arg::GlobalRef, info) = extract_type(eval(arg), info)

extract_type(cg_val::CGVal, arg, info::Core.CodeInfo) = !isnothing(cg_val.typ) ? cg_val.typ : extract_type(arg, info)

abstract type AbstractExtContext <: AbstractCodeContext end

function Base.getproperty(ctx::AbstractExtContext, name::Symbol)
    return isdefined(ctx, name) ? getfield(ctx, name) : Base.getproperty(ctx._ctx, name)
end


struct InvokeIntrinsicFinderContext <: AbstractExtContext
    _ctx::CodeContext
    InvokeIntrinsicFinderContext(builder, _module, linfo, source) = new(CodeContext(builder, _module, linfo, source))
end

function emit_call(ctx::InvokeIntrinsicFinderContext, ex, expr_t)
    cg_val = emit_call(ctx._ctx, ex, expr_t)
    c = cg_val.constant
    
    # if c isa Union{AbstractIR.IntrinsicCall, AbstractIR.BuiltinCall} # Builtin can't be "implemented" trivaly: `Base.getfield(_2, _3, $(Expr(:boundscheck)))`
    if c isa AbstractIR.IntrinsicCall
        f = ex.args[1]
        fargs = ex.args[2:end]
        head = c isa AbstractIR.IntrinsicCall ? "IC" : "BC"
        argtypes = map(cg_ori_arg -> extract_type(cg_ori_arg[1], cg_ori_arg[2], ctx.source), zip(c.args, fargs))
        # argtypes = map(arg -> extract_type(arg, ctx.source), fargs)
        print("$head: ")
        format_call(f, argtypes, expr_t) |> println    
    end

    return cg_val
end

function emit_invoke(ctx::InvokeIntrinsicFinderContext, ex, expr_t)
    cg_val = emit_invoke(ctx._ctx, ex, expr_t)
    c = cg_val.constant

    @assert c isa AbstractIR.Invoke
    f = ex.args[2]
    fargs = ex.args[3:end]
    head = "IK"
    argtypes = map(cg_ori_arg -> extract_type(cg_ori_arg[1], cg_ori_arg[2], ctx.source), zip(c.args, fargs))
    # argtypes = map(arg -> extract_type(arg, ctx.source), fargs)
    print("$head: ")
    format_call(f, argtypes, expr_t) |> println

    return cg_val
end


struct ExtractionContext <: AbstractExtContext
    _ctx::CodeContext
    s_set::Set{String}
end

function ExtractionContext(builder, _module, linfo, source)
    _ctx = CodeContext(builder, _module, linfo, source)
    s_set = Set{String}()
    return ExtractionContext(_ctx, s_set)
end

function emit_call(ctx::ExtractionContext, ex, expr_t)
    cg_val = emit_call(ctx._ctx, ex, expr_t)
    c = cg_val.constant
    
    if c isa AbstractIR.IntrinsicCall
        f = ex.args[1]
        fargs = ex.args[2:end]
        argtypes = map(cg_ori_arg -> extract_type(cg_ori_arg[1], cg_ori_arg[2], ctx.source), zip(c.args, fargs))
        # argtypes = map(arg -> extract_type(arg, ctx.source), fargs)
        s = format_call(f, argtypes, expr_t)
        push!(ctx.s_set, s)
    end

    return cg_val
end

