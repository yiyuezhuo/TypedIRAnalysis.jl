
abstract type IR end

struct JValue <: IR
    value
end

struct JType <: IR
    typ
end

## 

struct Undef <: IR
end

struct Argument <: IR
    n::Int
end

struct SSAValue <: IR
    id::Int
end

struct QuoteNode <: IR
    value 
end

#=
struct Const
    val
end
=#

struct Call <: IR
    func
    args
end

struct IntrinsicCall <: IR
    func
    args
end

struct BuiltinCall <: IR
    func
    args
end

struct Ifelse <: IR
    cond
    A
    B
end

struct Invoke <: IR
    method_instance
    # func
    args
end

struct Foreigncall <: IR
    func
    ret_type
    arg_types
    num_varargs
    calling_convention
    calling_convention_args
    gc_roots
end

struct CGlobal <: IR
    func_name::String
    library_name::Union{String, Nothing}
end

struct CCall <: IR
    func
    ret_type
    arg_types
    num_varargs
    args
    gc_roots
end

struct Enter <: IR
    dest::Int
end

struct Leave <: IR
    n::Int
end

struct PopException <: IR
    top::SSAValue
end

struct TheException <: IR
end

struct New <: IR
    ty
    args
end

struct StaticParameter <: IR
    id::Int
end

struct GcPreserveBegin <: IR
    args
end

struct GcPreserveEnd <: IR
end

struct ThrowUndefIfNot <: IR
    var
    cond
end

struct Copyast <: IR
    expr
end


struct Symbol
    string::String
end


struct GlobalRef <: IR
    mod::Module
    name::Symbol
end

struct Expr <: IR
    head::Symbol
    args
end

struct GotoNode <: IR
    label::Int
end

struct GotoIfNot <: IR
    cond
    dest::Int
end

struct ReturnNode <: IR
    val
end

struct Method <: IR
    name::Symbol
    _module::Module
    sig
end

struct MethodInstance <: IR
    jl_ref
end

struct PhiNode <: IR
    edges
    values
end

struct UpsilonNode <: IR
    val
end

struct PhiCNode <: IR
    values
end

struct PiNode <: IR
    val
    typ
end

struct Boundscheck <: IR
end

struct Unreachable <: IR
end

struct Ignored <: IR
end

struct LineInfoNode <: IR
    _module::Module
    method::Method
    file::Symbol
    line::Int
    inlined_at::Int
end

struct StmtRange <: IR
    start::Int
    stop::Int
end

struct BasicBlock <: IR
    stmts::StmtRange
    preds
    succs
end

struct CFG <: IR
    blocks
    index
end

struct CodeInfo <: IR
    code
    ssavaluetypes
    ssaflags
    linetable
    parent
    specTypes
    sparam_vals
    cfg
end