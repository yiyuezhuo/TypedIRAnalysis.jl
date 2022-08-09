module TypedIRAnalysis

export capture_all_intrinsic, emit_function, visit_recursive

include("AbstractIR/AbstractIR.jl")

using .AbstractIR
using .AbstractIR: CGVal

include("utils.jl")
include("codegen.jl")
include("devtools.jl")
include("dump.jl")
# include("reduction.jl")
include("hookers.jl")
include("visit.jl")

function capture_all_intrinsic(f, argtypes; recursive=false)

    function emit_function_step(code_info)
        ctx = make_context(TypedIRAnalysis.ExtractionContext, code_info)
        emit_function(ctx)
        return ctx.s_set
    end

    if !recursive
        code_info = Base.code_typed(f, argtypes)[1].first
        return emit_function_step(code_info) |> collect
    end

    set_iter = visit_recursive(emit_function_step, f, argtypes) |> values
    return union(set_iter...) |> collect
end

# export visit_call_and_invoke

# include("Scanner.jl")

# greet() = print("Hello World!")

end # module
