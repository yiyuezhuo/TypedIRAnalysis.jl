# https://stackoverflow.com/questions/41658692/how-to-determine-if-julia-object-is-callable
iscallable(f) = !isempty(methods(f))

"""
Visiting with stepping into MethodInstance.
visit: Core.CodeInfo => somethong, ex: visit ∈ {pydump, step_probe, emit_function}
"""
function visit_recursive(visit::Function, specTypes::Type{<:Tuple}, ret_dict::IdDict = Base.IdDict(), specTypes_stack = Any[]; kwargs...)
    if haskey(ret_dict, specTypes)
        return
    end

    push!(specTypes_stack, specTypes)

    @debug "Enter $specTypes"
    
    ctt = Base.code_typed_by_type(specTypes)
    @assert length(ctt) == 1 "but found length(ctt)=$(length(ctt)) for specTypes=$specTypes"
    info = ctt[1].first
    
    try
        ret_dict[specTypes] = visit(info; kwargs...)
    catch e
        println("visit report: specTypes=$specTypes")
        rethrow()
    end
    
    # ret_dict[specTypes] = visit(info; kwargs...) # Quick helper for the Debugger

    # TODO: consider ssatype and general return type handling
    for (idx, line) in enumerate(info.code)
        if line isa Expr && line.head == :invoke && line.args[1] isa Core.MethodInstance
            sub_specTypes = line.args[1].specTypes
            # visit_recursive(visit, sub_specTypes, ret_dict, specTypes_stack; kwargs...)
            try
                visit_recursive(visit, sub_specTypes, ret_dict, specTypes_stack; kwargs...)
            catch e
                @show idx line specTypes
                rethrow()
            end
        end
    end

    pop!(specTypes_stack)
    return ret_dict
end

"""
Helper
"""
function visit_recursive(visit::Function, f, argtypes; kwargs...)
    specTypes = Tuple{typeof(f), argtypes...} # Base.typeof
    return visit_recursive(visit, specTypes; kwargs...)
end
