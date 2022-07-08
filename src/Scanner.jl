function format_call(f, args_types, ret_types)
    arg_s = join(map(x->"::$x", args_types), ",")
    return "$(string(f))($arg_s)::$ret_types"
end

extract_type(arg::Core.SSAValue, info::Core.CodeInfo) = info.ssavaluetypes[arg.id]
extract_type(arg::Core.Argument, info::Core.CodeInfo) = info.parent.specTypes.parameters[arg.n]
extract_type(arg::String, info::Core.CodeInfo) = String
extract_type(arg::T, info) where T <: Number = T
# extract_type(arg::T, info) where T <: Number = T
extract_type(arg::QuoteNode, info) = typeof(arg.value)
extract_type(arg::Type{T}, info) where T = T # TODO: need cautious check
extract_type(arg::GlobalRef, info) = typeof(eval(arg))

function visit_call_and_invoke(f, argtypes)
    specTypes = Tuple{typeof(f), argtypes...}
    return visit_call_and_invoke(specTypes, Base.IdSet())
end

function visit_call_and_invoke(specTypes, visited::Base.IdSet = Base.IdSet())
    if specTypes ∈ visited
        return
    end
    push!(visited, specTypes)
    
    ps = specTypes.parameters
    f = ps[1].instance
    argtypes = ps[2:end]
    pairs = code_typed(f, argtypes)
    @assert length(pairs) == 1
    pair = pairs[1]
    info = pair.first
    
    format_call(f, argtypes, info.rettype) |> println
    
    for (line, ssatype) in zip(info.code, info.ssavaluetypes)
        if typeof(line) == Expr
            if line.head == :invoke
                methodinstance = line.args[1]
                visit_call_and_invoke(methodinstance.specTypes, visited)
            elseif line.head == :call
                f = eval(line.args[1])
                if isa(f, Core.Builtin) # the same predicate used in code_typed to determine if a functio is generic
                    argtypes = map(arg -> extract_type(arg, info), line.args[2:end])
                    format_call(f, argtypes, ssatype) |> println
                else
                    # Can't be determined in compiling time
                    @debug begin
                        def = info.parent.def
                        @show info line ssatype def.file def.line def.module
                        functionloc(def) .|> println
                        dump(line)
                        "Above: A case which is not determined in compiling time."
                    end
                end
            end
        end
    end
end