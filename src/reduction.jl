function eliminate_cgval!(node::Py)
    # @show node
    
    update_dict = PythonCall.pydict()
    if PythonCall.pyisjl(node)
        ret = eliminate_cgval!(PythonCall.pyjlvalue(node))
        return ret
    elseif PythonCall.pyisinstance(node, PythonCall.pybuiltins.tuple)
        return PythonCall.pytuple(eliminate_cgval!.(node))
    elseif PythonCall.pyisinstance(node, PythonCall.pybuiltins.list)
        return PythonCall.pylist(eliminate_cgval!.(node))
    elseif PythonCall.pyisinstance(node, PythonCall.pybuiltins.dict)
        @warn "Unexpected dict: $node"
        # node.update(update_dict)
        return node
    elseif PythonCall.pytruth(dataclasses.is_dataclass(node))
    # elseif pyhasattr(node, "__dict__") # TODO: find an efficient way
        for field in dataclasses.fields(node)
            value = PythonCall.pygetattr(node, field.name)
            update_dict[field.name] = eliminate_cgval!(value)
        end
        # node.__dict__.update(update_dict) # Sometimes it works, sometimes `AttributeError: 'mappingproxy' object has no attribute 'update'`
        for (key, value) in update_dict.items()
            # @show key value
            # pysetattr(node, key, value)
            node.__dict__[key] = value # pysetattr doesn't work for frozen dataclass object.
        end
        return node
    end
    return node # general fallback for int, string, float, etc.
end

function eliminate_cgval!(node::CGVal)
    !isnothing(node.tv) && return node.tv
    return node.constant
end

function eliminate_cgval!(nodes::AbstractVector{CGVal})
    return PythonCall.pylist(eliminate_cgval!.(nodes))
end

eliminate_cgval!(x) = x

function simplify_code_info_py!(code_info_py::Py)
    for code in code_info_py.code
        @debug "$code ==>"
        eliminate_cgval!(code)
        @debug code
    end
    return code_info_py
end
