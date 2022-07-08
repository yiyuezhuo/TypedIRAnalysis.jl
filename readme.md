### Example

```julia

@noinline function g(x)
   x - 1
end

function f(x)
     g(x) + x
end

visit_call_and_invoke(f, [Int])
"""
f(::Int64)::Int64
g(::Int64)::Int64
sub_int(::Int64,::Int64)::Int64
add_int(::Int64,::Int64)::Int64
"""
```

### Show Debug info

```julia
using Logging

let l = global_logger()
    l_new = typeof(l)(l.stream, Logging.Debug)
    global_logger(l_new)
    l_new
end

visit_call_and_invoke(randn, [])
```