function format_call(f, args_types, ret_types)
    arg_s = join(map(x->"::$x", args_types), ",")
    return "$(string(f))($arg_s)::$ret_types"
end
