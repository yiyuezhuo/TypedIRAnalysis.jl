function set_debug_level!(level)
    let l = global_logger()
        l_new = typeof(l)(l.stream, level)
        global_logger(l_new)
        l_new
    end
end

enable_debug!() = set_debug_level!(Logging.Debug)
disable_debug!() = set_debug_level!(Logging.Info)

function check(code_info)
    dumped = pydump(code_info)
    println(code_info)
    foreach(println, dumped.code)
end
