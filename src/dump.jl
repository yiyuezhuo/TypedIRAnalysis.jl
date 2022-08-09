#=
function pydump(code_info::Core.CodeInfo)
    emitted = emit_function(code_info)
    simplify_code_info_py!(emitted)
    return emitted
end

"""
Helper
"""
function pydump(f, argtypes)
    code_info = code_typed(f, argtypes)[1].first
    return pydump(code_info)
end
=#

statictypedump(typ::Type) = transform_static_type(typ) # AbstractIRJlType[](typ)
statictypedump(mu::Core.Compiler.MaybeUndef) = AbstractIR.MaybeUndef(transform_static_type(mu.typ))
# statictypedump(x) = transform_static_type(x) # TODO: is it proper to use a general fallback here? e.x. `Core.PartialStruct`, `Core.Const`
statictypedump(x) = Core.Compiler.widenconst(x)

staticvaluedump(typ::Type) = transform_static_type(typ)
staticvaluedump(x) = transform_value(x)

transform_cfg(sr::Core.Compiler.StmtRange) = AbstractIR.irrange(sr.start, sr.stop+1)
transform_cfg(bb::Core.Compiler.BasicBlock) = AbstractIR.BasicBlock(transform_cfg(bb.stmts), AbstractIR.irlist(bb.preds), AbstractIR.irlist(bb.succs))
transform_cfg(cfg::Core.Compiler.CFG) = AbstractIR.CFG(AbstractIR.irlist(transform_cfg.(cfg.blocks)), AbstractIR.irlist(cfg.index))

# temp mock

transform_value(x) = AbstractIR.JValue(x)
transform_static_type(x) = AbstractIR.JType(x)
