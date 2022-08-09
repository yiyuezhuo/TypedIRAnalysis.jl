# TypeIRAnalysis

### Example

```julia
using TypedIRAnalysis

capture_all_intrinsic(sin, [Float64])

#=
28-element Vector{String}:
 "Base.shl_int(::UInt64,::UInt64)::UInt64"
 "Base.rint_llvm(::Float64)::Float64"
 "Base.not_int(::Bool)::Bool"
 "Base.bitcast(::Type{UInt64},::Float64)::UInt64"
 "Base.Math.sqrt_llvm(::Float64)::Float64"
 "Base.muladd_float(::Float64,::Float64,::Float64)::Float64"
 "Base.mul_float(::Float64,::Float64)::Float64"
 "Base.neg_float(::Float64)::Float64"
 "Base.lt_float(::Float64,::Float64)::Bool"
 "Base.abs_float(::Float64)::Float64"
 "Base.lshr_int(::UInt64,::UInt64)::UInt64"
 "Core.zext_int(::Type{UInt64},::UInt32)::UInt64"
 "Base.fptosi(::Type{Int64},::Float64)::Int64"
 ⋮
 "Base.ult_int(::UInt64,::UInt64)::Bool"
 "Base.ule_int(::UInt32,::UInt32)::Bool"
 "Base.or_int(::Bool,::Bool)::Bool"
 "Base.trunc_int(::Type{UInt32},::UInt64)::UInt32"
 "Base.and_int(::Bool,::Bool)::Bool"
 "Base.sub_int(::UInt32,::UInt32)::UInt32"
 "Base.add_float(::Float64,::Float64)::Float64"
 "Base.eq_float(::Float64,::Float64)::Bool"
 "Base.and_int(::UInt32,::UInt32)::UInt32"
 "Base.shl_int(::UInt32,::UInt64)::UInt32"
 "Base.ult_int(::UInt32,::UInt32)::Bool"
 "Base.and_int(::Int64,::Int64)::Int64"
=#

capture_all_intrinsic(sin, [Float64], recursive=true) 
# 59-element Vector{String}
```

