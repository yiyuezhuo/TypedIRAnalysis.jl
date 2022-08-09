
module AbstractIR

include("IR.jl")

"""
Uncontrolled: Module
"""

# metadata tracking during "codegen"
mutable struct CGVal{T}
    V
    constant::T
    typ # never NULL
    tv # transformed value
end


irlist(x) = collect(x)
irtuple(x) = tuple(irlist(x)...)
irrange(x, y) = StmtRange(x, y) # TODO: temp hack

end
