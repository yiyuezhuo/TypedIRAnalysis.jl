
using Test
using AbstractGPs
using InteractiveUtils

using TypedIRAnalysis
using TypedIRAnalysis: AbstractIR


function f_abstract_gp()
    # Generate toy synthetic data.
    x = rand(10)
    # y = sin.(x)
    y = rand(10)

    # Define GP prior with Matern-3/2 kernel
    f = GP(Matern32Kernel())

    # Finite projection of `f` at inputs `x`.
    # Added Gaussian noise with variance 0.001.
    fx = f(x, 0.001)

    # Log marginal probability of `y` under `f` at `x`.
    # Quantity typically maximised to train hyperparameters.
    logpdf(fx, y)

    # Exact posterior given `y`. This is another GP.
    p_fx = posterior(fx, y)

    # Log marginal posterior predictive probability.
    return logpdf(p_fx(x), y)
end


function test_is_non_trival_vector(x, recursive)
    @test x isa AbstractVector
    if recursive # Sometimes single layer CodeInfo has only 1 `invoke` (e.x: `log``)
        @test length(x) >= 2 
    end
end


@testset "a testset" begin
    code_info = (@code_typed sin(1.)).first
    @test emit_function(code_info) isa AbstractIR.CodeInfo

    ctx = TypedIRAnalysis.make_context(TypedIRAnalysis.ExtractionContext, code_info)
    TypedIRAnalysis.emit_function(ctx)
    @test length(ctx.s_set) >= 1

    for f in [sin, exp, log], recursive in [true, false]
        test_is_non_trival_vector(capture_all_intrinsic(f, [Float64]; recursive), recursive)
    end

    for f in [f_abstract_gp, rand, randn], recursive in [true, false]
        test_is_non_trival_vector(capture_all_intrinsic(f, []; recursive), recursive)
    end
end

