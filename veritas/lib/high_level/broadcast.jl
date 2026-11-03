function broadcast_add(a::Vector{Any}, b::Vector{Any})::Vector{Any}
    output::Vector{Any} = []
    if (length(a) ≠ length(b))
        error("a and b do not have the same length in a .+ b")
    end
    for i in 1:length(a)
        push!(output, a[i] + b[i])
    end
    return output
end

function broadcast_subtract(a::Vector{Any}, b::Vector{Any})::Vector{Any}
    output::Vector{Any} = []
    if (length(a) ≠ length(b))
        error("a and b do not have the same length in a .+ b")
    end
    for i in 1:length(a)
        push!(output, a[i] - b[i])
    end
    return output
end

function broadcast_multiply(a::Vector{Any}, b::Vector{Any})::Vector{Any}
    output::Vector{Any} = []
    if (length(a) ≠ length(b))
        error("a and b do not have the same length in a .+ b")
    end
    for i in 1:length(a)
        push!(output, a[i] * b[i])
    end
    return output
end

function broadcast_divide(a::Vector{Any}, b::Vector{Any})::Vector{Any}
    output::Vector{Any} = []
    if (length(a) ≠ length(b))
        error("a and b do not have the same length in a .+ b")
    end
    for i in 1:length(a)
        push!(output, a[i] ÷ b[i])
    end
    return output
end