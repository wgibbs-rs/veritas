function __VERITAS__ternary_operator(c::Bool, if_true::Any, if_false::Any)::Any
    if c
        return if_true
    else
        return if_false
    end
end

# (c ? e1 : e2)