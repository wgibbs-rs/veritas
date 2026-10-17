

# UNSAT; fails to return at x == 0
function is_positive_if_if(x::Int64)::Int64
    if (x > 0)
        return true
    end
    if (x < 0)
        return false
    end
end

# SAT
function is_positive_if_else(x::Int64)::Bool
    if (x > 0)
        return true
    else
        return false
    end
end

# SAT
#@ ensures x > 0 ==> \result == true
function is_positive_if_default(x::Int64)::Bool
    if (x > 0)
        return true
    end
    return false
end
