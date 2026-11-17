
#@ ensures \result != \old(x)
function negate_if_else(x::Bool)::Bool
    if (x == true)
        return false
    else
        return true
    end
end