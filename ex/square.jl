
#@ ensures \result > x
# UNSAT
function squared(x)
    return x * x
end

#@ requires x > 0
#@ ensures \result > x
# SAT
function squared_positive(x)
    return x * x
end