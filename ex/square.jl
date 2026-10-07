
#@ ensures \result > x
# UNSAT
function squared(x::Int64)::Int64
    return x * x
end

#@ requires x < 10
#@ requires x > 1
#@ ensures \result > x
# SAT
function squared_positive(x::Int64)::Int64
    return x * x
end

# ===== VERIFICATION CONDITIONS =====
# 1.
# true ==> __RESULT__ > x
# true ==> x * x > x
# UNSAT

# 2.
# x > 0 ==> __RESULT__ > x
# x > 0 ==> x * x > 0
# SAT
