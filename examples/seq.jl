
#@ requires x < 100
#@ ensures \result > x
# SAT
function s(x::Int64)::Int64
    return x + 1
end

# ===== VERIFICATION CONDITIONS =====

# true ==> __RESULT__ > x
# true ==> x + 1 > x