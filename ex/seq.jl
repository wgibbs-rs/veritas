
#@ ensures \result > x
# SAT
function s(x)
    return x + 1
end

# ===== VERIFICATION CONDITIONS =====

# true ==> __RESULT__ > x
# true ==> x + 1 > x