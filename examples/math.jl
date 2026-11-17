
#@ requires x > 1
#@ ensures x < \old(x)
function math(x::Int64)::Int64
    y = x + 3
    x = x - 1
    z = y / 2
    a = z * 4
    return a
end

# true ==> __RESULT__ > 0
# a > 0
# z * 4 > 0
# (y / 2) * 4 > 0
# (y / 2) * 4 > 0
# ((x + 3) / 2) * 4 > 0
