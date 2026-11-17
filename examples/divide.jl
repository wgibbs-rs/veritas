#@ ensures denominator != 0
#@ ensures divide * denominator == numerator
function divide(numerator::Int64, denominator::Int64)::Int64
    return numerator ÷ denominator
end