# Example: The Factorial Function

#@ requires n >= 0

# intentionally successful postcondition
#@ ensures \result >= 1

# intentionally failing postcondition
#@ ensures \result < 0
function factorial(n::Integer)
    if n <= 1 
        return 1
    end
    return factorial(n - 1) * n
end