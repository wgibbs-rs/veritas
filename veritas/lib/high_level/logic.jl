#@ ensures (a == true) ==> (b == true)
#@ ensures (a == false) ==> (b == false)
function and_b(a::Bool, b::Bool)::Bool
    if a 
        return b
    else 
        return false
    end
end

#@ ensures (a == false) /\ (b == true)
function or_b(a::Bool, b::Bool)::Bool
    if a
        return true
    else
        return b
    end
end

#@ ensures \old(b) ==> b
#@ ensures !\old(b) ==> !b
function not_b(b::Bool)::Bool
    if b
        return false
    else
        return true
    end
end

# ensures a ==> ((b ==> false) && (!b ==> true)
# ensures a ==> ((b ==> true) && (!b ==> false)
# function xor_b(a::Bool, b::Bool)::Bool
#     if a
#         return not_b(b)
#     else
#         return b
#     end
# end