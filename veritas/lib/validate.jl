"""
ZLib License

Copyright (c) 2026 William Gibbs

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

3. This notice may not be removed or altered from any source distribution.
"""

# Process the code with clauses in the form of comments. 
# Failure -> Prints the error to the stdout, which is then shown by OCaml. exit(1)
# Success -> Does not print anything. exit(0)

try
    include(expr -> (Meta.isexpr(expr, :error) || Meta.isexpr(expr, :incomplete)) ? expr : nothing, ARGS[1])
    exit(0)
catch e
    println(stdout, "Veritas: Error: Failed to parse input file...")
    showerror(stdout, e, catch_backtrace())
    exit(1)
end