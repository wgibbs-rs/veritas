"""
ZLib License

Copyright (c) 2025 William Gibbs

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

# Test if JuliaSyntax and JSON packages are available. If not, add them.
if Base.find_package("JuliaSyntax") === nothing; using Pkg; Pkg.add("JuliaSyntax"); end
if Base.find_package("JSON") === nothing; using Pkg; Pkg.add("JSON"); end

using JuliaSyntax
using JSON

function expr_to_dict(expr::Expr)
    Dict(
        "head" => string(expr.head),
        "args" => [
            arg isa Expr ? expr_to_dict(arg) :
            arg isa Symbol ? string(arg) :
            arg
            for arg in expr.args
            if !(arg isa LineNumberNode)
        ]
    )
end

function get_ast(src::String)
    """
    Retrieve the AST in the form of an Expr tree. Removes comments and other trivia.
    :param src: the source code for the target script in string format.
    :return: an Expr tree representing the generated AST.
    """
    return JuliaSyntax.parseall(JuliaSyntax.Expr, src)
end

# ARGS[1] will refer to the name of the target Julia file.
# ARGS[2] will be 'y' if we print the AST.

# Retrieve the file contents as a plain string.
src_plain = read(ARGS[1], String)

# Replace instances of "#@" with "@ACSL" macro to prevent removal during AST retrieval.
source_acsl_included = replace(src_plain, "#@" => "@ACSL")
# Replace instances of "\result" with "__VERITAS_RESULT__" to prevent weird parsing problems.
source_acsl_included = replace(source_acsl_included, "\\result" => "__VERITAS_RESULT__")

# Retrieve the AST as an Expr tree.
ast = get_ast(source_acsl_included)

# Test if OCaml wants us to pretty-print, or return JSON text.
if ARGS[2] == "y"
    # Print the AST in a Julia pretty-printed format.
    dump(ast, maxdepth=100)
else
    # Print AST in JSON format.
    print(JSON.json(expr_to_dict(ast)))
end