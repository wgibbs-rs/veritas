# Veritas
Veritas is a static analyzer for the Julia programming language.

# Usage
Veritas is used through a CLI, not as a Julia package.
[ACSL annotations](https://www.frama-c.com/download/frama-c-acsl-implementation.pdf) should be written within the target Julia file, written directly above any functions. Veritas ACSL annotations are written line-by-line, each clause starting with the pattern `#@`. For example:

```jl
#@ requires x < 100
#@ ensures \result > x
function s(x::Int64)::Int64
    return x + 1
end
```

To run Veritas, use the syntax

```bash
veritas path/to/file.jl
```

A verbose output, which includes the AST, weakest precondition steps, verification conditions, generated SMT-LIB2, Z3 output, and Veritas output, can be printed by providing the `--debug` flag.

A list of further features can be printed by providing the `--help` flag.

# Contributing
We welcome any and all developers interested in contributing to this project. If you're unsure how to contribute to the project, we heavily advise looking at the open Github Issues. These forums can provide insight into what needs immediate work, and how new users can contribute.

## Issues
Everyone is welcome to create and contribute to Issues, and discussion should remain civil, aiming to fix the issue and contribute to the project. Issues are reported and disputed through GitHub Issues.

# Legal
Veritas is licensed under the "ZLib License" In short, any usage of this project is permitted, so long as the original license is included, a change to the software is noted, and the product is not claimed to be the creation of any individual other than William Gibbs. This is not legal advice. For the official license for Veritas, go to [LICENSE.md](LICENSE.md)
