(*
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
*)

open Propositions
open Ast

let apply_statement_to_vc (statement : ast) (_vc' : vc) : vc =
    match statement with
    | Expr (head, _args) -> 
        (match head with
        | Symbol "=" -> failwith "TODO"
        | Symbol "return" -> failwith "TODO"
        | _ -> failwith "Veritas: Error: unrecognized expression name.")
    | _ -> failwith "Veritas: Error: a statement was not an expression."

let generate_weakest_precondition_ensures_clause (_fn : ast) (_requires : clause list) (_ensures: clause) : vc =
    failwith "TODO"

(* Returns a list of all VC's to be proven. *)
(* Currently, one per "ensures" clause, but in the 
    future, this will be broken up by if statements, etc. *)
let generate_weakest_precondition (vf : verifiable_function) : vc list =
    List.map (
        fun x -> generate_weakest_precondition_ensures_clause (vf.fn) (vf.requires) x
    ) vf.ensures
