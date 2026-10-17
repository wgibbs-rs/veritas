(*
ZLib License

Copyright (c) 2026 William Gibbs

This software is provided 'as-is', without any acsl_express or implied
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

(* Traverses a jast, removing immediate verification problems, such as a failure to return. *)

open Ast

(* A program is guaranteed to return if the last statement of the highest block is a return,
    or an if/else statement where both branches return as their last statement. *)

let rec block_guarantees_return (block : jast list) : bool =
    match (List.rev block) with
    | [] -> 
        false
    | [h]
    | h :: _ ->
        match h with
        | IfElse (_, then_stmts, else_stmts) ->
            (* Ensure both blocks return *)
            (block_guarantees_return then_stmts) && (block_guarantees_return else_stmts)
        | Return _ -> true
        | _ -> false

let guarantees_return : jast -> bool = function
    | Function (_, _, _, stmts) -> block_guarantees_return stmts
    | _ -> Error.err "not a function in Quick_elimination.guarantees_return"

(* Veritas will fail if any provided function has no statements in its block. *)
let is_empty_function : jast -> bool = function
    | Function (_, _, _, stmts) ->
        (match stmts with
        | [] -> true
        | _ -> false)
    | _ -> Error.err "not a function in Quick_elimination.is_empty_function"

let early_unsat (x : string) = Printf.printf "Early UNSAT -- %s" x; exit 0

(* This runs all quick elimination checks. *)
(* If a function is eliminated, a.early_unsat = true. *)
let check_fn (a : verifiable_function) : verifiable_function =
    match a.fn with
    | Function (_, _, _, _) ->
        let a' =
            if (is_empty_function a.fn) then
                (print_endline ("NOTE: \"" ^ a.title ^ "\" is empty. No function may be empty.");
                { a with early_unsat=true })
            else a in
        let a' =
            if (not (guarantees_return a'.fn)) then
                (print_endline ("NOTE: \"" ^ a'.title ^ "\" does not always return. All functions must return.");
                { a with early_unsat=true })
            else a in
        a'
    | _ -> Error.err "arg is not a function in QuickElimination.quick_elimination"