(*
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
*)

open Propositions
open Ast

let rec apply_statement_to_prop (statement : jast) (vc : prop) (ensures : prop) (debug : bool) : prop =
    if debug then (print_endline (prop_to_string vc));
    match statement with
    | If (e, stmts) ->
        if debug then (Printf.printf "wp(if E then S1 else S2 end, R) = (E -> wp(S1,R)) /\\ (¬E -> wp(S2,R))\n");
        (* condition -> then-block /\ !condition -> not-then-block *)
        let then_block =
            (match stmts with
            | Return _ :: _ ->
                (* If the branch ends with a return, then start from nothing. *)
                List.fold_right (fun x acc -> apply_statement_to_prop x acc ensures debug) stmts ensures
            | _ ->
                List.fold_right (fun x acc -> apply_statement_to_prop x acc ensures debug) stmts vc) in
        let then_block_cond = IF (e, then_block) in

        let else_block_cond = IF (negate_prop e, vc) in

        AND (then_block_cond, else_block_cond)
    | IfElse (e, then_stmts, else_stmts) ->
        if debug then (Printf.printf "wp(if E then S1 else S2 end, R) = (E -> wp(S1,R)) /\\ (¬E -> wp(S2,R))\n");
        (* condition -> then-block /\ !condition -> else-block *)
        let then_block =
            (match then_stmts with
            | Return _ :: _ ->
                (* If the branch ends with a return, then start from nothing. *)
                List.fold_right (fun x acc -> apply_statement_to_prop x acc ensures debug) then_stmts ensures
            | _ ->
                List.fold_right (fun x acc -> apply_statement_to_prop x acc ensures debug) then_stmts vc) in
        let then_block_cond = IF (e, then_block) in

        let else_block = 
            (match else_stmts with
            | Return _ :: _ ->
                (* If the branch ends with a return, then start from nothing. *)
                List.fold_right (fun x acc -> apply_statement_to_prop x acc ensures debug) else_stmts ensures
            | _ ->
                List.fold_right (fun x acc -> apply_statement_to_prop x acc ensures debug) else_stmts vc) in
        let else_block_cond = IF (negate_prop e, else_block) in

        AND (then_block_cond, else_block_cond)
    | Assign (x, y) -> 
        if debug then (Printf.printf "wp(x := E,R) = R[x <- E]\n");
        Substitution.replace_all vc (jvar_to_acsl_expr x) (jexpr_to_acsl_expr y)
    | Return e ->
        if debug then (Printf.printf "wp(x := \\result,R) = R[x <- \\result]\n");
        Substitution.replace_all vc ACSL_Result (jexpr_to_acsl_expr e)
    | _ -> Error.err "Veritas: Error: a statement was not an expression."

let generate_wp_ensures_clause (fn : jast) (ensures: clause) (debug : bool) : prop =
    match ensures, fn with
    | Ensures c, Function (_, _, args, stmts) -> 
        (match args with
        | [] -> c
        | _ -> List.fold_right (fun x acc -> apply_statement_to_prop x acc c debug) stmts c)
    | _, _ -> Error.err "found a clause in vf.ensures that is not of ensures."

(* Returns a list of all VCs to be proven. *)
(* Currently, one per "ensures" clause, but in the
    future, this will be broken up by if statements, etc. *)
let generate_weakest_preconditions (vf : verifiable_function) (debug : bool) : prop =
    (*  If function was quick eliminated, 
        we set the "ensures" clauses to [] to reproduce "UNSAT" *)
    if vf.early_unsat
    then Boolean true
    else
    
    (*  If function has no postcondition contracts,
        we show that "are there any counterexamples" is false. *)
    if List.is_empty vf.ensures
    then Boolean true
    else

    let precondition : prop =
        let clauses : prop list = List.map (function
            | Requires x' -> x'
            | _ -> Error.err "found a clause in vf.requires that is not of requires."
        ) vf.requires in
        match clauses with
        | [] -> Boolean true
        | h :: t -> List.fold_left (create_conjunction) h t
    in

    let assertions_list = List.map (fun x ->
        let postcondition' = generate_wp_ensures_clause vf.fn x debug in
        if (match precondition with (Boolean true) -> true | _ -> false)
        then IF (precondition, postcondition')
        else postcondition'
    ) vf.ensures in

    match assertions_list with
    | [] -> Boolean false
    | h :: t -> List.fold_left (fun x y -> OR (x, NOTP y)) (NOTP h) t
