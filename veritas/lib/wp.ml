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

(* Does not replace values within \old() *)
let rec replace_e_e (e : acsl_expr) (x : acsl_expr) (y : acsl_expr) : acsl_expr =
    match e with
    | ACSL_Add (lhs, rhs) -> ACSL_Add (replace_e_e lhs x y, replace_e_e rhs x y)
    | ACSL_Subtract (lhs, rhs) -> ACSL_Subtract (replace_e_e lhs x y, replace_e_e rhs x y)
    | ACSL_Multiply (lhs, rhs) -> ACSL_Multiply (replace_e_e lhs x y, replace_e_e rhs x y)
    | ACSL_Divide (lhs, rhs) -> ACSL_Divide (replace_e_e lhs x y, replace_e_e rhs x y)
    | _ -> if e = x then y else e

let rec replace_all (p : prop) (x : acsl_expr) (y : acsl_expr) : prop =
    match p with
    | Boolean _ -> p
    | EQ (lhs, rhs) -> EQ (replace_e_e lhs x y, replace_e_e rhs x y)
    | NEQ (lhs, rhs) -> NEQ (replace_e_e lhs x y, replace_e_e rhs x y)
    | LT (lhs, rhs) -> LT (replace_e_e lhs x y, replace_e_e rhs x y)
    | LE (lhs, rhs) -> LE (replace_e_e lhs x y, replace_e_e rhs x y)
    | GT (lhs, rhs) -> GT (replace_e_e lhs x y, replace_e_e rhs x y)
    | GE (lhs, rhs) -> GE (replace_e_e lhs x y, replace_e_e rhs x y)
    | NOT x' -> NOT (replace_all x' x y)
    | AND (lhs, rhs) -> AND (replace_all lhs x y, replace_all rhs x y)
    | OR (lhs, rhs) -> OR (replace_all lhs x y, replace_all rhs x y)
    | IF (lhs, rhs) -> IF (replace_all lhs x y, replace_all rhs x y)
    | IFF (lhs, rhs) -> IFF (replace_all lhs x y, replace_all rhs x y)

let apply_statement_to_prop (statement : jast) (vc : prop) (debug : bool) : prop =
    if debug then (print_endline (prop_to_string vc));
    match statement with
    | Assign (x, y) -> 
        if debug then (Printf.printf "replace all x with y\n");
        replace_all vc (jvar_to_acsl_expr x) (jexpr_to_acsl_expr y)
    | Return e ->
        let output = replace_all vc ACSL_Result (jexpr_to_acsl_expr e) in
        if debug then begin
            Printf.printf "replace all \"\\result\" with y\n";
            print_endline (prop_to_string output) 
        end;
        output
    | _ -> failwith "Veritas: Error: a statement was not an expression."

let generate_wp_ensures_clause (fn : jast) (_context : jvar list) (ensures: clause) (debug : bool) : prop =
    match ensures, fn with
    | Ensures c, Function (_, _, args, stmts) -> 
        (match args with
        | [] -> c 
        | _ -> List.fold_right (fun x acc -> apply_statement_to_prop x acc debug) stmts c)
    | _, _ -> failwith "found a clause in vf.ensures that is not of ensures."

(* Returns a list of all VC's to be proven. *)
(* Currently, one per "ensures" clause, but in the 
    future, this will be broken up by if statements, etc. *)
let generate_weakest_preconditions (vf : verifiable_function) (debug : bool) : prop list =
    let precondition : prop =
        let clauses : prop list = List.map (function
            | Requires x' -> x'
            | _ -> failwith "found a clause in vf.requires that is not of requires."
            ) vf.requires in
        match clauses with
        | [] -> Boolean true
        | h :: t -> List.fold_left (create_conjunction) h t
    in
    List.map (fun x -> IF (precondition, generate_wp_ensures_clause vf.fn vf.context x debug)) 
    vf.ensures 
