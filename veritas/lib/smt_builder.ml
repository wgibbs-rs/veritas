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

let smt2_file (assertions : string) : string =
    Printf.sprintf "%s\n%s\n%s\n"
    "
    (set-option :print-success false)
    (set-option :produce-models true)
    "
    assertions
    "
    (check-sat)
    (exit)
    "

let rec expr_to_smt2 : expr -> string = function
    | Add (lhs, rhs) -> 
        Printf.sprintf "(+ %s %s)" (expr_to_smt2 lhs) (expr_to_smt2 rhs)
    | Subtract (lhs, rhs) ->
        Printf.sprintf "(- %s %s)" (expr_to_smt2 lhs) (expr_to_smt2 rhs)
    | Multiply (lhs, rhs) ->
        Printf.sprintf "(* %s %s)" (expr_to_smt2 lhs) (expr_to_smt2 rhs)
    | Divide (lhs, rhs) ->
        Printf.sprintf "(/ %s %s)" (expr_to_smt2 lhs) (expr_to_smt2 rhs)
    | Old x (* ACSL \old keyword *) -> 
        (* Assume \old(x) just preserves x during WP. *)
        Printf.sprintf "(%s)" (expr_to_smt2 x)
    | Result (* ACSL \result keyword *) -> "ERR" (* Unreachable *)
    | Ident s -> ("(" ^ s ^ ")")
    | String s -> ("(" ^ s ^ ")")
    | Int d -> Printf.sprintf "(%d)" d
    | Float f -> Printf.sprintf "(%f)" f
    | Boolean b -> Printf.sprintf "(%b)" b

let rec prop_to_smt2 : prop -> string = function
    | Boolean b -> Printf.sprintf "(%b)" b
    | EQ (lhs, rhs) -> Printf.sprintf "(= %s %s)" (expr_to_smt2 lhs) (expr_to_smt2 rhs)
    | NEQ (lhs, rhs) -> Printf.sprintf "(!= %s %s)" (expr_to_smt2 lhs) (expr_to_smt2 rhs)
    | LT (lhs, rhs) -> Printf.sprintf "(< %s %s)" (expr_to_smt2 lhs) (expr_to_smt2 rhs)
    | LE (lhs, rhs) -> Printf.sprintf "(<= %s %s)" (expr_to_smt2 lhs) (expr_to_smt2 rhs)
    | GT (lhs, rhs) -> Printf.sprintf "(> %s %s)" (expr_to_smt2 lhs) (expr_to_smt2 rhs)
    | GE (lhs, rhs) -> Printf.sprintf "(>= %s %s)" (expr_to_smt2 lhs) (expr_to_smt2 rhs)
    | NOT p -> Printf.sprintf "(not %s)" (prop_to_smt2 p)
    | AND (lhs, rhs) -> Printf.sprintf "(and %s %s)" (prop_to_smt2 lhs) (prop_to_smt2 rhs)
    | OR (lhs, rhs) -> Printf.sprintf "(or %s %s)" (prop_to_smt2 lhs) (prop_to_smt2 rhs)
    | IF (lhs, rhs) -> Printf.sprintf "(=> %s %s)" (prop_to_smt2 lhs) (prop_to_smt2 rhs)
    | IFF (lhs, rhs) -> Printf.sprintf "(= %s %s)" (prop_to_smt2 lhs) (prop_to_smt2 rhs)

let define_const (name : string) (kind : string) : string = 
    Printf.sprintf "(define-const %s %s)" name kind

let convert_prop_to_smtlib (_p : prop) : string =
    failwith "TODO convert_prop_to_smtlib"

(* let define_const (name : string) (t : type) *)