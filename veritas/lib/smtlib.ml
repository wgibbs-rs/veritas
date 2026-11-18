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

open Propositions
open Ast

(*
Provides a script of SMT-LIB2 to Z3 through the command line interface;
returns the result provided by Z3.
*)
let check_smtlib2_string input =
    let ic = Unix.open_process_in ("echo " ^ Filename.quote input ^ " | z3 -in") in
    let output = In_channel.input_all ic in
    let _ = Unix.close_process_in ic in
    output

(*
Creates the header for a new SMT2 file given the content of the file.
*)
let smt2_file : string -> string = fun assertions ->
    Printf.sprintf "%s\n%s\n%s\n%s\n"
    "(set-option :print-success false)"
    "(set-option :produce-models true)"
    assertions
    "(exit)"

(*
Creates a sectioned off block of SMT-LIB from SMT-LIB `content` with a commented
`title`.
*)
let smt2_section (content : string) (title : string) : string =
    Printf.sprintf "; %s\n%s\n%s\n%s\n%s\n\n" 
    title
    "(push)" 
    content 
    "(check-sat)"
    "(pop)"

(*
Converts an ACSL expression `e` with state `v` into an assertion held by 
generated SMT-LIB script.
*)
let rec acsl_expr_to_smt (e : acsl_expr) (v : jvar list) : string =
    match e with
    | ACSL_Add (lhs, rhs) -> 
        Printf.sprintf "(bvadd %s %s)" (acsl_expr_to_smt lhs v) (acsl_expr_to_smt rhs v)
    | ACSL_Subtract (lhs, rhs) ->
        Printf.sprintf "(bvsub %s %s)" (acsl_expr_to_smt lhs v) (acsl_expr_to_smt rhs v)
    | ACSL_Multiply (lhs, rhs) ->
        Printf.sprintf "(bvmul %s %s)" (acsl_expr_to_smt lhs v) (acsl_expr_to_smt rhs v)
    | ACSL_Divide (lhs, rhs) ->
        Printf.sprintf "(bvsdiv %s %s)" (acsl_expr_to_smt lhs v) (acsl_expr_to_smt rhs v)
    | ACSL_Old x (* ACSL \old keyword *) -> 
        (* Assume \old(x) just preserves x during WP. *)
        acsl_expr_to_smt x v
    | ACSL_Result (* ACSL \result keyword *) -> "__RESULT__" (* Unreachable *)
    | ACSL_Ident s -> s
    | ACSL_String s -> s
    | ACSL_Int d -> Printf.sprintf "((_ int_to_bv 64) %d)" d
    | ACSL_Float f -> Printf.sprintf "%f" f
    | ACSL_Boolean b -> Printf.sprintf "%b" b

(*
Converts a proposition `p` with state `vars` into SMT-LIB script. Called by
prop_to_smtlib.
*)
let rec prop_to_smtlib_aux (p : prop) (v : jvar list) : string =
    match p with
    | Boolean b -> 
        Printf.sprintf "%b" b
    | EQ (lhs, rhs) -> 
        Printf.sprintf "(= %s %s)" (acsl_expr_to_smt lhs v) (acsl_expr_to_smt rhs v)
    | NEQ (lhs, rhs) -> 
        Printf.sprintf "(distinct %s %s)" (acsl_expr_to_smt lhs v) (acsl_expr_to_smt rhs v)
    | LT (lhs, rhs) -> 
        Printf.sprintf "(bvslt %s %s)" (acsl_expr_to_smt lhs v) (acsl_expr_to_smt rhs v)
    | LE (lhs, rhs) -> 
        Printf.sprintf "(bvsle %s %s)" (acsl_expr_to_smt lhs v) (acsl_expr_to_smt rhs v)
    | GT (lhs, rhs) -> 
        Printf.sprintf "(bvsgt %s %s)" (acsl_expr_to_smt lhs v) (acsl_expr_to_smt rhs v)
    | GE (lhs, rhs) -> 
        Printf.sprintf "(bvsge %s %s)" (acsl_expr_to_smt lhs v) (acsl_expr_to_smt rhs v)
    | NOTP p -> 
        Printf.sprintf "(not %s)" (prop_to_smtlib_aux p v)
    | AND (lhs, rhs) -> 
        Printf.sprintf "(and %s %s)" (prop_to_smtlib_aux lhs v) (prop_to_smtlib_aux rhs v)
    | OR (lhs, rhs) -> 
        Printf.sprintf "(or %s %s)" (prop_to_smtlib_aux lhs v) (prop_to_smtlib_aux rhs v)
    | IF (lhs, rhs) -> 
        Printf.sprintf "(=> %s %s)" (prop_to_smtlib_aux lhs v) (prop_to_smtlib_aux rhs v)
    | IFF (lhs, rhs) -> 
        Printf.sprintf "(= %s %s)" (prop_to_smtlib_aux lhs v) (prop_to_smtlib_aux rhs v)

(* Converts a proposition `p` with state `vars` into SMT-LIB script. *)
let prop_to_smtlib (p : prop) (vars : jvar list): string =
    Printf.sprintf "\n(assert %s)\n" (prop_to_smtlib_aux p vars)

(* Creates a new const given a name and kind. *)
let define_const (name : string) (kind : string) : string =
    Printf.sprintf "(declare-const %s %s)" name kind

(* Converts a Julia variable to an SMT-LIB const. *)
let rec jvar_to_smtlib (jvars : jvar list) : string =
    match jvars with
    | [] -> "\n"
    | h :: t -> 
        (match h with
        | JAny s -> "; Error: " ^ s ^ " |—— Any"
        | JInt64 s | JFloat64 s -> (define_const s "(_ BitVec 64)")
        | JString s -> (define_const s "String")
        | JBool s -> (define_const s "Bool"))
        ^ "\n" ^ (jvar_to_smtlib t)