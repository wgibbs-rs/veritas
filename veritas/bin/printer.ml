(*
ZLib License

Copyright (c) 2025 William Gibbs

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

open Ast
open Propositions

let rec print_jexpr (expr : jexpr) (indent : string) =
    let print_node_and_dual_children indent node lhs rhs =
        Printf.printf "%s%s\n" indent node;
        print_jexpr lhs (indent ^ "  ");
        print_jexpr rhs (indent ^ "  ") in
    match expr with
    | Add (lhs, rhs) -> print_node_and_dual_children indent "+" lhs rhs
    | Subtract (lhs, rhs) -> print_node_and_dual_children indent "-" lhs rhs
    | Multiply (lhs, rhs) -> print_node_and_dual_children indent "*" lhs rhs
    | Divide (lhs, rhs) -> print_node_and_dual_children indent "/" lhs rhs
    | EQ (lhs, rhs) -> print_node_and_dual_children indent "==" lhs rhs
    | NEQ (lhs, rhs) -> print_node_and_dual_children indent "!=" lhs rhs
    | LT (lhs, rhs) -> print_node_and_dual_children indent "<" lhs rhs
    | LE (lhs, rhs) -> print_node_and_dual_children indent "<=" lhs rhs
    | GT (lhs, rhs) -> print_node_and_dual_children indent ">" lhs rhs
    | GE (lhs, rhs) -> print_node_and_dual_children indent ">=" lhs rhs
    | NOT x -> 
        Printf.printf "%sNot\n" indent;
        print_jexpr x (indent ^ "  ")
    | Ident s -> 
        Printf.printf "%sIdent\n" indent;
        Printf.printf "%s%s\n" (indent ^ "  ") (jvar_to_string s);
    | String s -> Printf.printf "%sString \n%s%s\n" indent (indent ^ "  ") s
    | Integer d -> Printf.printf "%sInt %d\n" indent d
    | Float f -> Printf.printf "%sFloat %f\n" indent f
    | Bool b -> Printf.printf "%sBool %b\n" indent b

let rec print_jast_aux (ast : jast) (indent : string) =
    (match ast with
    | Toplevel tree ->
        Printf.printf "%sToplevel:\n" indent;
        List.iter (fun x -> print_jast_aux x (indent ^ "  ")) tree
    | ACSL _ -> failwith "ACSL clause in general AST"
    | Function (_, kind, args, stmts) -> (* of string * jvar * jvar list * jast list *)
        Printf.printf "%sFunction %s =\n" indent (jvar_to_string kind);
        Printf.printf "%sArgs:\n" (indent ^ "  ");
        List.iter (fun x -> Printf.printf "%s%s" (indent ^ "    ") (jvar_to_string x)) args;
        Printf.printf "\n%sStatements:\n" (indent ^ "  ");
        List.iter (fun x -> print_jast_aux x (indent ^ "    ")) stmts;
    | Assign (x, y) ->
        Printf.printf "%s%s =\n" indent (jvar_to_string x);
        print_jexpr y (indent ^ "  ")
    | If (x, y) ->
        Printf.printf "%sIf\n%sCondition\n" indent (indent ^ "  ");
        print_jexpr x (indent ^ "    ");
        Printf.printf "%sThen\n" (indent ^ "  ");
        List.iter (fun x -> print_jast_aux x (indent ^ "    ")) y
    | IfElse (x, y, z) ->
        Printf.printf "%sIf\n%sCondition\n" indent (indent ^ "  ");
        print_jexpr x (indent ^ "    ");
        Printf.printf "%sThen\n" (indent ^ "  ");
        List.iter (fun x' -> print_jast_aux x' (indent ^ "    ")) y;
        Printf.printf "%sElse\n" (indent ^ "  ");
        List.iter (fun x' -> print_jast_aux x' (indent ^ "    ")) z
    | Return (e) ->
        Printf.printf "%sReturn:\n" indent;
        print_jexpr e (indent ^ "  "))

let print_jast (ast : jast) = print_jast_aux ast ""

let print_verifiable_function (f : verifiable_function) = 
    Printf.printf "\n\n=== %s ===\n" f.title;
    List.iter (fun x -> 
        Printf.printf "Requires: "; 
        match x with 
        | (Requires y) -> print_endline (prop_to_string y)
        | _ -> print_endline "  error reading requires value.") 
        f.requires;
    List.iter (fun x -> 
        Printf.printf "Ensures: "; 
        match x with 
        | (Ensures y) -> print_endline (prop_to_string y)
        | _ -> print_endline "  error reading ensures value.") 
        f.ensures;
    print_jast f.fn

(* Print JuliaSyntax AST. *)
let rec print_jsast_node_aux (ast : julia_syntax_ast) (indent : string) =
    match ast with
    | JSExpr (head, args) ->
        Printf.printf "%sExpr:\n" indent;
        Printf.printf "%shead:\n" (indent ^ "  ");
        print_jsast_node_aux (head) (indent ^ "    ");
        Printf.printf "%sargs:\n" (indent ^ "  ");
        List.iter (fun x -> print_jsast_node_aux x (indent ^ "    ")) args
    | JSSymbol s -> Printf.printf "%sSymbol %s\n" indent s
    | JSString s -> Printf.printf "%sString %s\n" indent s
    | JSInt i -> Printf.printf "%sInt %d\n" indent i
    | JSFloat f -> Printf.printf "%sFloat %f\n" indent f
    | JSBool b -> Printf.printf "%sBool %b\n" indent b

let print_jsast_node (ast : julia_syntax_ast) = print_jsast_node_aux ast ""
