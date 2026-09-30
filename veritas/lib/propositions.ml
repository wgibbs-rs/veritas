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


type expr =
    | Add of expr * expr
    | Subtract of expr * expr
    | Multiply of expr * expr
    | Divide of expr * expr
    | Ident of string
    | String of string
    | Int of int
    | Float of float
    | Bool of bool

(* P -> WP(S, Q) *)
type prop =
    | EQ of expr * expr
    | NEQ of expr * expr
    | LT of expr * expr
    | LE of expr * expr
    | GT of expr * expr
    | GE of expr * expr

type vc =
    | VC of prop list * prop

let rec print_expr (e : expr) (indent : string) = 
    match e with
    | Add (lhs, rhs) ->
        Printf.printf "%s+\n" indent;
        print_expr lhs (indent ^ "  ");
        print_expr rhs (indent ^ "  ")
    | Subtract (lhs, rhs) ->
        Printf.printf "%s+\n" indent;
        print_expr lhs (indent ^ "  ");
        print_expr rhs (indent ^ "  ")
    | Multiply (lhs, rhs) ->
        Printf.printf "%s+\n" indent;
        print_expr lhs (indent ^ "  ");
        print_expr rhs (indent ^ "  ")
    | Divide (lhs, rhs) ->
        Printf.printf "%s+\n" indent;
        print_expr lhs (indent ^ "  ");
        print_expr rhs (indent ^ "  ")
    | Ident s ->
        Printf.printf "%s%s\n" indent s
    | String s ->
        Printf.printf "%s%s\n" indent s
    | Int i ->
        Printf.printf "%s%d\n" indent i
    | Float f ->
        Printf.printf "%s%f\n" indent f
    | Bool b ->
        Printf.printf "%s%b\n" indent b

let print_proposition = function
    | EQ (lhs, rhs) ->
        print_endline "=";
        print_expr lhs "  ";
        print_expr rhs "  "
    | NEQ (lhs, rhs) ->
        print_endline "!=";
        print_expr lhs "  ";
        print_expr rhs "  "
    | LT (lhs, rhs) ->
        print_endline "<";
        print_expr lhs "  ";
        print_expr rhs "  "
    | LE (lhs, rhs) ->
        print_endline "<=";
        print_expr lhs "  ";
        print_expr rhs "  "
    | GT (lhs, rhs) ->
        print_endline ">";
        print_expr lhs "  ";
        print_expr rhs "  "
    | GE (lhs, rhs) ->
        print_endline ">=";
        print_expr lhs "  ";
        print_expr rhs "  "