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

type acsl_expr =
    | ACSL_Add of acsl_expr * acsl_expr
    | ACSL_Subtract of acsl_expr * acsl_expr
    | ACSL_Multiply of acsl_expr * acsl_expr
    | ACSL_Divide of acsl_expr * acsl_expr
    | ACSL_Old of acsl_expr (* ACSL \old keyword *)
    | ACSL_Result (* ACSL \result keyword *)
    | ACSL_Ident of string
    | ACSL_String of string
    | ACSL_Int of int
    | ACSL_Float of float
    | ACSL_Boolean of bool

(* P -> WP(S, Q) *)
type prop =
    | Boolean of bool
    | EQ of acsl_expr * acsl_expr
    | NEQ of acsl_expr * acsl_expr
    | LT of acsl_expr * acsl_expr
    | LE of acsl_expr * acsl_expr
    | GT of acsl_expr * acsl_expr
    | GE of acsl_expr * acsl_expr
    | NOT of prop
    | AND of prop * prop
    | OR of prop * prop
    | IF of prop * prop
    | IFF of prop * prop

let rec acsl_expr_to_string (e : acsl_expr) : string = 
    match e with
    | ACSL_Add (lhs, rhs) ->
        Printf.sprintf "(%s + %s)" (acsl_expr_to_string lhs) (acsl_expr_to_string rhs)
    | ACSL_Subtract (lhs, rhs) ->
        Printf.sprintf "(%s - %s)" (acsl_expr_to_string lhs) (acsl_expr_to_string rhs)
    | ACSL_Multiply (lhs, rhs) ->
        Printf.sprintf "(%s * %s)" (acsl_expr_to_string lhs) (acsl_expr_to_string rhs)
    | ACSL_Divide (lhs, rhs) ->
        Printf.sprintf "(%s / %s)" (acsl_expr_to_string lhs) (acsl_expr_to_string rhs)
    | ACSL_Old x -> Printf.sprintf "\\old(%s)" (acsl_expr_to_string x)
    | ACSL_Result -> Printf.sprintf "\\result";
    | ACSL_Ident s | ACSL_String s -> s
    | ACSL_Int i -> Printf.sprintf "%d" i
    | ACSL_Float f -> Printf.sprintf "%f" f
    | ACSL_Boolean b -> Printf.sprintf "%b" b

let rec prop_to_string (p : prop) : string =
    match p with
    | Boolean b ->
        Printf.sprintf "%b" b
    | EQ (lhs, rhs) ->
        Printf.sprintf "%s = %s" (acsl_expr_to_string lhs) (acsl_expr_to_string rhs)
    | NEQ (lhs, rhs) ->
        Printf.sprintf "%s != %s" (acsl_expr_to_string lhs) (acsl_expr_to_string rhs)
    | LT (lhs, rhs) ->
        Printf.sprintf "%s < %s" (acsl_expr_to_string lhs) (acsl_expr_to_string rhs)
    | LE (lhs, rhs) ->
        Printf.sprintf "%s <= %s" (acsl_expr_to_string lhs) (acsl_expr_to_string rhs)
    | GT (lhs, rhs) ->
        Printf.sprintf "%s > %s" (acsl_expr_to_string lhs) (acsl_expr_to_string rhs)
    | GE (lhs, rhs) ->
        Printf.sprintf "%s >= %s" (acsl_expr_to_string lhs) (acsl_expr_to_string rhs)
    | NOT x ->
        Printf.sprintf "!%s" (prop_to_string x);
    | AND (lhs, rhs) ->
        Printf.sprintf "%s /\\ %s" (prop_to_string lhs) (prop_to_string rhs)
    | OR (lhs, rhs) ->
        Printf.sprintf "%s \\/ %s" (prop_to_string lhs) (prop_to_string rhs)
    | IF (lhs, rhs) ->
        Printf.sprintf "%s ==> %s" (prop_to_string lhs) (prop_to_string rhs)
    | IFF (lhs, rhs) ->
        Printf.sprintf "%s <==> %s" (prop_to_string lhs) (prop_to_string rhs)

let create_conjunction (a : prop) (b : prop) : prop = AND (a, b)

let negate_prop : prop -> prop = fun a -> NOT a