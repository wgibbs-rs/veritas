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
    | Old of expr (* ACSL \old keyword *)
    | Result (* ACSL \result keyword *)
    | Ident of string
    | String of string
    | Int of int
    | Float of float
    | Boolean of bool

(* P -> WP(S, Q) *)
type prop =
    | Boolean of bool
    | EQ of expr * expr
    | NEQ of expr * expr
    | LT of expr * expr
    | LE of expr * expr
    | GT of expr * expr
    | GE of expr * expr
    | NOT of prop
    | AND of prop * prop
    | OR of prop * prop
    | IF of prop * prop
    | IFF of prop * prop

let rec expr_to_string (e : expr) : string = 
    match e with
    | Add (lhs, rhs) ->
        Printf.sprintf "(%s + %s)" (expr_to_string lhs) (expr_to_string rhs)
    | Subtract (lhs, rhs) ->
        Printf.sprintf "(%s - %s)" (expr_to_string lhs) (expr_to_string rhs)
    | Multiply (lhs, rhs) ->
        Printf.sprintf "(%s * %s)" (expr_to_string lhs) (expr_to_string rhs)
    | Divide (lhs, rhs) ->
        Printf.sprintf "(%s / %s)" (expr_to_string lhs) (expr_to_string rhs)
    | Old x ->
        Printf.sprintf "\\old(%s)" (expr_to_string x)
    | Result ->
        Printf.sprintf "\\result";
    | Ident s -> s
    | String s -> s
    | Int i -> Printf.sprintf "%d" i
    | Float f -> Printf.sprintf "%f" f
    | Boolean b -> Printf.sprintf "%b" b

let rec prop_to_string (p : prop) : string =
    match p with
    | Boolean b ->
        Printf.sprintf "%b" b
    | EQ (lhs, rhs) ->
        Printf.sprintf "%s = %s" (expr_to_string lhs) (expr_to_string rhs)
    | NEQ (lhs, rhs) ->
        Printf.sprintf "%s != %s" (expr_to_string lhs) (expr_to_string rhs)
    | LT (lhs, rhs) ->
        Printf.sprintf "%s < %s" (expr_to_string lhs) (expr_to_string rhs)
    | LE (lhs, rhs) ->
        Printf.sprintf "%s <= %s" (expr_to_string lhs) (expr_to_string rhs)
    | GT (lhs, rhs) ->
        Printf.sprintf "%s > %s" (expr_to_string lhs) (expr_to_string rhs)
    | GE (lhs, rhs) ->
        Printf.sprintf "%s >= %s" (expr_to_string lhs) (expr_to_string rhs)
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