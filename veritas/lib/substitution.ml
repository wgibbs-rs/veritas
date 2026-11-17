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
    | NOTP x' -> NOTP (replace_all x' x y)
    | AND (lhs, rhs) -> AND (replace_all lhs x y, replace_all rhs x y)
    | OR (lhs, rhs) -> OR (replace_all lhs x y, replace_all rhs x y)
    | IF (lhs, rhs) -> IF (replace_all lhs x y, replace_all rhs x y)
    | IFF (lhs, rhs) -> IFF (replace_all lhs x y, replace_all rhs x y)
