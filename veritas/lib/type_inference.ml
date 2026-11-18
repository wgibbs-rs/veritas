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

open Ast

(*
Attempts to infer the type of a Julia expression. 
*)
let rec try_infer_jexpr_type : jexpr -> jvar option = function
    | Add (lhs, rhs)
    | Subtract (lhs, rhs)
    | Multiply (lhs, rhs) ->
        (match (try_infer_jexpr_type lhs), (try_infer_jexpr_type rhs) with
        | Some JAny _, _
        | _, Some JAny _ -> None
        | Some JInt64 _, Some JInt64 _ -> Some (JInt64 "")
        | Some JInt64 _, Some JFloat64 _
        | Some JFloat64 _, Some JInt64 _
        | Some JFloat64 _, Some JFloat64 _ -> Some (JFloat64 "")
        | _ -> None)
    | Divide (lhs, rhs) -> 
        (match (try_infer_jexpr_type lhs), (try_infer_jexpr_type rhs) with
        | Some JAny _, _
        | _, Some JAny _ -> None
        | Some JInt64 _, Some JInt64 _
        | Some JInt64 _, Some JFloat64 _
        | Some JFloat64 _, Some JInt64 _
        | Some JFloat64 _, Some JFloat64 _ -> Some (JFloat64 "")
        | _ -> None)
    | EQ (lhs, rhs)
    | NEQ (lhs, rhs)
    | LT (lhs, rhs)
    | LE (lhs, rhs)
    | GT (lhs, rhs)
    | GE (lhs, rhs) -> 
        (match (try_infer_jexpr_type lhs),(try_infer_jexpr_type rhs) with
            | Some JAny _, _
            | _, Some JAny _ -> None
            | Some JInt64 _, Some JInt64 _
            | Some JInt64 _, Some JFloat64 _
            | Some JFloat64 _, Some JInt64 _
            | Some JFloat64 _, Some JFloat64 _ -> Some (JBool "")
            | Some JString _, _
            | _, Some JString _ -> None
            | _ -> None)
    | NOT e' ->
        (match (try_infer_jexpr_type e') with
        | Some JBool _ -> Some (JBool "")
        | _ -> None)
    | Ident v -> Some v
    | String _ -> Some (JString "")
    | Integer _ -> Some (JInt64 "")
    | Float _ -> Some (JFloat64 "")
    | Bool _ -> Some (JBool "")

(* 
Attempts to infer the type that a function returns given its statement body.
*)
let try_infer_function_type : jast -> jvar option = function
    | Function (_, kind, _args, _stmts) ->
        (match kind with
        | JAny _s -> None (* TODO *)
        | _ -> Some kind)
    | _ -> None