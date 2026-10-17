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

let rec try_infer_jexpr_type : jexpr -> jvar = function
    | Add (lhs, rhs)
    | Subtract (lhs, rhs)
    | Multiply (lhs, rhs) ->
        (match (try_infer_jexpr_type lhs), (try_infer_jexpr_type rhs) with
        | JAny _, _
        | _, JAny _ -> 
            Error.err "unable to verify expressions with type Any."
        | JInt64 _, JInt64 _ -> JInt64 ""
        | JInt64 _, JFloat64 _
        | JFloat64 _, JInt64 _
        | JFloat64 _, JFloat64 _ -> JFloat64 ""
        | _ -> Error.err "unable to operate with these types.")
    | Divide (lhs, rhs) -> 
        (match (try_infer_jexpr_type lhs), (try_infer_jexpr_type rhs) with
        | JAny _, _
        | _, JAny _ -> 
            Error.err "unable to verify expressions with type Any."
        | JInt64 _, JInt64 _
        | JInt64 _, JFloat64 _
        | JFloat64 _, JInt64 _
        | JFloat64 _, JFloat64 _ -> JFloat64 ""
        | _ -> Error.err "unable to operate with these types.")
    | EQ (lhs, rhs) -> 
        (match (try_infer_jexpr_type lhs), (try_infer_jexpr_type rhs) with
        | JAny _, _
        | _, JAny _ -> 
            Error.err "unable to verify expressions with type Any."
        | JInt64 _, JInt64 _
        | JInt64 _, JFloat64 _
        | JFloat64 _, JInt64 _
        | JFloat64 _, JFloat64 _
        | JBool _, JBool _ -> JBool ""
        | _ -> Error.err "unable to operate with these types.")
    | NEQ (lhs, rhs) -> 
        (match (try_infer_jexpr_type lhs), (try_infer_jexpr_type rhs) with
        | JAny _, _
        | _, JAny _ -> 
            Error.err "unable to verify expressions with type Any."
        | JInt64 _, JInt64 _
        | JInt64 _, JFloat64 _
        | JFloat64 _, JInt64 _
        | JFloat64 _, JFloat64 _
        | JBool _, JBool _ -> JBool ""
        | _ -> Error.err "unable to operate with these types.")
    | LT (lhs, rhs)
    | LE (lhs, rhs)
    | GT (lhs, rhs)
    | GE (lhs, rhs) -> 
        (match (try_infer_jexpr_type lhs),(try_infer_jexpr_type rhs) with
            | JAny _, _
            | _, JAny _ -> 
                Error.err "unable to verify expressions with type Any."
            | JInt64 _, JInt64 _
            | JInt64 _, JFloat64 _
            | JFloat64 _, JInt64 _
            | JFloat64 _, JFloat64 _ -> JBool ""
            | JString _, _
            | _, JString _ -> 
                Error.err "string comparison is not supported."
            | _ -> Error.err "unable to compare these types.")
    | NOT e' ->
        (match (try_infer_jexpr_type e') with
        | JBool _ -> JBool ""
        | _ -> Error.err "cannot negate a non-boolean value.")
    | Ident v -> v
    | String _ -> JString ""
    | Integer _ -> JInt64 ""
    | Float _ -> JFloat64 ""
    | Bool _ -> JBool ""

let get_all_return_types : jast list -> jvar list = function
    | [] -> []
    | _h :: _t -> Error.err "TODO"
      (* h :: (get_all_return_types :: t) *)

let try_infer_function_type : jast -> jvar = function
    | Function (_, kind, _args, _stmts) ->
        (match kind with
        | JAny _s -> Error.err "TODO"
        | _ -> kind)
    | _ -> Error.err "cannot infer function type of non-function node"