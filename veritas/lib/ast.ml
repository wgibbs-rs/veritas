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

type clause =
    | Requires of prop
    | Ensures of prop
    | Assigns of string list
    | LoopInvariant of prop
    | LoopVariant of prop
    | LoopAssigns of string list

type clause_record = {
    requires : clause list;
    ensures : clause list;
    assigns : clause list;
    loop_invariant : clause list;
    loop_variant : clause list;
    loop_assigns : clause list;
}

let empty_clause_record () : clause_record = {
    requires = [];
    ensures = [];
    assigns = [];
    loop_invariant = [];
    loop_variant = [];
    loop_assigns = [];
}

(* Stores Julia variables by their name. *)
type jvar =
    | JAny of string
    | JInt64 of string
    | JFloat64 of string
    | JString of string
    | JBool of string

type jexpr =
    | Add of jexpr * jexpr
    | Subtract of jexpr * jexpr
    | Multiply of jexpr * jexpr
    | Divide of jexpr * jexpr
    | EQ of jexpr * jexpr
    | NEQ of jexpr * jexpr
    | LT of jexpr * jexpr
    | LE of jexpr * jexpr
    | GT of jexpr * jexpr
    | GE of jexpr * jexpr
    | NOT of jexpr
    | Ident of jvar
    | String of string
    | Integer of int
    | Float of float
    | Bool of bool

type jast =
    | Toplevel of jast list
    | ACSL of clause
    | Function of string * jvar * jvar list * jast list 
    | If of prop * jast list
    | IfElse of prop * jast list * jast list
    | Assign of jvar * jexpr
    | Return of jexpr

type julia_syntax_ast =
    | JSExpr of julia_syntax_ast * julia_syntax_ast list
    | JSSymbol of string
    | JSString of string
    | JSInt of int
    | JSFloat of float
    | JSBool of bool

type verifiable_function = {
    requires: clause list; (* List of all ACSL requires clauses. *)
    ensures: clause list; (* List of all ACSL ensures clauses. *)
    context: jvar list; (* A list of context accessible outside of this function. *)
    title: string; (* The title of the function. *)
    fn: jast; (* The function as a jast node. *)
    early_unsat: bool; (* Whether the function was eliminated as unsat by quick eliminations. *)
}

let jvar_name : jvar -> string = function
    | JAny s
    | JInt64 s
    | JFloat64 s
    | JString s
    | JBool s
    -> s

let jvar_to_string : jvar -> string = function
    | JAny s -> Printf.sprintf "%s::Any" s 
    | JInt64 s -> Printf.sprintf "%s::Int64" s 
    | JFloat64 s -> Printf.sprintf "%s::Float64" s 
    | JString s -> Printf.sprintf "%s::String" s 
    | JBool s -> Printf.sprintf "%s::Bool" s 

let jvar_to_acsl_expr : jvar -> acsl_expr = fun x -> ACSL_Ident (jvar_name x)

let rec jexpr_to_acsl_expr : jexpr -> acsl_expr = function
    | Add (lhs, rhs) -> ACSL_Add (jexpr_to_acsl_expr lhs, jexpr_to_acsl_expr rhs)
    | Subtract (lhs, rhs) -> ACSL_Subtract (jexpr_to_acsl_expr lhs, jexpr_to_acsl_expr rhs)
    | Multiply (lhs, rhs) -> ACSL_Multiply (jexpr_to_acsl_expr lhs, jexpr_to_acsl_expr rhs)
    | Divide (lhs, rhs) -> ACSL_Divide (jexpr_to_acsl_expr lhs, jexpr_to_acsl_expr rhs)
    | Ident x -> jvar_to_acsl_expr x
    | String s -> ACSL_String s
    | Integer d -> ACSL_Int d
    | Float f -> ACSL_Float f
    | Bool b -> ACSL_Boolean b
    | _ -> Error.err "unexpected expr type in jexpr_to_acsl_expr"

let rec ast_to_expr (a : julia_syntax_ast) : acsl_expr =
    match a with
    | JSExpr (JSSymbol "call", [name; lhs; rhs]) ->
        (match name with
        | JSSymbol "+" -> Propositions.ACSL_Add (ast_to_expr lhs, ast_to_expr rhs)
        | JSSymbol "-" -> Propositions.ACSL_Subtract (ast_to_expr lhs, ast_to_expr rhs)
        | JSSymbol "*" -> Propositions.ACSL_Multiply (ast_to_expr lhs, ast_to_expr rhs)
        | JSSymbol "/" -> Propositions.ACSL_Divide (ast_to_expr lhs, ast_to_expr rhs)
        | _ -> Error.err "unknown call")
    | JSExpr (JSSymbol "call", [JSSymbol "__OLD__"; x]) -> Propositions.ACSL_Old (ast_to_expr x)
    | JSSymbol "__RESULT__" -> Propositions.ACSL_Result
    | JSSymbol s -> Propositions.ACSL_Ident s
    | JSString s -> Propositions.ACSL_String s
    | JSInt i -> Propositions.ACSL_Int i
    | JSFloat f -> Propositions.ACSL_Float f
    | JSBool b ->  Propositions.ACSL_Boolean b
    | _ -> 
        Error.err "unknown Expr.head that is NOT in the form Symbol \"Call\"."
    
let ast_to_prop : julia_syntax_ast -> prop = function
    | JSExpr (head, args) ->
        (match head, args with
        | JSSymbol "call", [op; lhs; rhs] ->
            (match op with
            | JSSymbol "==" -> Propositions.EQ (ast_to_expr lhs, ast_to_expr rhs)
            | JSSymbol "!=" -> Propositions.NEQ (ast_to_expr lhs, ast_to_expr rhs)
            | JSSymbol "<" -> Propositions.LT (ast_to_expr lhs, ast_to_expr rhs)
            | JSSymbol "<=" -> Propositions.LE (ast_to_expr lhs, ast_to_expr rhs)
            | JSSymbol ">" -> Propositions.GT (ast_to_expr lhs, ast_to_expr rhs)
            | JSSymbol ">=" -> Propositions.GE (ast_to_expr lhs, ast_to_expr rhs)
            | _ -> Error.err "bad operation structure")
        | _ -> Error.err "not a call")
    | _ -> Error.err "no ast"

(* Returns the highest-level IFF statement *)
let rec parse_symbol (s : string) (ast_list : julia_syntax_ast list) (lhs : julia_syntax_ast list) : prop option =
    match ast_list with
    | (JSSymbol s') :: t ->
        if s' = s then
            match lhs, t with
            | [], [] -> Error.err (Printf.sprintf "missing A,B in statement of type A %s B" s)
            | _, [] -> Error.err (Printf.sprintf "missing B in statement of type A %s B" s)
            | [], _ -> Error.err (Printf.sprintf "missing A in statement of type A %s B" s)
            | [h], [h'] ->
                (match s with
                | "__IFF__" -> Some (IFF (ast_to_prop h, ast_to_prop h'))
                | "__IMPLIES__" -> Some (IF (ast_to_prop h, ast_to_prop h'))
                | _ -> Error.err "unknown isolated symbol type.")
            | [h], l' ->
                (match s with
                | "__IFF__" -> Some (IFF (ast_to_prop h, parse_acsl_expr l'))
                | "__IMPLIES__" -> Some (IF (ast_to_prop h, parse_acsl_expr l'))
                | _ -> Error.err "unknown isolated symbol type.")
            | l, [h'] ->
                (match s with
                | "__IFF__" -> Some (IFF (parse_acsl_expr l, ast_to_prop h'))
                | "__IMPLIES__" -> Some (IF (parse_acsl_expr l, ast_to_prop h'))
                | _ -> Error.err "unknown isolated symbol type.")
            | l, l' ->
                (match s with
                | "__IFF__" -> Some (IFF (parse_acsl_expr l, parse_acsl_expr l'))
                | "__IMPLIES__" -> Some (IF (parse_acsl_expr l, parse_acsl_expr l'))
                | _ -> Error.err "unknown isolated symbol type.")
        else parse_symbol s t (lhs @ [JSSymbol s'])
    | h :: t -> parse_symbol s t (lhs @ [h])
    | _ -> None

and parse_acsl_expr (prop_expr_list : julia_syntax_ast list) : prop =
    match prop_expr_list with
    | [] -> Error.err "No ACSL proposition or compound proposition provided"
    | [h] -> ast_to_prop h
    | h ->
        match (parse_symbol "__IFF__" h []) with
        | Some p -> p
        | None ->
            match (parse_symbol "__IMPLIES__" h []) with
            | Some p' -> p'
            | None ->
                Error.err "malformed implication structure"

(* Builds a new ACSL clause from JuliaSyntax ast. *)
let build_clause : julia_syntax_ast list -> clause = function
    | JSSymbol "requires" :: prop_expr_list ->
        Requires (parse_acsl_expr prop_expr_list)
    | JSSymbol "ensures" :: prop_expr_list ->
        Ensures (parse_acsl_expr prop_expr_list)
    | JSSymbol "assigns" :: _prop_expr_list ->
        Error.err "`assigns` is not supported"
    | JSSymbol "loop" :: JSSymbol "invariant" :: prop_expr_list ->
        LoopInvariant (parse_acsl_expr prop_expr_list)
    | JSSymbol "loop" :: JSSymbol "variant" :: prop_expr_list ->
        LoopVariant (parse_acsl_expr prop_expr_list)
    | JSSymbol "loop" :: JSSymbol "assigns" :: _prop_expr_list ->
        Error.err "`loop assigns` is not supported."
    | _ -> Error.err "malformed clause structure"

let str_to_jvar (name : string) (kind : string) : jvar =
    match kind with
    | "Any" -> JAny name
    | "Int64" -> JInt64 name
    | "Float64" -> JFloat64 name
    | "String" -> JString name
    | "Bool" -> JBool name
    | _ -> Error.err ("unexpected type " ^ kind ^ " for function " ^ name ^ ".")

let julia_syntax_ast_to_jvar : julia_syntax_ast -> jvar = function
    | JSSymbol s -> JAny s
    | JSExpr (JSSymbol "::", [JSSymbol name; JSSymbol kind]) -> str_to_jvar name kind
    | _ -> Error.err "unexpected variable declaration syntax"

let rec julia_syntax_ast_to_jexpr : julia_syntax_ast -> jexpr = function
    | JSExpr (JSSymbol "call", [JSSymbol op; lhs; rhs]) ->
        (match op with
        | "+" -> Add (julia_syntax_ast_to_jexpr lhs, julia_syntax_ast_to_jexpr rhs)
        | "-" -> Subtract (julia_syntax_ast_to_jexpr lhs, julia_syntax_ast_to_jexpr rhs)
        | "*" -> Multiply (julia_syntax_ast_to_jexpr lhs, julia_syntax_ast_to_jexpr rhs)
        | "/" -> Divide (julia_syntax_ast_to_jexpr lhs, julia_syntax_ast_to_jexpr rhs)
        | "==" -> EQ (julia_syntax_ast_to_jexpr lhs, julia_syntax_ast_to_jexpr rhs)
        | "!=" -> NEQ (julia_syntax_ast_to_jexpr lhs, julia_syntax_ast_to_jexpr rhs)
        | "<" -> LT (julia_syntax_ast_to_jexpr lhs, julia_syntax_ast_to_jexpr rhs)
        | "<=" -> LE (julia_syntax_ast_to_jexpr lhs, julia_syntax_ast_to_jexpr rhs)
        | ">" -> GT (julia_syntax_ast_to_jexpr lhs, julia_syntax_ast_to_jexpr rhs)
        | ">=" -> GE (julia_syntax_ast_to_jexpr lhs, julia_syntax_ast_to_jexpr rhs)
        | _ -> Error.err ("unknown operation: " ^ op ^ "."))
    | JSSymbol s -> Ident (JAny s)
    | JSInt d -> Integer d
    | JSBool b -> Bool b
    | _ -> Error.err "unexpected JuliaSyntax expr structure"

let rec julia_syntax_ast_to_jast : julia_syntax_ast -> jast = function
    | JSExpr (JSSymbol "toplevel", args) ->
        Toplevel (List.map (julia_syntax_ast_to_jast) args)
    | JSExpr (JSSymbol "macrocall", JSSymbol "@ACSL" :: args) ->
        ACSL (build_clause args)
    | JSExpr (JSSymbol "function", [call; block]) ->
        (* Handle all function declarations + definitions *)
        (match call with
        | JSExpr (JSSymbol "call", fn_name :: args) ->
            (* Used for functions without a stated type. *)
            let fn_name_str =
                (match fn_name with
                | JSSymbol s -> s
                | _ -> Error.err "bad function name structure") in
            Function (
                fn_name_str,
                JAny fn_name_str,
                List.map (fun x -> julia_syntax_ast_to_jvar x) args,
                match block with
                | JSExpr (JSSymbol "block", args) ->
                    List.map (julia_syntax_ast_to_jast) args
                | _ -> Error.err "unhandled function structure; bad block."
            )
        | JSExpr (JSSymbol "::", [JSExpr (JSSymbol "call", fn_name :: args); JSSymbol kind]) ->
            (* Used for functions WITH a stated type. *)
            let fn_name_str = 
                (match fn_name with
                | JSSymbol s -> s
                | _ -> Error.err "bad function name structure") in
            Function (
                fn_name_str,
                str_to_jvar fn_name_str kind,
                List.map (fun x -> julia_syntax_ast_to_jvar x) args,
                (match block with
                | JSExpr (JSSymbol "block", args) ->
                    List.map (julia_syntax_ast_to_jast) args
                | _ -> Error.err "unhandled function structure; bad block.")
            )
        | _ -> Error.err "unexpected function structure; bad call")
    | JSExpr (JSSymbol "=", [lhs; rhs]) ->
        Assign (julia_syntax_ast_to_jvar lhs, julia_syntax_ast_to_jexpr rhs)
    | JSExpr (JSSymbol "if", 
        [condition; JSExpr (JSSymbol "block", stmts)]) ->
            If (ast_to_prop condition, List.map (julia_syntax_ast_to_jast) stmts)
    | JSExpr (JSSymbol "if",
        [condition; 
        JSExpr (JSSymbol "block", then_stmts); 
        JSExpr (JSSymbol "block", else_stmts)]) ->
            IfElse (
                ast_to_prop condition, 
                List.map (julia_syntax_ast_to_jast) then_stmts, 
                List.map (julia_syntax_ast_to_jast) else_stmts )
    | JSExpr (JSSymbol "return", [e]) ->
        Return (julia_syntax_ast_to_jexpr e)
    | _ -> Error.err "unknown JuliaSyntax AST structure, or used disallowed Julia feature(s)."

let rec ast_of_json (json : Yojson.Safe.t) =
    match json with
    | `Assoc fields ->
        let head =
            match List.assoc_opt "head" fields with
            | Some head_json -> ast_of_json head_json
            | None -> Error.err "Expected field 'head'"
        in
        let args =
            match List.assoc_opt "args" fields with
            | Some (`List xs) -> List.map ast_of_json xs
            | _ -> Error.err "Expected list field 'args'"
        in
        JSExpr (head, args)
    | `String s -> JSSymbol s
    | `Int i -> JSInt i
    | `Float f -> JSFloat f
    | `Bool b -> JSBool b
    | _ -> Error.err "Unexpected JSON value"

let validate_syntax filename =
    let ic = Unix.open_process_args_in
        "julia" [| "julia"; "-e"; Bridge.validate_src; filename |] in
    let output = In_channel.input_all ic in
    if (Unix.close_process_in ic) <> Unix.WEXITED 0 then
        print_string output

let get_ast filename : jast =
    julia_syntax_ast_to_jast (
        ast_of_json (
            Yojson.Safe.from_string (
                let ic = Unix.open_process_args_in
                    "julia" [| "julia"; "-e"; Bridge.parser_src; filename; "n" |] in
                let output = In_channel.input_all ic in
                let _ = Unix.close_process_in ic in
                output )))

let rec get_verifiable_functions_from_block (stmt_list : jast list) (context : jvar list) (clauses : clause_record) : verifiable_function list =
    match stmt_list with
    | h :: t ->
        (match h with
        | ACSL c ->
            (match c with
            | Requires _ -> get_verifiable_functions_from_block t context {clauses with requires = clauses.requires @ [c]}
            | Ensures _ -> get_verifiable_functions_from_block t context {clauses with ensures = clauses.ensures @ [c]}
            | Assigns _ -> get_verifiable_functions_from_block t context {clauses with assigns = clauses.assigns @ [c]}
            | LoopInvariant _ -> get_verifiable_functions_from_block t context {clauses with loop_invariant = clauses.loop_invariant @ [c]}
            | LoopVariant _ -> get_verifiable_functions_from_block t context {clauses with loop_variant = clauses.loop_variant @ [c]}
            | LoopAssigns _ -> get_verifiable_functions_from_block t context {clauses with loop_assigns = clauses.loop_assigns @ [c]})
        | Function (_, kind, args, _stmts) ->
            let new_verifiable_function = (
                match h with
                | Function (name, kind, args, stmts) ->
                    {requires = clauses.requires; ensures = clauses.ensures; context = (context @ kind :: args); title = name; fn = h; early_unsat = false} ::
                    (get_verifiable_functions_from_block stmts (context @ kind :: args) (empty_clause_record ()))
                | _ -> Error.err "not of function in add_new_verifiable_function"
            ) in
            new_verifiable_function @
            get_verifiable_functions_from_block t (context @ kind :: args) (empty_clause_record ())
        | _ -> [])
    | _ -> []

(* Retrieve all functions in the program that need to be verified. *)
let get_fn_list_of_program (ast' : jast) : verifiable_function list = 
    match ast' with
    | Toplevel stmts -> get_verifiable_functions_from_block stmts [] (empty_clause_record ())
    | _ -> []