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
type clause =
    | Requires of prop
    | Ensures of prop

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
    title: string;
    fn: jast;
}

let rec print_jsast_node_aux (ast : julia_syntax_ast) (indent : string) =
    match ast with
    | JSExpr (head, args) ->
        Printf.printf "%sExpr:\n" indent;
        Printf.printf "%shead:\n" (indent ^ "  ");
        print_jsast_node_aux (head) (indent ^ "    ");
        Printf.printf "%sargs:\n" (indent ^ "  ");
        List.iter (fun x -> print_jsast_node_aux x (indent ^ "    ")) args
    | JSSymbol s ->
        Printf.printf "%sSymbol %s\n" indent s
    | JSString s ->
        Printf.printf "%sString %s\n" indent s
    | JSInt i ->
        Printf.printf "%sInt %d\n" indent i
    | JSFloat f ->
        Printf.printf "%sFloat %f\n" indent f
    | JSBool b ->
        Printf.printf "%sBool %b\n" indent b

let print_jsast_node (ast : julia_syntax_ast) = print_jsast_node_aux ast ""

let jvar_to_acsl_expr : jvar -> acsl_expr = function
    | JAny s -> ACSL_Ident s
    | JInt64 s -> ACSL_Ident s
    | JFloat64 s -> ACSL_Ident s
    | JString s -> ACSL_Ident s
    | JBool s -> ACSL_Ident s

let rec jexpr_to_acsl_expr : jexpr -> acsl_expr = function
    | Add (lhs, rhs) -> ACSL_Add (jexpr_to_acsl_expr lhs, jexpr_to_acsl_expr rhs)
    | Subtract (lhs, rhs) -> ACSL_Subtract (jexpr_to_acsl_expr lhs, jexpr_to_acsl_expr rhs)
    | Multiply (lhs, rhs) -> ACSL_Multiply (jexpr_to_acsl_expr lhs, jexpr_to_acsl_expr rhs)
    | Divide (lhs, rhs) -> ACSL_Divide (jexpr_to_acsl_expr lhs, jexpr_to_acsl_expr rhs)
    | Ident x ->
        (match x with
        | JAny s -> ACSL_Ident s
        | JInt64 s -> ACSL_Ident s
        | JFloat64 s -> ACSL_Ident s
        | JString s -> ACSL_Ident s
        | JBool s -> ACSL_Ident s)
    | String s -> ACSL_String s
    | Integer d -> ACSL_Int d
    | Float f -> ACSL_Float f
    | Bool b -> ACSL_Boolean b
    | _ -> failwith "unexpected expr type in jexpr_to_acsl_expr"

let jsexpr_to_jvar (a : julia_syntax_ast) (name : string) : jvar =
    match a with
    | JSExpr (JSSymbol "call", [JSSymbol kind; _lhs; _rhs]) ->
        (match kind with
        | "+" | "-" | "*" -> JInt64 name
        | "/" -> JFloat64 name
        | _ -> failwith "unknown assignment call rhs type")
    | JSSymbol _s -> failwith "currently unable to assign to a symbol"
    | JSString _s -> JString name
    | JSInt _d -> JInt64 name
    | JSFloat _f -> JFloat64 name
    | JSBool _b -> JBool name
    | _ -> failwith "unknown expression in assignment"

let ast_to_julia_variable (a : julia_syntax_ast) : jvar = 
    match a with
    (* Just a symbol by itself, which we assume is Any *)
    | JSSymbol x -> JAny x
    (* An assignment of the form x = y *)
    | JSExpr (JSSymbol "=", [JSSymbol x; args]) ->
        (* This is a standard new variable declaration/definition. *)
        (match args with
        | JSExpr (JSSymbol "call", _args) -> jsexpr_to_jvar args x
        | _ -> failwith "Veritas: Error: unidentified assignment type")
    (* A symbol with a type annotation, x::T . *)
    | JSExpr (JSSymbol "::", [JSSymbol x; JSSymbol kind]) ->
        (match kind with
        | "String" -> JString x
        | "Int64" -> JInt64 x
        | "Float64" -> JFloat64 x
        | "Bool" -> JBool x
        | _ -> failwith ("unknown assigned type \"" ^ kind ^ "\""))
    (* Anything that is not supported. *)
    | _ -> failwith "unknown variable declaration"

let rec ast_to_expr (a : julia_syntax_ast) : acsl_expr =
    match a with
    | JSExpr (JSSymbol "call", [name; lhs; rhs]) ->
        (match name with
        | JSSymbol "+" -> Propositions.ACSL_Add (ast_to_expr lhs, ast_to_expr rhs)
        | JSSymbol "-" -> Propositions.ACSL_Subtract (ast_to_expr lhs, ast_to_expr rhs)
        | JSSymbol "*" -> Propositions.ACSL_Multiply (ast_to_expr lhs, ast_to_expr rhs)
        | JSSymbol "/" -> Propositions.ACSL_Divide (ast_to_expr lhs, ast_to_expr rhs)
        | _ -> failwith "unknown call")
    | JSExpr (JSSymbol "call", [JSSymbol "__OLD__"; x]) -> Propositions.ACSL_Old (ast_to_expr x)
    | JSSymbol "__RESULT__" -> Propositions.ACSL_Result
    | JSSymbol s -> Propositions.ACSL_Ident s
    | JSString s -> Propositions.ACSL_String s
    | JSInt i -> Propositions.ACSL_Int i
    | JSFloat f -> Propositions.ACSL_Float f
    | JSBool b ->  Propositions.ACSL_Boolean b
    | _ -> 
        failwith "unknown Expr.head that is NOT in the form Symbol \"Call\"."
    
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
            | _ -> failwith "bad operation structure")
        | _ -> failwith "not a call")
    | _ -> failwith "no ast"

(* Returns the highest-level IFF statement *)
let rec parse_symbol (s : string) (ast_list : julia_syntax_ast list) (lhs : julia_syntax_ast list) : prop option =
    match ast_list with
    | (JSSymbol s') :: t -> 
        if s' = s then
            match lhs, t with
            | [], [] -> failwith (Printf.sprintf "missing A,B in statement of type A %s B" s)
            | _, [] -> failwith (Printf.sprintf "missing B in statement of type A %s B" s)
            | [], _ -> failwith (Printf.sprintf "missing A in statement of type A %s B" s)
            | [h], [h'] -> 
                (match s with
                | "__IFF__" -> Some (IFF (ast_to_prop h, ast_to_prop h'))
                | "__IMPLIES__" -> Some (IF (ast_to_prop h, ast_to_prop h'))
                | _ -> failwith "unknown isolated symbol type.")
            | [h], l' -> 
                (match s with
                | "__IFF__" -> Some (IFF (ast_to_prop h, parse_acsl_expr l'))
                | "__IMPLIES__" -> Some (IF (ast_to_prop h, parse_acsl_expr l'))
                | _ -> failwith "unknown isolated symbol type.")
            | l, [h'] -> 
                (match s with
                | "__IFF__" -> Some (IFF (parse_acsl_expr l, ast_to_prop h'))
                | "__IMPLIES__" -> Some (IF (parse_acsl_expr l, ast_to_prop h'))
                | _ -> failwith "unknown isolated symbol type.")
            | l, l' -> 
                (match s with
                | "__IFF__" -> Some (IFF (parse_acsl_expr l, parse_acsl_expr l'))
                | "__IMPLIES__" -> Some (IF (parse_acsl_expr l, parse_acsl_expr l'))
                | _ -> failwith "unknown isolated symbol type.")
        else parse_symbol s t (lhs @ [JSSymbol s'])
    | h :: t -> parse_symbol s t (lhs @ [h])
    | _ -> None

and parse_acsl_expr (prop_expr_list : julia_syntax_ast list) : prop =
    match prop_expr_list with
    | [] -> failwith "No ACSL proposition or compound proposition provided"
    | [h] -> ast_to_prop h
    | h -> 
        match (parse_symbol "__IFF__" h []) with
        | Some p -> p
        | None -> 
            match (parse_symbol "__IMPLIES__" h []) with
            | Some p' -> p'
            | None ->
                failwith "Single AST Node ?"

let build_clause : julia_syntax_ast list -> clause = function
    | JSSymbol "requires" :: prop_expr_list ->
        Requires (parse_acsl_expr prop_expr_list)
    | JSSymbol "ensures" :: prop_expr_list ->
        Ensures (parse_acsl_expr prop_expr_list)
    | _ -> failwith "malformed clause structure"

let str_to_jvar (name : string) (kind : string) : jvar =
    match kind with
    | "Any" -> JAny name
    | "Int64" -> JInt64 name
    | "Float64" -> JFloat64 name
    | "String" -> JString name
    | "Bool" -> JBool name
    | _ -> failwith ("unexpected type " ^ kind ^ " for function " ^ name ^ ".")

let jsast_to_jvar : julia_syntax_ast -> jvar = function
    | JSSymbol s -> JAny s
    | JSExpr (JSSymbol "::", [JSSymbol name; JSSymbol kind]) -> str_to_jvar name kind
    | _ -> failwith "unexpected variable declaration syntax"

let rec jsast_to_jexpr : julia_syntax_ast -> jexpr = function
    | JSExpr (JSSymbol "call", [JSSymbol op; lhs; rhs]) ->
        (match op with
        | "+" -> Add (jsast_to_jexpr lhs, jsast_to_jexpr rhs)
        | "-" -> Subtract (jsast_to_jexpr lhs, jsast_to_jexpr rhs)
        | "*" -> Multiply (jsast_to_jexpr lhs, jsast_to_jexpr rhs)
        | "/" -> Divide (jsast_to_jexpr lhs, jsast_to_jexpr rhs)
        | "==" -> EQ (jsast_to_jexpr lhs, jsast_to_jexpr rhs)
        | "!=" -> NEQ (jsast_to_jexpr lhs, jsast_to_jexpr rhs)
        | "<" -> LT (jsast_to_jexpr lhs, jsast_to_jexpr rhs)
        | "<=" -> LE (jsast_to_jexpr lhs, jsast_to_jexpr rhs)
        | ">" -> GT (jsast_to_jexpr lhs, jsast_to_jexpr rhs)
        | ">=" -> GE (jsast_to_jexpr lhs, jsast_to_jexpr rhs)
        | _ -> failwith ("unknown operation: " ^ op ^ "."))
    | JSSymbol s -> Ident (JAny s)
    | JSInt d -> Integer d
    | _ -> failwith "unexpected JuliaSyntax expr structure"

let rec jsast_to_jast : julia_syntax_ast -> jast = function
    | JSExpr (JSSymbol "toplevel", args) ->
        Toplevel (List.map (jsast_to_jast) args)
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
                | _ -> failwith "bad function name structure") in
            Function (
                fn_name_str,
                JAny fn_name_str,
                List.map (fun x -> jsast_to_jvar x) args,
                match block with
                | JSExpr (JSSymbol "block", args) ->
                    List.map (jsast_to_jast) args
                | _ -> failwith "unhandled function structure; bad block."
            )
        | JSExpr (JSSymbol "::", [JSExpr (JSSymbol "call", fn_name :: args); JSSymbol kind]) ->
            (* Used for functions WITH a stated type. *)
            let fn_name_str = 
                (match fn_name with
                | JSSymbol s -> s
                | _ -> failwith "bad function name structure") in
            Function (
                fn_name_str,
                str_to_jvar fn_name_str kind,
                List.map (fun x -> jsast_to_jvar x) args,
                (match block with
                | JSExpr (JSSymbol "block", args) ->
                    List.map (jsast_to_jast) args
                | _ -> failwith "unhandled function structure; bad block.")
            )
        | _ -> failwith "unexpected function structure; bad call")
    | JSExpr (JSSymbol "=", [lhs; rhs]) ->
        Assign (jsast_to_jvar lhs, jsast_to_jexpr rhs)
    | JSExpr (JSSymbol "return", [e]) ->
        Return (jsast_to_jexpr e)
    | _ -> failwith "unknown JuliaSyntax AST structure, or used disallowed Julia feature(s)."

let rec ast_of_json (json : Yojson.Safe.t) =
    match json with
    | `Assoc fields ->
        let head =
            match List.assoc_opt "head" fields with
            | Some head_json -> ast_of_json head_json
            | None -> failwith "Expected field 'head'"
        in
        let args =
            match List.assoc_opt "args" fields with
            | Some (`List xs) -> List.map ast_of_json xs
            | _ -> failwith "Expected list field 'args'"
        in
        JSExpr (head, args)
    | `String s -> JSSymbol s
    | `Int i -> JSInt i
    | `Float f -> JSFloat f
    | `Bool b -> JSBool b
    | _ -> failwith "Unexpected JSON value"

let validate_syntax filename =
    let ic = Unix.open_process_args_in
        "julia" [| "julia"; "-e"; Bridge.validate_src; filename |] in
    let output = In_channel.input_all ic in
    let result = Unix.close_process_in ic in
    if result <> Unix.WEXITED 0 then
        print_string output

let get_json_ast filename =
    let ic = Unix.open_process_args_in
        "julia" [| "julia"; "-e"; Bridge.parser_src; filename; "n" |] in
    let output = In_channel.input_all ic in
    let _ = Unix.close_process_in ic in
    output
    
let get_ast filename : jast =
    let json_str : string = get_json_ast filename in
    let jsast = ast_of_json (Yojson.Safe.from_string json_str) in
    print_jsast_node jsast;
    jsast_to_jast jsast

let jvar_to_string : jvar -> string = function
    | JAny s -> s
    | JInt64 s -> s
    | JFloat64 s -> s
    | JString s -> s
    | JBool s -> s

let rec print_jexpr (expr : jexpr) (indent : string) =
    match expr with
    | Add (lhs, rhs) -> 
        Printf.printf "%s+\n" indent;
        print_jexpr lhs (indent ^ "  ");
        print_jexpr rhs (indent ^ "  ")
    | Subtract (lhs, rhs) -> 
        Printf.printf "%s-\n" indent;
        print_jexpr lhs (indent ^ "  ");
        print_jexpr rhs (indent ^ "  ")
    | Multiply (lhs, rhs) -> 
        Printf.printf "%s*\n" indent;
        print_jexpr lhs (indent ^ "  ");
        print_jexpr rhs (indent ^ "  ")
    | Divide (lhs, rhs) -> 
        Printf.printf "%s/\n" indent;
        print_jexpr lhs (indent ^ "  ");
        print_jexpr rhs (indent ^ "  ")
    | EQ (lhs, rhs) -> 
        Printf.printf "%s==\n" indent;
        print_jexpr lhs (indent ^ "  ");
        print_jexpr rhs (indent ^ "  ")
    | NEQ (lhs, rhs) -> 
        Printf.printf "%s!=\n" indent;
        print_jexpr lhs (indent ^ "  ");
        print_jexpr rhs (indent ^ "  ")
    | LT (lhs, rhs) -> 
        Printf.printf "%s<\n" indent;
        print_jexpr lhs (indent ^ "  ");
        print_jexpr rhs (indent ^ "  ")
    | LE (lhs, rhs) -> 
        Printf.printf "%s<=\n" indent;
        print_jexpr lhs (indent ^ "  ");
        print_jexpr rhs (indent ^ "  ")
    | GT (lhs, rhs) -> 
        Printf.printf "%s>\n" indent;
        print_jexpr lhs (indent ^ "  ");
        print_jexpr rhs (indent ^ "  ")
    | GE (lhs, rhs) -> 
        Printf.printf "%s>=\n" indent;
        print_jexpr lhs (indent ^ "  ");
        print_jexpr rhs (indent ^ "  ")
    | NOT x -> 
        Printf.printf "%sNot\n" indent;
        print_jexpr x (indent ^ "  ")
    | Ident s -> 
        Printf.printf "%sIdent\n" indent;
        Printf.printf "%s%s\n" (indent ^ "  ") (jvar_to_string s);
    | String s ->
        Printf.printf "%sString \n%s%s\n" indent (indent ^ "  ") s
    | Integer d ->
        Printf.printf "%sInt %d\n" indent d
    | Float f ->
        Printf.printf "%sFloat %f\n" indent f
    | Bool b ->
        Printf.printf "%sBool %b\n" indent b

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
    | Return (e) ->
        Printf.printf "%sReturn:\n" indent;
        print_jexpr e (indent ^ "  "))

let print_jast (ast : jast) = print_jast_aux ast ""

let print_verifiable_function (f : verifiable_function) = 
    Printf.printf "\n\n=== %s ===\n" f.title;
    List.iter (fun x -> 
        Printf.printf "Requires:\n"; 
        match x with 
        | (Requires y) -> print_endline (prop_to_string y)
        | _ -> print_endline "  error reading requires value.") 
        f.requires;
    List.iter (fun x -> 
        Printf.printf "Ensures:\n"; 
        match x with 
        | (Ensures y) -> print_endline (prop_to_string y)
        | _ -> print_endline "  error reading ensures value.") 
        f.ensures;
    print_jast f.fn

let rec get_fn_list_of_block_list (stmt_list : jast list) (context : jvar list) (requires : clause list) (ensures : clause list) : (verifiable_function list) =
    match stmt_list with
    | h :: t ->
        (match h with
        | ACSL c ->
            (match c with
            | Requires _ ->
                get_fn_list_of_block_list t context (requires @ [c]) ensures
            | Ensures _ ->
                get_fn_list_of_block_list t context requires (ensures @ [c]))
        | Function (_, kind, args, _stmts) ->
            (extend_verifiable_function_block h context requires ensures) @
            get_fn_list_of_block_list t (context @ kind :: args) [] []
        | _ -> [])
    | _ -> []

(* Functions are in the form of (call; arguments) :: () *)
and extend_verifiable_function_block (fn : jast) (context : jvar list) (requires : clause list) (ensures : clause list) : (verifiable_function list) = 
    match fn with
    | Function (name, kind, args, stmts) ->
        {requires = requires; ensures = ensures; context = (context @ kind :: args); title = name; fn = fn} ::
        (get_fn_list_of_block_list stmts (context @ kind :: args) [] [])
    | _ -> failwith "Veritas: Error: not of function in extend_verifiable_function_block"

(* Steps through all branches, maintaining a context list of accessible information. *)
(* Returns a list of all functions, including functions within functions. *)
(* For each branch of the AST, we test if it is a function, *)
and get_fn_list_of_program_aux (ast' : jast) (context : jvar list) (requires : clause list) (ensures : clause list) : (verifiable_function list) = 
    match ast' with
    | Toplevel stmts ->
        get_fn_list_of_block_list stmts context requires ensures
    | _ -> []

let get_fn_list_of_program (ast' : jast) : verifiable_function list = get_fn_list_of_program_aux ast' [] [] []
