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

type ast =
    | Expr of ast * ast list
    | Symbol of string
    | String of string
    | Int of int
    | Float of float
    | Boolean of bool

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
        Expr (head, args)

    | `String s -> Symbol s
    | `Int i -> Int i
    | `Float f -> Float f
    | `Bool b -> Boolean b
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

let print_ast filename = 
    let ic = Unix.open_process_args_in
        "julia" [| "julia"; "-e"; Bridge.parser_src; filename; "y" |] in
    let output = In_channel.input_all ic in
    let _ = Unix.close_process_in ic in
    print_string output

let get_ast filename =
    let json_str : string = get_json_ast filename in
    ast_of_json (Yojson.Safe.from_string json_str)

open Propositions
type clause =
    | Requires of prop
    | Ensures of prop

type verifiable_function = {
    requires: clause list; (* List of all ACSL requires clauses. *)
    ensures: clause list; (* List of all ACSL ensures clauses. *)
    context: ast list; (* A list of context accessible outside of this function. *)
    title: string;
    fn: ast; (* (head, args)-containing function Expr. *)
}

let rec print_ast_node_aux (ast : ast) (indent : string) =
    match ast with
    | Expr (head, args) ->
        Printf.printf "%sExpr:\n" indent;
        Printf.printf "%shead:\n" (indent ^ "  ");
        print_ast_node_aux (head) (indent ^ "    ");
        Printf.printf "%sargs:\n" (indent ^ "  ");
        List.iter (fun x -> print_ast_node_aux x (indent ^ "    ")) args
    | Symbol s ->
        Printf.printf "%sSymbol %s\n" indent s
    | String s ->
        Printf.printf "%sString %s\n" indent s
    | Int i ->
        Printf.printf "%sInt %d\n" indent i
    | Float f ->
        Printf.printf "%sFloat %f\n" indent f
    | Boolean b ->
        Printf.printf "%sBool %b\n" indent b

let print_ast_node (ast : ast) = print_ast_node_aux ast ""

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
    print_ast_node f.fn


let rec ast_to_expr (a : ast) : expr =
    match a with
    | Expr (Symbol "call", [name; lhs; rhs]) ->
        (match name with
        | Symbol "+" -> Propositions.Add (ast_to_expr lhs, ast_to_expr rhs)
        | Symbol "-" -> Propositions.Subtract (ast_to_expr lhs, ast_to_expr rhs)
        | Symbol "*" -> Propositions.Multiply (ast_to_expr lhs, ast_to_expr rhs)
        | Symbol "/" -> Propositions.Divide (ast_to_expr lhs, ast_to_expr rhs)
        | _ -> failwith "unknown call")
    | Expr (Symbol "call", [Symbol "__OLD__"; x]) -> Propositions.Old (ast_to_expr x)
    | Symbol "__RESULT__" -> Propositions.Result
    | Symbol s -> Propositions.Ident s
    | String s -> Propositions.String s
    | Int i -> Propositions.Int i
    | Float f -> Propositions.Float f
    | Boolean b ->  Propositions.Boolean b
    | _ -> 
        print_ast_node a;
        failwith "unknown Expr.head that is NOT in the form Symbol \"Call\"."
    
let ast_to_prop (a : ast) : prop =
    match a with
    | Expr (head, args) ->
        (match head, args with
        | Symbol "call", [op; lhs; rhs] ->
            (match op with
            | Symbol "==" -> Propositions.EQ (ast_to_expr lhs, ast_to_expr rhs)
            | Symbol "!=" -> Propositions.NEQ (ast_to_expr lhs, ast_to_expr rhs)
            | Symbol "<" -> Propositions.LT (ast_to_expr lhs, ast_to_expr rhs)
            | Symbol "<=" -> Propositions.LE (ast_to_expr lhs, ast_to_expr rhs)
            | Symbol ">" -> Propositions.GT (ast_to_expr lhs, ast_to_expr rhs)
            | Symbol ">=" -> Propositions.GE (ast_to_expr lhs, ast_to_expr rhs)
            | _ -> failwith "bad operation structure")
        | _ -> failwith "not a call")
    | _ -> failwith "no ast"


(* Returns the highest-level IFF statement *)
let rec parse_symbol (s : string) (ast_list : ast list) (lhs : ast list) : prop option =
    match ast_list with
    | (Symbol s') :: t -> 
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
        else parse_symbol s t (lhs @ [Symbol s'])
    | h :: t -> parse_symbol s t (lhs @ [h])
    | _ -> None

and parse_acsl_expr (prop_expr_list : ast list) : prop =
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
                let rec print_ast_list (ast_list : ast list) =
                    match ast_list with
                    | h' :: t :: [] ->
                        print_ast_node h';
                        print_ast_node t
                    | h' :: t -> 
                        print_ast_node h';
                        print_ast_list t
                    | [] -> 
                        print_endline "N/A" in
                print_ast_list h;
                failwith "Single AST Node ?"

let build_clause (component_list : ast list) : clause =
    match component_list with
    | Symbol "requires" :: prop_expr_list ->
        Requires (parse_acsl_expr prop_expr_list)
    | Symbol "ensures" :: prop_expr_list ->
        Ensures (parse_acsl_expr prop_expr_list)
    | _ -> failwith "malformed clause structure"

let rec get_fn_list_of_block_list (stmt_list : ast list) (context : ast list) (requires : clause list) (ensures : clause list) : (verifiable_function list) =
    match stmt_list with
    | h :: t ->
        (match h with
        | Expr (head, args) ->
            (match head with
            | Symbol "macrocall" ->
                (match args with
                | (Symbol "@ACSL") :: clause_expr_list -> 
                    (let new_clause = build_clause clause_expr_list in
                    match new_clause with
                    | Requires _ ->
                        get_fn_list_of_block_list t context (requires @ [new_clause]) ensures
                    | Ensures _ ->
                        get_fn_list_of_block_list t context requires (ensures @ [new_clause]))
                | _ -> failwith "Veritas: Error: macros are not supported.")
            | Symbol "function" ->
                (extend_verifiable_function_block h context requires ensures) @
                get_fn_list_of_block_list t (context @ [h]) [] []
            | _ -> [])
        | _ -> [])
    | [] -> []

(* Functions are in the form of (call; arguments) :: () *)
and extend_verifiable_function_block (fn : ast) (context : ast list) (requires : clause list) (ensures : clause list) : (verifiable_function list) = 
    match fn with
    | Expr (_, fn_args) ->
        (match fn_args with
        | call :: block :: [] ->
            (match call, block with
            | Expr (call_head, call_args), Expr (block_head, block_args) -> 
                (match call_head, block_head with
                | Symbol "call", Symbol "block" -> 
                    (match call_args with
                    | Symbol title :: _ ->
                        {requires = requires; ensures = ensures; context = (context @ call_args); title = title; fn = fn} ::
                        (get_fn_list_of_block_list block_args (context @ call_args) [] [])
                    | _ -> failwith "Veritas: Error: no function title found.")
                | _ -> failwith "Veritas: Error: unknown function call/block name")
            | _ -> failwith "Veritas: Error: unknown function call/block type")
        | _ -> failwith "Veritas: Error: unknown function structure")
    | _ -> failwith "Veritas: Error: unknown function structure"

(* Steps through all branches, maintaining a context list of accessible information. *)
(* Returns a list of all functions, including functions within functions. *)
(* For each branch of the AST, we test if it is a function, *)
and get_fn_list_of_program_aux (ast' : ast) (context : ast list) (requires : clause list) (ensures : clause list) : (verifiable_function list) = 
    match ast' with
    | Expr (head, args) ->
        (match head with
        | Symbol "toplevel" ->
            get_fn_list_of_block_list args context requires ensures
        | Symbol t -> failwith (Printf.sprintf "Veritas: Error: unexpected symbol %s" t)
        | _ -> failwith "Veritas: Error: unknown toplevel ast type.")
    | _ -> []

let get_fn_list_of_program (ast' : ast) : verifiable_function list = get_fn_list_of_program_aux ast' [] [] []
