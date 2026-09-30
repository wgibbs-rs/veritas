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
    | Bool of bool

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
    | `Bool b -> Bool b
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
    | Bool b ->
        Printf.printf "%sBool %b\n" indent b

let print_ast_node (ast : ast) = print_ast_node_aux ast ""

let print_verifiable_function (f : verifiable_function) = 
    Printf.printf "\n\n=== %s ===\n" f.title;
    List.iter (fun x -> 
        Printf.printf "Requires:\n"; 
        match x with 
        | (Requires y) -> print_proposition y 
        | _ -> print_endline "  error reading requires value.") 
        f.requires;
    List.iter (fun x -> 
        Printf.printf "Ensures:\n"; 
        match x with 
        | (Ensures y) -> print_proposition y 
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
    | Symbol s -> Propositions.Ident s
    | String s -> Propositions.String s
    | Int i -> Propositions.Int i
    | Float f -> Propositions.Float f
    | Bool b ->  Propositions.Bool b
    | _ -> failwith "uh oh"
    
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

let rec get_fn_list_of_block_list (stmt_list : ast list) (context : ast list) (requires : clause list) (ensures : clause list) : (verifiable_function list) =
    (match stmt_list with
    | h :: t ->
        (match h with
        | Expr (head, args) ->
            (match head with
            | Symbol "macrocall" ->
                (match args with
                | (Symbol "@ACSL") :: (Symbol clause_kind) :: c :: [] -> 
                    (let new_clause = (match clause_kind with
                    | "requires" -> Requires (ast_to_prop c)
                    | "ensures" -> Ensures (ast_to_prop c)
                    | _ -> failwith (Printf.sprintf "Veritas: Error: unknown ACSL clause type \"%s\"" clause_kind)) in
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
    | [] -> [])

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
