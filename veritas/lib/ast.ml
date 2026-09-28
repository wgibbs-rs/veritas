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
    | Expr of string * ast list
    | Symbol of string
    | String of string
    | Int of int
    | Float of float
    | Bool of bool
    | Null
    
let rec ast_of_json (json : Yojson.Safe.t) =
    match json with
    | `Assoc fields ->
        let head =
            match List.assoc_opt "head" fields with
            | Some (`String s) -> s
            | _ -> failwith "Expected string field 'head'"
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
    | `Null -> Null
    | _ -> failwith "Unexpected JSON value"

let validate_syntax filename =
    let ic =
        Unix.open_process_args_in
        "julia"
        [| "julia"; "-e"; Bridge.validate_src; filename |]
    in
    let output = In_channel.input_all ic in
    let result = Unix.close_process_in ic in
    if result <> Unix.WEXITED 0 then
        print_string output

let get_json_ast filename =
    let ic =
    Unix.open_process_args_in
        "julia"
        [| "julia"; "-e"; Bridge.parser_src; filename; "n" |]
    in
    let output = In_channel.input_all ic in
    let _ = Unix.close_process_in ic in
    output

let print_ast filename = 
    let ic =
        Unix.open_process_args_in
        "julia"
        [| "julia"; "-e"; Bridge.parser_src; filename; "y" |]
    in
    let output = In_channel.input_all ic in
    let _ = Unix.close_process_in ic in
    print_string output

let get_ast filename =
    let json_str : string = get_json_ast filename in
    ast_of_json (Yojson.Safe.from_string json_str)
