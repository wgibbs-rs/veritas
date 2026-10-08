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

type config = {
    filename: string;
    debug: bool;
    wp_out: string;
    smt_out: string;
}

let default_config : config = {
    filename=Sys.argv.(1);
    debug=false;
    wp_out="";
    smt_out=""
}

let help_message : string = {|
This is Veritas 0.1.0

Usage:
    Veritas [FILE] [OPTIONS]

Options:
    -h, --help                        Print this help text and exit the program
    --version                         Print the version and exit the program
    --debug                           Print debug messages for internal processes
    -wp-out FILE                      Output the generated verification conditions to a file
    -smt-out FILE                     Output the generated SMT-LIB contract to a file

|}

let version_message : string = "Veritas (Version 0.1.0)"

let rec parse_arguments_aux (args : string list) (acc : config) : config =
    match args with
    | [] -> acc
    | ["-h"]
    | ["--help"] -> 
        print_string help_message; exit 0
    | ["--version"] -> 
        print_endline version_message; exit 0
    | "--debug" :: t -> 
        parse_arguments_aux t { acc with debug=true }
    | "-wp-out" :: o :: t ->
        parse_arguments_aux t { acc with wp_out=o }
    | "-smt-out" :: o :: t -> 
        parse_arguments_aux t { acc with smt_out=o }
    | h :: _ -> 
        print_endline ("unknown argument \"" ^ h ^ "\""); exit 1

let parse_arguments : config =
    if Array.length Sys.argv < 2 then begin
        Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0); exit 1
    end;
    if String.starts_with ~prefix:"-" Sys.argv.(1) 
    then parse_arguments_aux [Sys.argv.(1)] default_config
    else match (Array.to_list Sys.argv) with
    | _ :: _ :: args -> parse_arguments_aux args default_config
    | _ -> failwith "unknown argument structure"
