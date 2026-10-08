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

open Cli

let () =
    let state : config = parse_arguments in

    Ast.validate_syntax state.filename;

    let ast = Ast.get_ast state.filename in

    let func_ctx_list : Ast.verifiable_function list = Ast.get_fn_list_of_program ast in

    if state.debug then begin
        List.iter Printer.print_verifiable_function func_ctx_list;
        print_endline "\n----- WEAKEST PRECONDITION STEPS -----";
    end;

    let wp_list : Propositions.prop list list =
        List.map (fun x -> Wp.generate_weakest_preconditions x state.debug) func_ctx_list in
    
    let wp_list_negated = 
        List.map (fun x -> (List.map (Propositions.negate_prop) x)) wp_list in

    if state.debug = true then begin
        print_endline "\n----- VERIFICATION CONDITIONS -----";
        List.iter (fun x -> 
            List.iter (
                fun y -> print_endline (Propositions.prop_to_string y)) x ) wp_list
    end;

    let context =
        List.concat_map (fun (vf : Ast.verifiable_function) -> vf.context) func_ctx_list
        |> List.sort_uniq compare in

    let vf_names = List.map (fun (vf : Ast.verifiable_function) -> vf.title) func_ctx_list in

    let smtlib_sections : string list = 
        List.map2 (fun (i : Propositions.prop list) (j : string) -> 
            Smtlib.smt2_section 
                (String.concat "" (List.map (fun a -> Smtlib.prop_to_smtlib a context) i)) j
        )
        wp_list_negated
        vf_names in

    let smtlib_sections_combined = String.concat "" smtlib_sections in

    let g_smt2 = Smtlib.smt2_file ((Smtlib.jvar_to_smtlib context) ^ smtlib_sections_combined) in

    if state.debug = true then begin
        print_endline "\n----- SMT-LIB GENERATION -----\n";
        print_endline g_smt2;
    end;

    let z3_outputs = 
        String.split_on_char '\n' (Smtlib.check_smtlib2_string g_smt2)
        |> List.filter (fun s -> s <> "") in

    let result : string list = List.map ( fun x ->
        match x with
        | "sat" -> "UNSAT"
        | "unsat" -> "SAT"
        | _ -> "ERR"
    ) z3_outputs in

    print_endline "\n----- VERITAS RESULTS -----";

    List.iter2 (fun (x : string) (y : string) ->
        Printf.printf "%s: %s\n" x y) vf_names result;

    print_endline ""