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

let () =
    if Array.length Sys.argv <> 2 then begin
        Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0);
        exit 1
    end;

    Ast.validate_syntax Sys.argv.(1);

    let ast = Ast.get_ast Sys.argv.(1) in

    (* Search for and create a list of all functions with conditions,
    and their context they have access to.For now, we will assume only 
    functions can be verified in Julia, given the nature of arguments. *)
    let func_ctx_list : Ast.verifiable_function list = Ast.get_fn_list_of_program ast in
    List.iter Ast.print_verifiable_function func_ctx_list;

    print_endline "\n----- VERIFICATION STEPS -----";

    let wp_list : Propositions.prop list list =
        List.map (Wp.generate_weakest_preconditions) func_ctx_list in

    print_endline "\n----- VERIFICATION CONDITIONS -----";

    List.iter 
        (fun x -> List.iter (fun y -> 
            print_endline (Propositions.prop_to_string y)) x) wp_list;

    print_endline "\n----- SMT-LIB GENERATION -----\n";

    let wp_list_negated = List.map (fun x -> (List.map Propositions.negate_prop x)) wp_list in

    let context =
        List.concat (List.map (fun (vf : Ast.verifiable_function) -> vf.context) func_ctx_list)
        |> List.sort_uniq compare in

    let defined_consts : string = Smtlib.jvar_to_smtlib context in

    let smtlib_sections : string list = 
        List.map
        (fun (i : Propositions.prop list) -> 
            Smtlib.smt2_section (String.concat "" (List.map (fun a -> Smtlib.prop_to_smtlib a context) i))
        )
        wp_list_negated in
    
    let smtlib_sections_combined = String.concat "" smtlib_sections in

    let input = Smtlib.smt2_file (defined_consts ^ smtlib_sections_combined) in

    print_endline input;
    
    let result_flipped = 
        String.split_on_char '\n' (Smtlib.check_smtlib2_string input)
        |> List.filter (fun s -> s <> "") in

    let result = List.map ( fun x ->
        match x with
        | "sat" -> "UNSAT"
        | "unsat" -> "SAT"
        | _ -> "ERR"
    ) result_flipped in

    let vf_names = List.map (fun (vf : Ast.verifiable_function) -> vf.title) func_ctx_list in

    List.iter2 (fun (x : string) (y : string) ->
        Printf.printf "%s: %s\n" x y) vf_names result