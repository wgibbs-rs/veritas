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
            print_endline (Propositions.prop_to_string y)) x)
        wp_list;

    print_endline "\n----- SMT-LIB GENERATION -----\n";

    let defined_consts : string list =
        List.map 
        (fun (x : Ast.verifiable_function) -> Smtlib.jvar_to_smtlib x.context) 
        func_ctx_list in

    List.iter (fun x -> print_string x) defined_consts;
    
    let wp_list_negated = List.map (fun x -> (List.map Propositions.negate_prop x)) wp_list in

    let _smtlib_sections : string list list = 
        List.map2 
        (fun 
        (i : Propositions.prop list)
        (j : Ast.verifiable_function) -> 
            List.map (fun a ->
                print_endline (Smtlib.prop_to_smtlib a j.context);
                Smtlib.prop_to_smtlib a j.context
            ) i
        ) 
        (wp_list_negated)
        (func_ctx_list) in

    let input = "(exit)" in

    let ctx = Z3.mk_context [] in

    let solver = Z3.Solver.mk_solver ctx None in

    Z3.Solver.add solver (
        Z3.AST.ASTVector.to_expr_list 
        (Z3.SMT.parse_smtlib2_string ctx input [] [] [] [])
    );

    match Z3.Solver.check solver [] with
    | Z3.Solver.SATISFIABLE ->
        let _model = Z3.Solver.get_model solver in
        print_endline "SAT"
    | Z3.Solver.UNSATISFIABLE ->
        print_endline "UNSAT"
    | Z3.Solver.UNKNOWN ->
        print_endline "UNKNOWN"
