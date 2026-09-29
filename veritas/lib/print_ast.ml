open Ast

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
    | Null ->
        Printf.printf "%sNull\n" indent

let print_ast_node (ast : ast) = print_ast_node_aux ast ""

let print_verifiable_function (f : verifiable_function) = print_ast_node f.fn