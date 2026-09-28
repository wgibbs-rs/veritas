type expr =
    | Int of int
    | Ident of string
    | String of string
    | Add of expr * expr
    | Subtract of expr * expr
    | Multiply of expr * expr
    | Divide of expr * expr
    | EQ of expr * expr
    | NEQ of expr * expr
    | LT of expr * expr
    | LE of expr * expr
    | GT of expr * expr
    | GE of expr * expr

type arg = 
    | Typeless of string

type clause = 
    | Requires of expr
    | Ensures of expr

type statement =
    | Assignment of string * expr
    | Function of string * arg list * statement list
    | Return of expr
    | Clause of clause (* ACSL clauses *)
    | WS

type program = statement list

let validate_syntax filename =
    let validator = {|
      try
        include(expr -> (Meta.isexpr(expr, :error) || Meta.isexpr(expr, :incomplete)) ? expr : nothing, ARGS[1])
        exit(0)
      catch e
        println(stdout, "Error while parsing input file.")
        println(stdout)
        showerror(stdout, e, catch_backtrace())
        println(stdout)
        flush(stdout)
        exit(1)
      end
    |} in
    let ic =
      Unix.open_process_args_in
        "julia"
        [| "julia"; "-e"; validator; filename |]
    in
    let output = In_channel.input_all ic in
    let result = Unix.close_process_in ic in
    if result <> Unix.WEXITED 0 then
      print_string output

let rec print_expr indent = function
    | Int n ->
        Printf.printf "%s%d\n" indent n
    | Ident name ->
        Printf.printf "%s%s\n" indent name
    | String name ->
        Printf.printf "%s%s\n" indent name
    | Add (lhs, rhs) ->
        Printf.printf "%s+\n" indent;
        print_expr (indent ^ "  ") lhs;
        print_expr (indent ^ "  ") rhs
    | Subtract (lhs, rhs) ->
        Printf.printf "%s-\n" indent;
        print_expr (indent ^ "  ") lhs;
        print_expr (indent ^ "  ") rhs
    | Multiply (lhs, rhs) ->
        Printf.printf "%s*\n" indent;
        print_expr (indent ^ "  ") lhs;
        print_expr (indent ^ "  ") rhs
    | Divide (lhs, rhs) ->
        Printf.printf "%s/\n" indent;
        print_expr (indent ^ "  ") lhs;
        print_expr (indent ^ "  ") rhs
    | EQ (lhs, rhs) -> 
        Printf.printf "%s==\n" indent;
        (print_expr (indent ^ "  ") lhs);
        (print_expr (indent ^ "  ") rhs)
    | NEQ (lhs, rhs) -> 
        Printf.printf "%s!=\n" indent;
        (print_expr (indent ^ "  ") lhs);
        (print_expr (indent ^ "  ") rhs)
    | LT (lhs, rhs) -> 
        Printf.printf "%s<\n" indent;
        (print_expr (indent ^ "  ") lhs);
        (print_expr (indent ^ "  ") rhs)
    | LE (lhs, rhs) -> 
        Printf.printf "%s<=\n" indent;
        (print_expr (indent ^ "  ") lhs);
        (print_expr (indent ^ "  ") rhs)
    | GT (lhs, rhs) -> 
        Printf.printf "%s>\n" indent;
        (print_expr (indent ^ "  ") lhs);
        (print_expr (indent ^ "  ") rhs)
    | GE (lhs, rhs) -> 
        Printf.printf "%s>=\n" indent;
        (print_expr (indent ^ "  ") lhs);
        (print_expr (indent ^ "  ") rhs)

let print_arg indent = function
    | Typeless (name) ->
        Printf.printf "%sArg" indent;
        Printf.printf "%s%s : Any\n" (indent ^ "  ") name

let print_clause indent = function
    | Requires expr ->
        Printf.printf "%sRequires\n" indent;
        print_expr indent expr
    | Ensures expr ->
        Printf.printf "%sEnsures\n" indent;
        print_expr indent expr

let rec print_statement indent = function
    | Assignment (name, expr) ->
        Printf.printf "%s%s := \n" indent name;
        print_expr (indent ^ "  ") expr
    | Function (name, args, statements) ->
        Printf.printf "%sFunction %s\n" indent name;
        (match args with
        | [] ->
            Printf.printf "%s[]\n" (indent ^ "  ")
        | args' ->
            List.iter
                (print_arg (indent ^ "  "))
                args');
        Printf.printf "%sStatements\n" (indent ^ "  ");
        List.iter
            (print_statement (indent ^ "    "))
            statements;
    | Return expr ->
        Printf.printf "%sReturn\n" indent;
        print_expr (indent ^ "  ") expr
    | Clause clause ->
        Printf.printf "%sACSL Clause\n" indent;
        print_clause (indent ^ "  ") clause
    | WS ->
        Printf.printf ""

let print_statements indent = function
    | [] ->
        Printf.printf "No Statements.\n"
    | statements ->
        Printf.printf "Statement List\n";
        List.iter
            (print_statement (indent ^ "  "))
            statements

let print_ast = function
    | [] ->
        Printf.printf "Program\n"
    | statements ->
        Printf.printf "Program\n";
        List.iter
            (print_statement "  ")
            statements