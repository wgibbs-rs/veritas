type expr =
    | Int of int
    | Ident of string
    | String of string
    | Add of expr * expr
    | Subtract of expr * expr
    | Multiply of expr * expr
    | Divide of expr * expr

type arg = 
    | Typeless of string

type statement =
    | Assignment of string * expr
    | Function of string * arg list * statement list
    | Return of expr
    | WS

type program = statement list

let rec print_expr indent = function
    | Int n ->
        Printf.printf "%sInt(%d)\n" indent n
    | Ident name ->
        Printf.printf "%sIdent(%s)\n" indent name
    | String name ->
        Printf.printf "%sString(%s)\n" indent name
    | Add (lhs, rhs) ->
        Printf.printf "%sAdd\n" indent;
        print_expr (indent ^ "  ") lhs;
        print_expr (indent ^ "  ") rhs
    | Subtract (lhs, rhs) ->
        Printf.printf "%sSubtract\n" indent;
        print_expr (indent ^ "  ") lhs;
        print_expr (indent ^ "  ") rhs
    | Multiply (lhs, rhs) ->
        Printf.printf "%sMultiply\n" indent;
        print_expr (indent ^ "  ") lhs;
        print_expr (indent ^ "  ") rhs
    | Divide (lhs, rhs) ->
        Printf.printf "%sDivide\n" indent;
        print_expr (indent ^ "  ") lhs;
        print_expr (indent ^ "  ") rhs

let print_arg indent = function
    | Typeless (name) ->
        Printf.printf "Argument\n";
        Printf.printf "%sName(%s)\n" indent name;
        Printf.printf "Type(Any)\n"

let rec print_statement indent = function
    | Assignment (name, expr) ->
        Printf.printf "%sAssignment(%s)\n" indent name;
        print_expr (indent ^ "  ") expr
    | Function (name, args, statements) ->
        Printf.printf "%sFunction(%s)\n" indent name;
        (match args with
        | [] ->
            Printf.printf "%sNo Args\n" (indent ^ "  ")
        | args' ->
            List.iter
                (print_arg (indent ^ "  "))
                args');
        Printf.printf "%sStatements\n" (indent ^ "  ");
        List.iter
            (print_statement (indent ^ "    "))
            statements
    | Return expr ->
        Printf.printf "%sReturn\n" indent;
        print_expr (indent ^ "  ") expr
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