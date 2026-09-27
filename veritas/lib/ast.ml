type expr =
  | Int of int
  | Ident of string
  | Add of expr * expr

type statement =
  | Assignment of string * expr
  | NewLine

type program = statement list

let rec print_expr indent = function
  | Int n ->
      Printf.printf "%sInt(%d)\n" indent n

  | Ident name ->
      Printf.printf "%sIdent(%s)\n" indent name

  | Add (lhs, rhs) ->
      Printf.printf "%sAdd\n" indent;
      print_expr (indent ^ "  ") lhs;
      print_expr (indent ^ "  ") rhs

let print_statement indent = function
    | Assignment (name, expr) ->
        Printf.printf "%sAssignment(%s)\n" indent name;
        print_expr (indent ^ "  ") expr
    | NewLine ->
        Printf.printf ""

let print_ast = function
  | [] ->
      Printf.printf "Program\n"

  | statements ->
      Printf.printf "Program\n";
      List.iter
        (print_statement "  ")
        statements