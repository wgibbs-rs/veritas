{

  type validator_result =
  | Success
  | Failure of string

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
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> Success
    | _ -> Failure output
    
  open Parser

  exception Lexing_error of string

  let keyword = function
    | "function"  -> FUNCTION
    | "if"        -> IF
    | "else"      -> ELSE
    | "end"       -> END
    | "while"     -> WHILE
    | "for"       -> FOR
    | "return"    -> RETURN
    | name        -> IDENT name
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let identifier = letter (letter | digit)*

rule token = parse
  (* Whitespace *)
  | [' ' '\t' '\r'] { token lexbuf }

  (* Integers *)
  | digit+ as n { INT (int_of_string n)}

  (* Identifiers and keywords *)
  | identifier as name { keyword name }

  (* Strings *)
  | '\'' ([^ '\''])* '\'' { STRING (Lexing.lexeme lexbuf) }
  | '"' ([^ '"'])* '"' { STRING (Lexing.lexeme lexbuf) }

  (* Newline *)
  | '\n' { NEWLINE }

  (* Arithmetic operators *)
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { FSLASH }
  | '%' { PERCENT }

  (* Comparison operators *)
  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LE }
  | ">=" { GE }
  | '<' { LT }
  | '>' { GT }

  (* Boolean operators *)
  | "&&" { AND }
  | "||" { OR }
  | '!' { NOT }

  (* Assignment *)
  | '=' { ASSIGN }

  (* Utility *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | '.' { DOT }

  (* End of input *)
  | eof { EOF }

  (* Error *)
  | _ { raise (Lexing_error "Unexpected character") }