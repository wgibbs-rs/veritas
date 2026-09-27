{
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
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | ':' { COLON }
  | '.' { DOT }

  (* End of input *)
  | eof { EOF }

  (* Error *)
  | _ { raise (Lexing_error "Unexpected character") }